// evict-disk-cache: unprivileged, pod-scoped approximation of
// `echo 3 > /proc/sys/vm/drop_caches`, for environments (e.g. Workbench
// sessions on Kubernetes) where purge-disk-cache can't work.
//
// Usage: evict-disk-cache [-n] [-D] [-r] [-q] [-m MiB] [DIR...]
//   DIR defaults to $TARGET_DIR. Steps, in order:
//   1. sync(), then posix_fadvise(POSIX_FADV_DONTNEED) on every regular file
//      under DIR (page cache). Residency before/after is measured with mincore.
//   2. -D disables: create+unlink a dummy entry in every directory under DIR,
//      after step 1, so the directory's change attribute moves and cached NFS
//      lookups (incl. those the walk itself just made) must be revalidated.
//   3. -n disables: memory squeeze. Allocate anonymous memory until the
//      cgroup's reclaimable file pages are gone or only a safety margin (-m,
//      default max(128 MiB, limit/32)) is left, then free it. Skipped if the
//      cgroup has no memory limit, isn't cgroup v2, or swap is available.
//      The kernel only reclaims once the cgroup is at its limit, so roughly
//      `margin` bytes of the coldest file cache survive: this is a backstop
//      behind step 1 (and a way to pressure memcg-charged dentries/inodes),
//      not a full drain. Never exceeding the limit matters because kubelet
//      may set memory.oom.group, which would kill the whole container.
//   -r only reports page-cache residency (no eviction). -q suppresses output.
//
// Needs no capabilities, no setuid, and only affects the caller's own files
// and memory cgroup. It does not clear the NFS server's cache (neither does
// drop_caches).

#define _GNU_SOURCE
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef __linux__
int main(void) {
  fprintf(stderr, "evict-disk-cache: only supported on Linux\n");
  return 2;
}
#else

#include <fcntl.h>
#include <ftw.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>

#define MiB (1024.0 * 1024.0)

static int opt_dirtouch = 1, opt_squeeze = 1, opt_report = 0, opt_quiet = 0;
static long long opt_margin_mib = -1;
static long page_size;

static struct {
  uint64_t files, file_errs, dirs, dir_errs;
  uint64_t bytes, resident_before, resident_after;
} st;

static double now(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec + ts.tv_nsec / 1e9;
}

// Bytes of the file currently in the page cache, or -1 on error.
static int64_t resident_bytes(int fd, off_t size) {
  if (size <= 0) return 0;
  void *p = mmap(NULL, size, PROT_READ, MAP_SHARED, fd, 0);
  if (p == MAP_FAILED) return -1;
  size_t pages = (size + page_size - 1) / page_size;
  unsigned char *vec = malloc(pages);
  int64_t n = -1;
  if (vec && mincore(p, size, vec) == 0) {
    n = 0;
    for (size_t i = 0; i < pages; i++) n += vec[i] & 1;
    n *= page_size;
  }
  free(vec);
  munmap(p, size);
  return n;
}

static int visit(const char *path, const struct stat *sb, int type,
                 struct FTW *ftw) {
  (void)ftw;
  if (type == FTW_F && S_ISREG(sb->st_mode)) {
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd == -1) { st.file_errs++; return 0; }
    st.files++;
    st.bytes += sb->st_size;
    int64_t r = resident_bytes(fd, sb->st_size);
    if (r > 0) st.resident_before += r;
    if (!opt_report) {
      if (posix_fadvise(fd, 0, 0, POSIX_FADV_DONTNEED) != 0) st.file_errs++;
      r = resident_bytes(fd, sb->st_size);
      if (r > 0) st.resident_after += r;
    }
    close(fd);
  } else if (type == FTW_DP && !opt_report && opt_dirtouch) {
    // FTW_DEPTH: directories are visited after their contents.
    char name[4096];
    int n = snprintf(name, sizeof name, "%s/.fsbench-evict-%ld", path,
                     (long)getpid());
    int fd = (n > 0 && (size_t)n < sizeof name)
               ? open(name, O_CREAT | O_EXCL | O_WRONLY | O_CLOEXEC, 0600)
               : -1;
    if (fd == -1) { st.dir_errs++; return 0; }
    close(fd);
    if (unlink(name) == 0) st.dirs++; else st.dir_errs++;
  } else if (type == FTW_DNR || type == FTW_NS) {
    st.file_errs++;
  }
  return 0;
}

// --- cgroup v2 memory squeeze ----------------------------------------------

#define CG "/sys/fs/cgroup/"

// Reads a single number (or "max" -> -1) from a cgroup file; -2 on error.
static int64_t read_cg_value(const char *file) {
  FILE *f = fopen(file, "r");
  if (!f) return -2;
  char buf[64] = {0};
  int64_t v = -2;
  if (fgets(buf, sizeof buf, f)) {
    if (strncmp(buf, "max", 3) == 0) v = -1;
    else v = strtoll(buf, NULL, 10);
  }
  fclose(f);
  return v;
}

typedef struct { int64_t file_lru, slab_reclaimable; } memstat_t;

static int read_memstat(memstat_t *m) {
  FILE *f = fopen(CG "memory.stat", "r");
  if (!f) return -1;
  char key[64];
  long long v;
  int64_t active = 0, inactive = 0;
  m->slab_reclaimable = 0;
  while (fscanf(f, "%63s %lld", key, &v) == 2) {
    if (!strcmp(key, "active_file")) active = v;
    else if (!strcmp(key, "inactive_file")) inactive = v;
    else if (!strcmp(key, "slab_reclaimable")) m->slab_reclaimable = v;
  }
  fclose(f);
  // active_file/inactive_file are the file LRUs: reclaimable without swap.
  // ("file" would also count shmem/tmpfs, which can't be reclaimed here.)
  m->file_lru = active + inactive;
  return 0;
}

static int swap_possible(void) {
  FILE *f = fopen("/proc/meminfo", "r");
  if (!f) return 1;
  char line[256];
  long long swap_kb = 0;
  while (fgets(line, sizeof line, f))
    if (sscanf(line, "SwapTotal: %lld kB", &swap_kb) == 1) break;
  fclose(f);
  if (swap_kb == 0) return 0;
  return read_cg_value(CG "memory.swap.max") != 0;
}

static void squeeze(void) {
  const char *skip = NULL;
  FILE *f = fopen("/proc/self/cgroup", "r");
  char line[512] = {0};
  if (!f || !fgets(line, sizeof line, f) || strncmp(line, "0::", 3) != 0)
    skip = "not cgroup v2";
  if (f) fclose(f);

  int64_t limit = read_cg_value(CG "memory.max");
  int64_t high = read_cg_value(CG "memory.high");
  if (high > 0 && (limit < 0 || high < limit)) limit = high;
  if (!skip && limit < 0) skip = "no memory limit";
  if (!skip && swap_possible()) skip = "swap available";
  memstat_t before;
  if (!skip && read_memstat(&before) != 0) skip = "memory.stat unreadable";
  if (skip) {
    if (!opt_quiet) fprintf(stderr, "evict-disk-cache: squeeze skipped (%s)\n", skip);
    return;
  }

  // If anything gets OOM-killed, make it this helper, not the caller.
  int fd = open("/proc/self/oom_score_adj", O_WRONLY | O_CLOEXEC);
  if (fd != -1) { (void)!write(fd, "1000\n", 5); close(fd); }

  int64_t margin = opt_margin_mib >= 0 ? opt_margin_mib * (int64_t)MiB
                                       : limit / 32;
  if (opt_margin_mib < 0 && margin < 128 * (int64_t)MiB) margin = 128 * (int64_t)MiB;
  const size_t chunk = 32 << 20;
  const int64_t floor_bytes = 8 << 20;
  size_t max_chunks = limit / chunk + 1, n = 0;
  void **chunks = calloc(max_chunks, sizeof *chunks);
  memstat_t m = before;
  const char *stop = "file LRU drained";

  while (chunks && n < max_chunks) {
    int64_t current = read_cg_value(CG "memory.current");
    if (current < 0 || read_memstat(&m) != 0) { stop = "read error"; break; }
    if (m.file_lru <= floor_bytes) break;
    if (current - m.file_lru + (int64_t)chunk + margin > limit) {
      stop = "margin reached";
      break;
    }
    void *p = mmap(NULL, chunk, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE, -1, 0);
    if (p == MAP_FAILED) { stop = "mmap failed"; break; }
    for (size_t off = 0; off < chunk; off += page_size) ((char *)p)[off] = 1;
    chunks[n++] = p;
  }
  read_memstat(&m);
  for (size_t i = 0; i < n; i++) munmap(chunks[i], chunk);
  free(chunks);

  if (!opt_quiet)
    fprintf(stderr,
            "evict-disk-cache: squeeze: file LRU %.0f -> %.0f MiB, "
            "slab_reclaimable %.0f -> %.0f MiB, peak alloc %.0f MiB, "
            "limit %.0f MiB, margin %.0f MiB (%s)\n",
            before.file_lru / MiB, m.file_lru / MiB,
            before.slab_reclaimable / MiB, m.slab_reclaimable / MiB,
            n * (double)chunk / MiB, limit / MiB, margin / MiB, stop);
}

int main(int argc, char **argv) {
  int c;
  while ((c = getopt(argc, argv, "nDrqm:")) != -1) {
    switch (c) {
      case 'n': opt_squeeze = 0; break;
      case 'D': opt_dirtouch = 0; break;
      case 'r': opt_report = 1; break;
      case 'q': opt_quiet = 1; break;
      case 'm': opt_margin_mib = atoll(optarg); break;
      default:
        fprintf(stderr, "usage: %s [-n] [-D] [-r] [-q] [-m MiB] [DIR...]\n", argv[0]);
        return 2;
    }
  }
  page_size = sysconf(_SC_PAGESIZE);

  char *dirs_default[1];
  char **dirs = argv + optind;
  int ndirs = argc - optind;
  if (ndirs == 0) {
    dirs_default[0] = getenv("TARGET_DIR");
    if (!dirs_default[0] || !*dirs_default[0]) {
      fprintf(stderr, "evict-disk-cache: no DIR given and TARGET_DIR unset\n");
      return 2;
    }
    dirs = dirs_default;
    ndirs = 1;
  }

  double t0 = now();
  if (!opt_report) sync();
  for (int i = 0; i < ndirs; i++) {
    if (nftw(dirs[i], visit, 64, FTW_PHYS | FTW_MOUNT | FTW_DEPTH) != 0) {
      perror(dirs[i]);
      return 1;
    }
  }
  double t1 = now();
  if (!opt_report && opt_squeeze) squeeze();
  double t2 = now();

  if (!opt_quiet) {
    if (opt_report)
      fprintf(stderr, "evict-disk-cache: %llu files, %.1f MiB, resident %.1f MiB\n",
              (unsigned long long)st.files, st.bytes / MiB, st.resident_before / MiB);
    else
      fprintf(stderr,
              "evict-disk-cache: %llu files (%.1f MiB), resident %.1f -> %.1f MiB, "
              "%llu dirs touched, %llu file / %llu dir errors, "
              "walk %.2f s, squeeze %.2f s\n",
              (unsigned long long)st.files, st.bytes / MiB,
              st.resident_before / MiB, st.resident_after / MiB,
              (unsigned long long)st.dirs, (unsigned long long)st.file_errs,
              (unsigned long long)st.dir_errs, t1 - t0, t2 - t1);
  }
  return 0;
}

#endif
