#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#ifdef __linux__
#include <sys/mount.h>
#endif

int error(int err) {
  perror("Error");
  return 1;
}

int main() {
  sync();
#ifdef __linux__
  int fd = open("/proc/sys/vm/drop_caches", O_WRONLY);
  if (-1 == fd && EROFS == errno) {
    // Containers mount /proc/sys read-only; remounting it needs CAP_SYS_ADMIN
    // (e.g. Kubernetes `capabilities: {add: ["SYS_ADMIN"]}`).
    if (-1 == mount(NULL, "/proc/sys", NULL,
                    MS_REMOUNT | MS_BIND | MS_NOSUID | MS_NODEV | MS_NOEXEC,
                    NULL)) {
      return error(errno);
    }
    fd = open("/proc/sys/vm/drop_caches", O_WRONLY);
  }
  if (-1 == fd) {
    return error(errno);
  }
  if (-1 == write(fd, "3\n", 2)) {
    return error(errno);
  }
#endif
#ifdef __APPLE__
  char* args[1];
  args[0] = 0;
  execv("/usr/sbin/purge", args);
#endif
}
