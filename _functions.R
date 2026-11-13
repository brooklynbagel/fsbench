target_dir <- Sys.getenv("TARGET_DIR", "../fsbench.work")
target <- function(...) {
  file.path(target_dir, ...)
}

times <- list()

# construct a static 100MB dataframe
static_df_rows <- 0.0291268 * 100*1024*1024
static_df <- data.frame(x1 = runif(static_df_rows), x2 = runif(static_df_rows))

benchmark_begin <- function() {
  times <<- list()
}

benchmark <- function(name, task, parallelism = NA) {
  if (!is.na(parallelism)) {
    name <- sprintf("%s(%d)", name, parallelism)
  }
  message("
# ================================================
# Task: ", name, "
# ================================================
")
  purge_time <- system.time(dump_cache())[["elapsed"]]
  stats_before <- mountstats_snapshot()
  force(task)
  mountstats_record(name, stats_before, purge_time)
  if (!inherits(task, "proc_time")) {
    warning("Task '", name, "' returned wrong object type. Expected 'proc_time', got '", class(task)[1], "'")
  } else {
    times[[name]] <<- list(time=task, parallelism=parallelism)
  }
}

aggregate_benchmark <- function(name, iterations, task) {
  purge_time <- system.time(dump_cache())[["elapsed"]]
  stats_before <- mountstats_snapshot()

  # perform the timer on the entire set of iterations instead of timing each
  # iteration individually - we need to do this because the overhead of using
  # system.time is HUGE! calling this in a loop is disastrous for performance,
  # and thus causes very inaccurate benchmarks. as a result, each iteration
  # should strive to minimize calls unrelated to the actual filesystem being tested
  aggregate_time = system.time({
    for (i in 1:iterations) {
      task(i)
    }
  })

  mountstats_record(name, stats_before, purge_time)
  times[[name]] <<- list(time=aggregate_time, parallelism=NA)
}

benchmark_end <- function() {
  df <- do.call(rbind, unname(lapply(times, function(list) {
    proc_time <- list[[1]]
    parallelism <- as.integer(list[[2]])
    pt <- summary(proc_time)
    data.frame(user = pt[[1]], system = pt[[2]], elapsed = pt[[3]], parallelism = parallelism)
  })))
  # strip off the added parallelism in the name when creating the final data frame
  df <- cbind(data.frame(task = sub("(.*)\\(([\\d]+)\\)$", "\\1", names(times), perl=TRUE), stringsAsFactors = FALSE), df)
  df <- tibble::as_tibble(df)
  results_filename <- Sys.getenv("OUTPUT_FILE", "")
  if (!nzchar(results_filename)) {
    results_filename <- paste0("results-", format(Sys.time(), format = "%Y%m%d-%H%M%S"), ".csv")
    message("No OUTPUT_FILE env var; writing to ", results_filename)
  }
  write.csv(df, results_filename, row.names = FALSE)
  print(df)
}

time_install <- function(pkgname, type = "source", ...) {
  pkgs <- as.data.frame(available.packages(repos = "https://cloud.r-project.org"), stringsAsFactors = FALSE)
  # https://cran.r-project.org/src/contrib/BH_1.75.0-0.tar.gz
  pkgs <- pkgs[pkgs$Package == pkgname,]
  if (nrow(pkgs) == 0) {
    stop("Unknown package '", pkgname, "'")
  }
  stopifnot(nrow(pkgs) == 1)

  pkgfile <- target(paste0(pkgname, "_", pkgs$Version, ".tar.gz"))
  url <- file.path(pkgs$Repository, basename(pkgfile))

  download.file(url, pkgfile)
  on.exit(if (file.exists(pkgfile)) unlink(pkgfile))

  res <- system.time({
    utils::install.packages(pkgfile, repos = NULL, type = type, ...)
  })

  res
}

write_random_csv <- function(path, bytes) {
  rows <- 0.0291268 * bytes

  df <- data.frame(x1 = runif(rows), x2 = runif(rows))
  system.time({
    data.table::fwrite(df, file = path, row.names = FALSE, col.names = FALSE)
  })
}

write_static_csv <- function(path, num_files, num_iter) {
  num_rows <- as.integer(static_df_rows / num_files)
  start <- (num_iter - 1) * num_rows
  end <- start + num_rows
  sub_df <- static_df[start:end, ]
  data.table::fwrite(sub_df, file = path, row.names = FALSE, col.names = FALSE)
}

warn_once <- function(key, ...) {
  key <- paste0("fsbench.warned.", key)
  if (!isTRUE(getOption(key))) {
    warning(..., call. = FALSE)
    options(structure(list(TRUE), names = key))
  }
}

evict_bin <- function() {
  candidates <- c(
    Sys.getenv("FSBENCH_EVICT_BIN"),
    Sys.which("evict-disk-cache"),
    "/usr/local/bin/evict-disk-cache",
    file.path("tools", "evict-disk-cache")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates)) candidates[[1]] else ""
}

# FSBENCH_PURGE selects how the cache is cleared before each task:
# - "auto" (default): purge-disk-cache (drop_caches); if it's missing or fails
#   (e.g. unprivileged Kubernetes pods/Workbench sessions), fall back to
#   evict-disk-cache, a pod-scoped approximation (tools/evict-disk-cache.c).
# - "drop" / "evict": only that method; error if it's unavailable or fails.
# - "none": don't clear anything (for comparisons).
# FSBENCH_EVICT_ARGS passes extra flags to evict-disk-cache (e.g. "-n -D").
dump_cache <- function() {
  mode <- Sys.getenv("FSBENCH_PURGE", "auto")
  mode <- match.arg(mode, c("auto", "drop", "evict", "none"))
  sysname <- Sys.info()[["sysname"]]
  if (mode == "none") {
    warn_once("none", "FSBENCH_PURGE=none; disk cache is not cleared")
    return(invisible())
  }
  if (!sysname %in% c("Darwin", "Linux")) {
    warning("Clearing disk cache is not possible on this platform, benchmark results are unreliable!")
    return(invisible())
  }

  purge_bin <- "/usr/local/sbin/purge-disk-cache"
  if (mode == "drop" || (mode == "auto" && !isTRUE(getOption("fsbench.drop_failed")))) {
    if (file.exists(purge_bin)) {
      status <- system(purge_bin)
      if (status == 0) return(invisible())
      if (mode == "drop") stop("purge-disk-cache failed with exit status ", status)
      # Remember the failure so later tasks don't retry it every time
      options(fsbench.drop_failed = TRUE)
      message("purge-disk-cache failed (exit status ", status, "); trying evict-disk-cache")
    } else if (mode == "drop") {
      stop("FSBENCH_PURGE=drop but ", purge_bin, " is not installed")
    }
  }

  bin <- if (sysname == "Linux") evict_bin() else ""
  if (nzchar(bin)) {
    if (mode == "auto") {
      warn_once("evict", "Using evict-disk-cache: per-file/pod-scoped cache eviction, an approximation of a full disk-cache purge")
    }
    args <- strsplit(trimws(Sys.getenv("FSBENCH_EVICT_ARGS")), "\\s+")[[1]]
    status <- system2(bin, c(args, shQuote(target_dir)))
    if (status != 0) {
      if (mode == "evict") stop("evict-disk-cache failed with exit status ", status)
      warning("evict-disk-cache failed with exit status ", status)
    }
    return(invisible())
  }

  if (mode == "evict") stop("FSBENCH_PURGE=evict but evict-disk-cache was not found")
  warn_once("purge", "No working purge-disk-cache or evict-disk-cache; disk cache is not cleared, benchmark results may be optimistic!")
}

# NFS client counters ==========================================================
# If FSBENCH_MOUNTSTATS_FILE is set, append per-task deltas of the NFS
# per-op counts and byte counters for TARGET_DIR's mount (from
# /proc/self/mountstats) to that CSV. Counters are per NFS superblock, so they
# include other pods on the node that mount the same volume.

mountstats_mount <- function(lines, path) {
  dev <- grep("^device ", lines)
  mp <- sub("^device .* mounted on (\\S+) with fstype (\\S+).*$", "\\1", lines[dev])
  fs <- sub("^device .* mounted on (\\S+) with fstype (\\S+).*$", "\\2", lines[dev])
  ok <- startsWith(fs, "nfs") &
    (path == mp | startsWith(path, paste0(sub("/$", "", mp), "/")))
  if (!any(ok)) return(NULL)
  k <- which(ok)[which.max(nchar(mp[ok]))]
  end <- if (k < length(dev)) dev[k + 1] - 1 else length(lines)
  lines[(dev[k] + 1):end]
}

mountstats_snapshot <- function() {
  if (!nzchar(Sys.getenv("FSBENCH_MOUNTSTATS_FILE")) ||
      !file.exists("/proc/self/mountstats")) {
    return(NULL)
  }
  path <- normalizePath(target_dir, mustWork = FALSE)
  block <- mountstats_mount(readLines("/proc/self/mountstats"), path)
  if (is.null(block)) {
    warn_once("mountstats", "FSBENCH_MOUNTSTATS_FILE set but ", path, " is not on an NFS mount")
    return(NULL)
  }
  bytes_line <- grep("^\\s*bytes:", block, value = TRUE)[1]
  bytes <- as.numeric(strsplit(trimws(sub("^\\s*bytes:", "", bytes_line)), "\\s+")[[1]])
  bytes <- stats::setNames(bytes[1:6], c(
    "bytes_normal_read", "bytes_normal_write", "bytes_direct_read",
    "bytes_direct_write", "bytes_server_read", "bytes_server_write"
  ))
  ops_start <- grep("per-op statistics", block)
  ops <- block[seq_along(block) > ops_start]
  ops <- regmatches(ops, regexec("^\\s+([A-Z_0-9]+):\\s+(\\d+)", ops))
  ops <- Filter(length, ops)
  ops <- stats::setNames(
    vapply(ops, function(m) as.numeric(m[[3]]), numeric(1)),
    paste0("ops_", vapply(ops, `[[`, character(1), 2))
  )
  c(bytes, ops)
}

mountstats_record <- function(name, before, purge_time) {
  if (is.null(before)) return(invisible())
  after <- mountstats_snapshot()
  delta <- after[names(before)] - before
  delta <- delta[!is.na(delta) & delta != 0]
  out <- data.frame(
    task = name,
    metric = c("purge_elapsed", names(delta)),
    value = c(purge_time, unname(delta))
  )
  file <- Sys.getenv("FSBENCH_MOUNTSTATS_FILE")
  utils::write.table(out, file, sep = ",", row.names = FALSE,
    col.names = !file.exists(file), append = file.exists(file))
}
