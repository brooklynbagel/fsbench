# Compare the vroom CRAN-log task with and without a forced gc per file.
# Usage: Rscript vroom_gc_bench.R <gc|nogc|gcout> <rep>
#   nogc:  one timer around the loop, no forced gc
#   gc:    one timer around the loop, rm() + gc() after each file (inside timer)
#   gcout: per-file timers summed, rm() + gc() after each file outside the timer
#          (what main.R does)
# Appends one CSV row to stdout: mode,rep,elapsed,user,system,gc_total,peak_rss_mb,max_tmp_mb
library(vroom)
source("_functions.R")

args <- commandArgs(trailingOnly = TRUE)
mode <- match.arg(args[1], c("gc", "nogc", "gcout"))
rep <- as.integer(args[2])

peak_rss_mb <- function() {
  status <- readLines("/proc/self/status")
  as.numeric(sub("^\\S+:\\s+(\\d+).*", "\\1", grep("^VmHWM:", status, value = TRUE))) / 1024
}
tmp_mb <- function() sum(file.size(list.files(tempdir(), full.names = TRUE)), na.rm = TRUE) / 1024^2

files <- sort(dir(target("cranlogs"), full.names = TRUE))
gc_total <- 0
max_tmp <- 0

# The timed body is identical across modes (including the tmp_mb() probe) so
# the only difference is where, or whether, the gc happens.
read_one <- function(file) {
  vroom_df <- vroom(file, progress = FALSE, col_types = "Dtdccccccd",
    col_names = c("date","time","size","r_version","r_arch","r_os","package","version","country","ip_id")
  )
  sample(vroom_df$country, 5000)
  max_tmp <<- max(max_tmp, tmp_mb())
  vroom_df
}
force_gc <- function() {
  gc_total <<- gc_total + system.time(invisible(gc()))[["elapsed"]]
}

dump_cache()
if (mode == "gcout") {
  t <- NULL
  for (file in files) {
    file_time <- system.time(vroom_df <- read_one(file))
    t <- if (is.null(t)) file_time else t + file_time
    rm(vroom_df)
    force_gc()
  }
} else {
  t <- system.time({
    for (file in files) {
      vroom_df <- read_one(file)
      if (mode == "gc") {
        rm(vroom_df)
        force_gc()
      }
    }
  })
}

cat(sprintf("%s,%d,%.3f,%.3f,%.3f,%.3f,%.0f,%.0f\n",
  mode, rep, t[["elapsed"]], t[["user.self"]], t[["sys.self"]],
  gc_total, peak_rss_mb(), max_tmp))
