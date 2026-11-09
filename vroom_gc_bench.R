# Compare the vroom CRAN-log task with and without a forced gc per file.
# Usage: Rscript vroom_gc_bench.R <gc|nogc> <rep>
# Appends one CSV row to stdout: mode,rep,elapsed,user,system,gc_total,peak_rss_mb,max_tmp_mb
library(vroom)
source("_functions.R")

args <- commandArgs(trailingOnly = TRUE)
mode <- match.arg(args[1], c("gc", "nogc"))
rep <- as.integer(args[2])

peak_rss_mb <- function() {
  status <- readLines("/proc/self/status")
  as.numeric(sub("^\\S+:\\s+(\\d+).*", "\\1", grep("^VmHWM:", status, value = TRUE))) / 1024
}
tmp_mb <- function() sum(file.size(list.files(tempdir(), full.names = TRUE)), na.rm = TRUE) / 1024^2

files <- sort(dir(target("cranlogs"), full.names = TRUE))
gc_total <- 0
max_tmp <- 0

dump_cache()
t <- system.time({
  for (file in files) {
    vroom_df <- vroom(file, progress = FALSE, col_types = "Dtdccccccd",
      col_names = c("date","time","size","r_version","r_arch","r_os","package","version","country","ip_id")
    )
    sample(vroom_df$country, 5000)
    max_tmp <- max(max_tmp, tmp_mb())
    if (mode == "gc") {
      gc_total <- gc_total + system.time({
        rm(vroom_df)
        invisible(gc())
      })[["elapsed"]]
    }
  }
})

cat(sprintf("%s,%d,%.3f,%.3f,%.3f,%.3f,%.0f,%.0f\n",
  mode, rep, t[["elapsed"]], t[["user.self"]], t[["sys.self"]],
  gc_total, peak_rss_mb(), max_tmp))
