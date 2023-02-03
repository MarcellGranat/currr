update_status <- function(done = 50, cache = 40, n = 230, chr_length = 80, eta = "120 sec") {
  cache_rate <- round(cache / n * chr_length)
  done_rate <- round(done / n * chr_length) - cache_rate
  remain_rate <- chr_length - (cache_rate + done_rate)

  flush.console()
  cat(crayon::magenta("|"),
      crayon::cyan(paste0(rep("█", cache_rate), collapse = "")),
      crayon::green(paste0(rep("█", done_rate), collapse = "")),
      crayon::white(paste0(rep("█", remain_rate), collapse = "")),
      crayon::magenta("| ETA: ", eta), sep = "")
  cat(" \r")
  flush.console()
}
