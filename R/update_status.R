#' Update the status of the progress bar.
#'
#' @param done A number.
#' @param cache A number.
#' @param n A number.
#' @param chr_length A number.
#' @param eta A string
#' @examples
#' library(currr)
#' update_status(done = 50, cache = 40, n = 100, chr_length = 80, eta = "120 sec")
#' Sys.sleep(.1)
#' update_status(done = 55, cache = 40, n = 100, chr_length = 80, eta = "120 sec")
#' Sys.sleep(.1)
#' update_status(done = 60, cache = 40, n = 100, chr_length = 80, eta = "120 sec")
#'
#' @export

update_status <- function(name, done = 50, n = 230, chr_length = 80, eta = "120 sec") {
  cache <- read_rds(paste0(".currr.data/", name, "/meta.rds"))$cache
  cache_rate <- round(cache / n * chr_length)
  done_rate <- round(done / n * chr_length)
  remain_rate <- chr_length - (cache_rate + done_rate)

  flush.console()
  cat(crayon::magenta("|"),
      crayon::cyan(paste0(rep("█", max(cache_rate, 0)), collapse = "")),
      crayon::green(paste0(rep("█", max(done_rate, 0)), collapse = "")),
      crayon::white(paste0(rep("█", max(remain_rate, 0)), collapse = "")),
      crayon::magenta("| ETA: ", eta), sep = "")
  cat(" \r")
  flush.console()
}
