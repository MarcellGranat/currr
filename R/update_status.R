#' Progress bar and ETA.
#' @keywords internal
#' @noRd
#'

update_status <- function(name, done, n, eta) {

  if (is.null(getOption("currr.folder"))) {
    currr_folder <- tempdir(check = TRUE)
  } else {
    currr_folder <- getOption("currr.folder")
  }

  if (is.null(getOption("currr.progress_length"))) {
    chr_length <- 50 # default
  } else {
    chr_length <- getOption("currr.progress_length")
  }

  cache <- readr::read_rds(paste0(currr_folder, "/", name, "/meta.rds"))$cache
  cache_rate <- round(cache / n * chr_length)
  done_rate <- round((done - cache) / n * chr_length)
  done_rate <- min(done_rate, chr_length - cache_rate)
  remain_rate <- chr_length - (cache_rate + done_rate)

  utils::flush.console()
  cat(crayon::magenta("|"),
      crayon::cyan(paste0(rep("\u2588", max(cache_rate, 0)), collapse = "")),
      crayon::green(paste0(rep("\u2588", max(done_rate, 0)), collapse = "")),
      crayon::white(paste0(rep("\u2588", max(remain_rate, 0)), collapse = "")),
      crayon::magenta(" | "),
      crayon::green(scales::percent((done_rate + cache_rate) / chr_length)),
      crayon::magenta(" ETA: ", eta), sep = "")
  cat(" \r")
  utils::flush.console()
}
