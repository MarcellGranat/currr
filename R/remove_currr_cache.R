#' Remove currr's intermediate data from the folder.
#'
#' @export

remove_currr_cache <- function(list = NULL) {
  if (is.null(list)) {
    unlink(".currr.data", recursive = TRUE)
  } else {
    list.dirs(".currr.data", full.names = FALSE) |>
    intersect(list) |>
      purrr::walk(~ unlink(paste0(".currr.data/", .), recursive = TRUE))
  }
}
