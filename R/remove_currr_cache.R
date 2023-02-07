#' Remove currr's intermediate data from the folder.
#'
#'
#' @param list A character vector specifying the name of the caches you want to remove (files in .currr.data folder). If empy (default), all caches will be removed.
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
