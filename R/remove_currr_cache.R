#' Remove currr's intermediate data from the folder.
#'
#'
#' @param list A character vector specifying the name of the caches you want to remove (files in .currr.data folder). If empy (default), all caches will be removed.
#' @return No return value, called for side effects
#' @export

remove_currr_cache <- function(list = NULL) {

  if (is.null(getOption("currr.folder"))) {
    currr_folder <- tempdir(check = TRUE)
  } else {
    currr_folder <- getOption("currr.folder")
  }

  if (is.null(list)) {
    unlink(currr_folder, recursive = TRUE)
  } else {
    list.dirs(currr_folder, full.names = FALSE) |>
    intersect(list) |>
      purrr::walk(~ unlink(paste0(currr_folder, "/", .), recursive = TRUE))
  }
}
