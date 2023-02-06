#' Run a map with the function, but saves after a given number of execution.
#'
#' @export

cp_map_dfc <- function(.x, .f, ..., name = NULL, wait = 1, workers = NULL, fill = TRUE, n_checkpoint = 100) {
  cp_map(.x = .x, .f = .f, ... = ..., name = name, wait = wait, workers = workers, fill = fill, n_checkpoint = n_checkpoint) |>
    map_dfc(~ .)
}
