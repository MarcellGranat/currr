#' Run a map with the function, but saves after a given number of execution.
#'
#' @export

cp_map_lgl <- function(.x, .f, ..., name = NULL, wait = NULL, workers = NULL, fill = NULL, n_checkpoint = NULL) {

  if (is.null(name)) {
    message(crayon::blue(clisymbols::symbol$warning), " Using name is suggested.")

    name <- str_c(deparse(substitute(.f)), deparse(substitute(.x))) |>
      str_remove_all("\\W") |>
      str_to_lower() |>
      str_flatten("")
  }

  get_fun <- function(x) {
    as.numeric(ifelse(is.null(x), as.logical(NA), x))
  }

  get_fun <- safely(get_fun, as.logical(NA), quiet = TRUE)

  cp_map(.x = .x, .f = .f, ... = ..., name = name, wait = wait, workers = workers, fill = fill, n_checkpoint = n_checkpoint) |>
    map_dbl(~ get_fun(.)[[1]])
}
