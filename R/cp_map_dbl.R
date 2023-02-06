#' Run a map with the function, but saves after a given number of execution.
#'
#' @export

cp_map_dbl <- function(.x, .f, ..., name = NULL, cp_options = list()) {

  if (is.null(name)) {
    message(crayon::blue(clisymbols::symbol$warning), " Using name is suggested.")

    name <- str_c(deparse(substitute(.f)), deparse(substitute(.x))) |>
      str_remove_all("\\W") |>
      str_to_lower() |>
      str_flatten("")
  }

  get_fun <- function(x) {
      as.numeric(ifelse(is.null(x), as.numeric(NA), x))
  }

  get_fun <- safely(get_fun, as.numeric(NA), quiet = TRUE)

  cp_map(.x = .x, .f = .f, ... = ..., name = name, cp_options = cp_options) |>
    map_dbl(~ get_fun(.)[[1]])
}
