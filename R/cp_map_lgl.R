#' Wrapper function of `purrr::map`. Apply a function to each element of a vector, but save the intermediate data after a given number of iterations.
#'
#' @description
#' The map functions transform their input by applying a function to
#' each element of a list or atomic vector and returning an object of
#' the same length as the input. `cp_map` functions work exactly the
#' same way, but creates a secret folder in your current working directory
#' and saves the results if they reach a given checkpoint. This way
#' if you rerun the code, it reads the result from the cache folder
#' and start to evalutate where you finished.
#'
#' * `cp_map()` always returns a list.
#'
#' * `map_lgl()`, `map_dbl()` and `map_chr()` return an
#'   atomic vector of the indicated type (or die trying). For these functions,
#'   `.f` must return a length-1 vector of the appropriate type.
#'
#' @param .x A list or atomic vector.
#' @param .f A function, specified in one of the following ways:
#'
#'   * A named function, e.g. `mean`.
#'   * An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.
#'   * A formula, e.g. `~ .x + 1`. You must use `.x` to refer to the first
#'     argument. Only recommended if you require backward compatibility with
#'     older versions of R.
#'
#' @param ... Additional arguments passed on to the mapped function.
#'
#' @param name Name for the subfolder in the cache folder. If you do not specify,
#' then `cp_map` uses the name of the function combined with the name of x.
#' This is dangerous, since this generated name can appear multiple times in your code.
#' Also changing x will result a rerun of the code, however you max want to avoid this.
#' (if a subset of .x matches with the cached one and the function is the same,
#' then elements of this subset won't evaluated, rather read from the cache)
#'
#'
#' @param cp_options Options for the evaluation: `wait`, `n_checkpoint`, `workers`, `fill`.
#'
#' * `wait`: An integer to specify that after how many iterations the console shows the intermediate results (default `1`).
#' If its value is between 0 and 1, then it is taken as proportions of iterations to wait (example length of .x equals 100, then
#' you get back the result after 50 if you set it to 0.5). Set to `Inf` to get back the results only after full evaluations.
#' If its value is not equal to `Inf` then evaluation is goind in background job.
#'
#' * `n_chekpoint`: Number of checkpoints, when intermadiate results are saved (default = 100).
#'
#' * `workers`: Number of CPU cores to use (parallel package called in background). Set to 1 (default) to avoid parallel computing.
#'
#' * `fill()` When you get back a not fully evaluated result (default `TRUE`). Should the length of the result be the same as .x?
#'
#' You can set these options also with `options(currr.n_checkpoint = 200)`. Additional options: `currr.unchanged_message` (TRUE/FALSE), `currr.progress_length`
#' @return A logical vector.
#' @export
#' @family map variants
#' @examples
#' # Run them on console!
#' # (functions need writing and reading access to your working directory and they also print)
#'
#' avg_n <- function(.data, .col, x) {
#'   Sys.sleep(.01)
#'
#'   .data |>
#'     dplyr::pull({{ .col }}) |>
#'     (\(m) mean(m) * x) ()
#' }
#'
#'
#' cp_map(.x = 1:10, .f = avg_n, .data = iris, .col = Sepal.Length, name = "iris_mean")
#'
#'  # same function, read from cache
#' cp_map(.x = 1:10, .f = avg_n, .data = iris, .col = Sepal.Length, name = "iris_mean")
#'
#' remove_currr_cache()
#'

cp_map_lgl <- function(.x, .f, ..., name = NULL, cp_options = list()) {

  if (is.null(name)) {

    name <- stringr::str_c(deparse(substitute(.f)), deparse(substitute(.x))) |>
      stringr::str_remove_all("\\W") |>
      stringr::str_to_lower() |>
      stringr::str_flatten("")

    if (!name %in% list.files(".currr.data")) {
      message(crayon::blue(clisymbols::symbol$warning), " Using name is suggested. Currently named to ", crayon::cyan(name), ".")
    }
  }

  get_fun <- function(x) {
    if (length(x) > 1) {
      stop("The length if the returned list is not equal to 1.")
    }
    as.logical(ifelse(is.null(x), as.logical(NA), x))
  }

  get_fun <- purrr::safely(get_fun, as.logical(NA), quiet = TRUE)

  cp_map(.x = .x, .f = .f, ... = ..., name = name, cp_options = cp_options) |>
    purrr::map_lgl(~ get_fun(.)[[1]])
}
