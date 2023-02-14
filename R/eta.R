#' Calculate the ETA. Result is the input to `update_status`.
#' @keywords internal
#' @noRd
#'

eta <- function(name) {

  if (is.null(getOption("currr.folder"))) {
    currr_folder <- tempdir(check = TRUE)
  } else {
    currr_folder <- getOption("currr.folder")
  }

  finished <- list.files(paste0(currr_folder, "/", name)) |>
    purrr::keep(stringr::str_starts, "et_")

  start_time <- purrr::map(finished, ~ readr::read_rds(paste0(currr_folder, "/", name, "/", (stringr::str_replace(., "et", "st")))))
  finish_time <- purrr::map(finished, ~ readr::read_rds(paste0(currr_folder, "/", name, "/", .)))
  exec_time <- purrr::map2_dbl(start_time, finish_time, ~ as.numeric(.y - .x))

  exec_inds <- finished |>
    purrr::map(~ readr::read_rds(paste0(currr_folder, "/", name, "/", stringr::str_replace(., "et", "id"))))

  n <- readr::read_rds(paste0(currr_folder, "/", name, "/meta.rds"))$n

  lin_estimation <- mean(exec_time/ purrr::map_dbl(exec_inds, length))  * # avg time
    (n - length(purrr::reduce(exec_inds, c))) # inds left

  if (length(exec_time) < 5) {
    poly_estimation <- 0
  } else {

  poly_n <- min(length(exec_time), 3)

  estim_inds <- tibble::tibble(i = seq(n)) |>
    dplyr::mutate(
      poly_2 = i ^ 2,
      poly_3 = i ^ 3
    )

  poly_estimation <- purrr::map2(exec_inds, exec_time / purrr::map_dbl(exec_inds, length), tidyr::crossing) |>
    purrr::map_dfr(purrr::set_names, "i", "time") |>
    dplyr::left_join(estim_inds[, seq(poly_n)], by = "i") |>
    stats::lm(formula = time ~ .) |>
    stats::step(direction = "backward", steps = 3, trace = FALSE, k = 4) |>
    broom::augment(newdata = estim_inds) |>
    dplyr::slice(- purrr::reduce(exec_inds, c)) |>
    dplyr::pull(.fitted) |>
    sum()
  }

  if ((poly_estimation > lin_estimation * 1.3 | poly_estimation < lin_estimation * .7) & poly_estimation > 10) {
    eta <- paste(format_eta(poly_estimation), "(polynomial est.)")
  } else {
    eta <- format_eta(lin_estimation)
  }

  eta <- stringr::str_c(eta, stringr::str_flatten(rep(" ", 30 - stringr::str_length(eta)), ""))

  list(
    done = length(purrr::reduce(exec_inds, c)),
    n = n,
    eta = eta
  )

}
