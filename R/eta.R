#' Calculate ETA
#'
#' @export

eta <- function(name) {

  finished <- list.files(paste0(".currr.data/", name)) |>
    keep(str_starts, "et_")

  start_time <- map(finished, ~ read_rds(paste0(".currr.data/", name, "/", (str_replace(., "et", "st")))))
  finish_time <- map(finished, ~ read_rds(paste0(".currr.data/", name, "/", .)))
  exec_time <- map2_dbl(start_time, finish_time, ~ as.numeric(.y - .x))

  exec_inds <- finished |>
    map(~ read_rds(paste0(".currr.data/", name, "/", str_replace(., "et", "id"))))

  n <- read_rds(paste0(".currr.data/", name, "/meta.rds"))$n

  lin_estimation <- mean(exec_time/ map_dbl(exec_inds, length))  * # avg time
    (n - length(reduce(exec_inds, c))) # inds left

  if (length(exec_time) < 5) {
    poly_estimation <- 0
  } else {

  poly_n <- min(length(exec_time), 3)

  estim_inds <- tibble(i = seq(n)) |>
    mutate(
      poly_2 = i ^ 2,
      poly_3 = i ^ 3
    )

  poly_estimation <- map2(exec_inds, exec_time / map_dbl(exec_inds, length), tidyr::crossing) |>
    map_dfr(purrr::set_names, "i", "time") |>
    left_join(estim_inds[, seq(poly_n)], by = "i") |>
    lm(formula = time ~ .) |>
    step(direction = "backward", steps = 3, trace = FALSE, k = 4) |>
    broom::augment(newdata = estim_inds) |>
    slice(- reduce(exec_inds, c)) |>
    pull(.fitted) |>
    sum()
  }

  if ((poly_estimation > lin_estimation * 1.3 | poly_estimation < lin_estimation * .7) & poly_estimation > 10) {
    eta <- paste(format_eta(poly_estimation), "(polynomial est.)")
  } else {
    eta <- format_eta(lin_estimation)
  }

  eta <- str_c(eta, str_flatten(rep(" ", 30 - str_length(eta)), ""))

  list(
    done = length(reduce(exec_inds, c)),
    n = n,
    eta = eta
  )

}
