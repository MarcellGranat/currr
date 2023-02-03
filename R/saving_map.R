saving_map <- function(.x, .f, ...,  workers = NULL, fill = "default", name = NULL, .board = NULL, n_checkpoint = NULL, notification = NULL) {

  if (is.null(n_checkpoint)) {
    n_checkpoint <- 100
  }

  n_checkpoint <- min(n_checkpoint, length(.x))

  .f <- purrr::as_mapper(.f, ...)

  out <- list()

  for (i in seq(n_checkpoint)) {
    tictoc::tic()

    out <- map(.x = .x, .f = f, ...)

    runtime <- tictoc::toc() |>
      capture.output() |>
      stringr::str_remove(" sec elapsed") |>
      as.numeric()
  }

}
