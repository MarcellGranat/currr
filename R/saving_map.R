saving_map <- function(.ids =  NULL, .f, ..., name = NULL, .board = NULL, n_checkpoint = NULL, notification = NULL) {

  if (is.null(n_checkpoint)) {
    n_checkpoint <- 100
  }

  .x <- read_rds(paste0(".curr.data/", name, "/x.rds"))[.ids]

  n_checkpoint <- min(n_checkpoint, length(.x))

  .f <- purrr::as_mapper(.f, ...)

  out <- list()

  for (i in seq(n_checkpoint)) {
    saveRDS(Sys.time(), file = "st_.rds")

    out <- map(.x = .x, .f = f, ...)

    saveRDS(Sys.time(), file = "et_.rds")

    saveRDS(out)

    update_status()

  }

}
