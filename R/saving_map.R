#' Run a map with the function, but saves after a given number of execution.
#'
#' @export

saving_map <- function(.ids =  NULL, .f, ..., name = NULL, .board = NULL,
                       n_checkpoint = NULL, notification = NULL) {

  if (is.null(n_checkpoint)) {
    n_checkpoint <- 100
  }

  x <- read_rds(paste0(".currr.data/", name, "/x.rds"))[.ids]

  n_checkpoint <- min(n_checkpoint, length(x))

  q <- as.numeric(floor(quantile(seq_along(x), probs = 1:n_checkpoint / n_checkpoint)))

  for (i in seq(n_checkpoint)) {

    current_eval_ids <- .ids[seq(from = c(0, q)[i] + 1, to = q[i])]

    saveRDS(Sys.time(), file = paste0(".currr.data/", name, "/st_", str_flatten(current_eval_ids, "_"), ".rds"))

    out <- map(.x = x[current_eval_ids], .f, ...)

    saveRDS(Sys.time(), file = paste0(".currr.data/", name, "/et_", str_flatten(current_eval_ids, "_"), ".rds"))

    saveRDS(out, file = paste0(".currr.data/", name, "/out_", str_flatten(current_eval_ids, "_"), ".rds"))

    eta(name) |>
      (\(x) update_status(name = name, done = x$done, n = x$n, eta = x$eta)) ()

  }

}
