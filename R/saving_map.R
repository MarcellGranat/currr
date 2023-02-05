#' Run a map with the function, but saves after a given number of execution.
#'
#' @export

saving_map <- function(.ids, .f, name, n_checkpoint = 100, ...) {

  x <- read_rds(paste0(".currr.data/", name, "/x.rds"))[.ids]

  n_checkpoint <- min(n_checkpoint, length(x))

  q <- as.numeric(floor(quantile(seq_along(x), probs = 1:n_checkpoint / n_checkpoint)))

  for (i in seq(n_checkpoint)) {

    current_eval_ids <- .ids[seq(from = c(0, q)[i] + 1, to = q[i])]

    saveRDS(Sys.time(), file = paste0(".currr.data/", name, "/st_", first(current_eval_ids), ".rds"))

    out <- map(.x = x[current_eval_ids], .f, ...)

    saveRDS(Sys.time(), file = paste0(".currr.data/", name, "/et_", first(current_eval_ids), ".rds"))

    saveRDS(out, file = paste0(".currr.data/", name, "/out_", first(current_eval_ids), ".rds"))
    saveRDS(current_eval_ids, file = paste0(".currr.data/", name, "/id_", first(current_eval_ids), ".rds"))

    eta(name) |>
      (\(x) update_status(name = name, done = x$done, n = x$n, eta = x$eta)) ()
  }

}
