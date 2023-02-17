#' Run a map with the function, but saves after a given number of execution. This is an internal function, you are not supposed to use it manually, but can call for background job only if exported. This function differs from saving_map, since it does not have a ... input. This is neccessary because job::job fails if ... is not provided for the cp_map call.
#'
#' @param .ids Placement of .x to work with.
#' @param .f Called function.
#' @param name Name for saving.
#' @param n_checkpoint Number of checkpoints.
#' @param currr_folder Folder where cache files are stored.
#' @return No return value, called for side effects
#' @export

saving_map_nodot <- function(.ids, .f, name, n_checkpoint = 100, currr_folder) {

  x <- readr::read_rds(paste0(currr_folder, "/", name, "/x.rds"))[.ids]

  n_checkpoint <- min(n_checkpoint, length(x))

  q <- as.numeric(floor(stats::quantile(seq_along(x), probs = 1:n_checkpoint / n_checkpoint)))

  for (i in seq(n_checkpoint)) {

    current_eval_ids <- .ids[seq(from = c(0, q)[i] + 1, to = q[i])]

    saveRDS(Sys.time(), file = paste0(currr_folder, "/", name, "/st_", current_eval_ids[1], ".rds"))

    out <- purrr::map(.x = x[seq(from = c(0, q)[i] + 1, to = q[i])], .f)

    saveRDS(Sys.time(), file = paste0(currr_folder, "/", name, "/et_", current_eval_ids[1], ".rds"))

    saveRDS(out, file = paste0(currr_folder, "/", name, "/out_", current_eval_ids[1], ".rds"))
    saveRDS(current_eval_ids, file = paste0(currr_folder, "/", name, "/id_", current_eval_ids[1], ".rds"))

    if (!rstudioapi::isJob()) {
      tryCatch({

      eta(name) |>
        (\(x) update_status(name = name, done = x$done, n = x$n, eta = x$eta)) ()
      }, error = \(e) {})
    }
  }

}
