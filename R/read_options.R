#' Get the options from options and cp_options input. Called in cp_map
#' @keywords internal
#' @noRd
#'

read_options <- function(x) {
  if (is.null(getOption("currr.workers"))) {
    workers <- 1 # default
  } else {
    workers <- getOption("currr.workers")
  }
  if ("workers" %in% names(x)) {
    workers <- x$workers
  }
  if (is.null(getOption("currr.wait"))) {
    wait <- Inf
  } else {
    wait <- getOption("currr.wait")
  }
  if ("wait" %in% names(x)) {
    wait <- x$wait
  }
  if (is.null(getOption("currr.fill"))) {
    fill <- TRUE
  } else {
    fill <- getOption("currr.fill")
  }
  if ("fill" %in% names(x)) {
    fill <- x$fill
  }
  if (is.null(getOption("currr.n_checkpoint"))) {
    n_checkpoint <- 100
  } else {
    n_checkpoint <- getOption("currr.n_checkpoint")
  }
  if ("n_checkpoint" %in% names(x)) {
    n_checkpoint <- x$n_checkpoint
  }

  if (is.null(getOption("currr.unchanged_message"))) {
    unchanged_message <- TRUE
  } else {
    unchanged_message <- getOption("currr.unchanged_message")
  }

 if (is.null(getOption("currr.folder"))) {
    currr_folder <- tempdir(check = TRUE)
  } else {
    currr_folder <- getOption("currr.folder")
  }

  if (!workers %in% 1:100) stop("`workers` must be a positive integer, defining the number of core to use for parrallel computing.")
  if (!(wait > 0)) stop("`wait` must be a positive integer or numberic between 0 & 1.")
  if (!fill %in% c(FALSE, TRUE)) stop("`fill` must be TRUE or FALSE.")
  if (!(n_checkpoint > 0 & n_checkpoint %% 1 == 0)) stop("`n_checkpoint` must be a positive interger.")

  if (wait != Inf & is.null(getOption("currr.folder"))) {
    warning("Wait can differ from Inf only if currr.folder is not temporary (default)")
    wait <- Inf
  }

  if (wait != Inf & rstudioapi::isJob()) {
    warning("Wait can differ from Inf if the code is running in RStudio.")
    wait <- Inf
  }

  assign(x = "workers", workers, envir = parent.frame())
  assign(x = "wait", wait, envir = parent.frame())
  assign(x = "fill", fill, envir = parent.frame())
  assign(x = "n_checkpoint", n_checkpoint, envir = parent.frame())
  assign(x = "unchanged_message", unchanged_message, envir = parent.frame())
  assign(x = "currr_folder", currr_folder, envir = parent.frame())
}
