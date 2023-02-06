read_options <- function(options) {
  if (is.null(getOption("currr.workers"))) {
    workers <- 1 # default
  }
  if ("workers" %in% names(options)) {
    workers <- options$workers
  }
  if (is.null(getOption("currr.wait"))) {
    wait <- 1
  }
  if ("wait" %in% names(options)) {
    wait <- options$wait
  }
  if (is.null(getOption("currr.fill"))) {
    fill <- TRUE
  }
  if ("fill" %in% names(options)) {
    fill <- options$fill
  }
  if (is.null("currr.n_checkpoint")) {
    n_checkpoint <- 100
  }
  if ("n_checkpoint" %in% names(options)) {
    n_checkpoint <- options$n_checkpoint
  }

  if (!workers %in% 1:100) stop("`workers` must be a positive integer, defining the number of core to use for parrallel computing.")
  if (!(wait > 0)) stop("`wait` must be a positive integer or numberic between 0 & 1.")
  if (!fill %in% c(FALSE, TRUE)) stop("`fill` must be TRUE or FALSE.")
  if (!(n_checkpoint > 0 & n_checkpoint %% 1 == 0)) stop("`n_checkpoint` must be a positive interger.")

  assign(x = "workers", workers, envir = parent.frame())
  assign(x = "wait", wait, envir = parent.frame())
  assign(x = "fill", workers, envir = parent.frame())
  assign(x = "n_checkpoint", n_checkpoint, envir = parent.frame())
}
