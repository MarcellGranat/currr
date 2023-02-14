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
#' @return A list.
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

cp_map <- function(.x, .f, ..., name = NULL, cp_options = list()) {

  if (is.null(name)) {

    name <- stringr::str_c(deparse(substitute(.f)), deparse(substitute(.x))) |>
      stringr::str_remove_all("\\W") |>
      stringr::str_to_lower() |>
      stringr::str_flatten("")

    if (!name %in% list.files(currr_folder)) {
      message(crayon::blue(clisymbols::symbol$warning), " Using name is suggested. Currently named to ", crayon::cyan(name), ".")
    }
  }

  read_options(cp_options)

  if (wait != Inf & .Platform$GUI != "RStudio") {
    message(crayon::blue(clisymbols::symbol$info), " Intermediate result return available only at Rstudio console.")
    wait <- Inf
  }

  if (!dir.exists(currr_folder)) {
    dir.create(currr_folder)
  }

  name_dir <- paste0(currr_folder, "/", name)
  if (!dir.exists(name_dir)) {
    dir.create(name_dir)
  }

  if (!any(stringr::str_starts(list.files(paste0(currr_folder, "/", name)), "out_"))) {
    list.files(paste0(currr_folder, "/", name), full.names = TRUE) |>
      purrr::walk(unlink, recursive = TRUE) # remove all files if no result in it
  }

  ids <- seq_along(.x)

  if ("x.rds" %in% list.files(name_dir) & "f.rds" %in% list.files(name_dir)) {

    old_f <- readr::read_rds(paste0(name_dir, "/f.rds"))

    equal_f <- tryCatch({
      out <- all.equal(old_f, .f)
      if (!is.logical(out) | length(out) != 1) {
        out <- FALSE
      }
      out
    }, error = \(e) FALSE)

    if (!equal_f) {

      message(crayon::red(clisymbols::symbol$cross), " The function is not identical to the one you used previously. ", crayon::red("I restart the process.\r"), "\n")

      tryCatch({ # remove previous job
        job_id_exists <- list.files(currr_folder) |>
          (\(x) x == "currr_job_ids.rds") () |>
          any()

        if (!job_id_exists) {
          saveRDS(list(), file = paste0(currr_folder, "/currr_job_ids.rds"))
        }

        job_ids <- readRDS(paste0(currr_folder, "/currr_job_ids.rds"))

        rstudioapi::jobRemove(job_ids[[name]])
      }, error = \(e) {})

      list.files(name_dir, full.names = TRUE) |>
        purrr::walk(unlink, recursive = TRUE)

      saveRDS(.f, paste0(name_dir, "/f.rds"))
      # TODO if
      saveRDS(.x, paste0(name_dir, "/x.rds"))

      cache <- 0
    } else {


      old_x <- readr::read_rds(paste0(name_dir, "/x.rds"))

      if (!identical(.x, old_x)) { # save the ones that matches and save
        utils::flush.console()
        message(crayon::blue(clisymbols::symbol$warning), " .x has changed. ", crayon::red("Looking for mathcing result to save them as cache\r"))

        tryCatch({ # remove previous job
          job_id_exists <- list.files(currr_folder) |>
            (\(x) x == "currr_job_ids.rds") () |>
            any()

          if (!job_id_exists) {
            saveRDS(list(), file = paste0(currr_folder, "/currr_job_ids.rds"))
          }

          job_ids <- readRDS(paste0(currr_folder, "/currr_job_ids.rds"))

          rstudioapi::jobRemove(job_ids[[name]])
        }, error = \(e) {})

        matching_x_df <- dplyr::inner_join(
          tibble::tibble(x = old_x) |>
            dplyr::mutate(old_x_id = row_number()),
          tibble::tibble(x = .x) |>
            dplyr::mutate(new_x_id = row_number()),
          by = "x"
        ) |>
          dplyr::select(old_x_id, new_x_id)

        x_file_names <- list.files(name_dir) |>
          purrr::keep(stringr::str_starts, "out_")

        old_x_ids <- x_file_names |>
          stringr::str_replace("out_", "id_") |>
          (\(x) stringr::str_c(name_dir, "/", x)) () |>
          purrr::map(readr::read_rds)

        contains_relevant_x <- purrr::map_lgl(old_x_ids, \(x) any(matching_x_df$old_x_id %in% x))

        old_relevant_outcomes_df <- x_file_names[contains_relevant_x] |>
          purrr::map(\(x) stringr::str_c(name_dir, "/", x)) |>
          purrr::map(readr::read_rds) |>
          (\(x) tibble::tibble(old_x_id = old_x_ids[contains_relevant_x], outcome = x)) () |>
          tidyr::unnest(c(old_x_id, outcome))

        matching_x_df <- matching_x_df |>
          dplyr::inner_join(
            old_relevant_outcomes_df,
            by = "old_x_id"
          )

        old_x_file_name <- stringr::str_c(name_dir, "/out_", "cached_from_old", ".rds")
        old_x_id_name <- stringr::str_c(name_dir, "/id_", "cached_from_old", ".rds")

        saveRDS(matching_x_df$outcome, file = old_x_file_name)
        saveRDS(matching_x_df$new_x_id, file = old_x_id_name)
        cache <- nrow(matching_x_df)

        ids <- setdiff(ids, matching_x_df$new_x_id)
        utils::flush.console()
        message(crayon::cyan(clisymbols::symbol$circle_dotted), " Cache updated based on the new .x values\r")
        saveRDS(.x, paste0(name_dir, "/x.rds")) # update x

        list.files(name_dir, full.names = TRUE) |> # remove everything else
          setdiff(c(old_x_file_name, old_x_id_name, paste0(name_dir, "/x.rds"), paste0(name_dir, "/f.rds"))) |>
          purrr::walk(unlink, recursive = TRUE)

      } else {
        if (unchanged_message) {
          utils::flush.console()
          message(crayon::green("\u2713"), " Everything is unchanged. Reading cache.\r")
        }

        x_file_names <- list.files(name_dir) |>
          purrr::keep(stringr::str_starts, "out_")

        old_x_ids <- x_file_names |>
          stringr::str_replace("out", "id") |>
          (\(x) stringr::str_c(name_dir, "/", x)) () |>
          purrr::map(readr::read_rds)

        ids <- setdiff(ids, purrr::reduce(old_x_ids, c))
        cache <- sum(purrr::map_dbl(old_x_ids, length))

      }
    }

  } else { # first run...,
    saveRDS(.x, paste0(name_dir, "/x.rds"))
    saveRDS(.f, paste0(name_dir, "/f.rds"))
    cache <- 0
  }

  list(
    n = length(.x),
    cache = cache # TODO
  ) |>
    saveRDS(paste0(name_dir, "/meta.rds"))

  if (cache < length(.x)) {

    if (wait == Inf) {

      if (workers == 1) {
        currr::saving_map(.ids = ids, .f = .f, name = name, n_checkpoint = n_checkpoint, currr_folder = currr_folder, ... = ...)
      }

      if (workers > 1) {
        id_groups <- seq_along(ids) %% workers
        id_list <- purrr::map(unique(id_groups), ~ ids[which(id_groups == .)])
        loaded_packages <- pacman::p_loaded()
        cl <- parallel::makeCluster(min(workers, parallel::detectCores()))
        parallel::clusterExport(cl, varlist = c("loaded_packages", "id_list", ".f", "..."), envir = environment())
        parallel::clusterExport(cl, varlist = ls(envir = environment()), envir = environment())
        parallel::clusterEvalQ(cl, library(tidyverse))
        parallel::clusterEvalQ(cl, lapply(loaded_packages, library, character.only = TRUE))
        parallel::parLapply(X = id_list, cl = cl, function(x) {
          currr::saving_map(.ids = x, .f = .f, name = name, n_checkpoint = ceiling(n_checkpoint / workers), currr_folder = currr_folder, ... = ...)
        })
        parallel::stopCluster(cl)
      }

    } else {

      # Job running? ------------------------------------------------

      job_running <- FALSE

      job_id_exists <- list.files(currr_folder) |>
        (\(x) x == "currr_job_ids.rds") () |>
        any()

      if (!job_id_exists) {
        saveRDS(list(), file = paste0(currr_folder, "/currr_job_ids.rds"))
      }

      job_ids <- readRDS(paste0(currr_folder, "/currr_job_ids.rds"))
      suppressWarnings({
        tryCatch({
          rstudioapi::jobAddOutput(job_ids[[name]], stringr::str_c("This job is still, running. ", crayon::cyan(format(Sys.time(), "%H:%M:%S")), "\n"))
          job_running <- TRUE
          message(crayon::cyan(clisymbols::symbol$info), " This evaluation is still running in a bg job.\r")
        }, error = \(e) {})
      })

      if (!job_running) {

        job_id <- job::job({
          if (workers == 1) {
            currr::saving_map(.ids = ids, .f = .f, name = name, n_checkpoint = n_checkpoint, currr_folder = currr_folder, ... = ...)
          }

          if (workers > 1) {
            id_groups <- seq_along(ids) %% workers
            id_list <- purrr::map(unique(id_groups), ~ ids[which(id_groups == .)])
            loaded_packages <- pacman::p_loaded()
            cl <- parallel::makeCluster(min(workers, parallel::detectCores()))
            parallel::clusterExport(cl, varlist = c("loaded_packages", "id_list", ".f", "..."), envir = environment())
            parallel::clusterExport(cl, varlist = ls(envir = environment()), envir = environment())
            parallel::clusterEvalQ(cl, library(tidyverse))
            parallel::clusterEvalQ(cl, lapply(loaded_packages, library, character.only = TRUE))
            parallel::parLapply(X = id_list, cl = cl, function(x) {
              currr::saving_map(.ids = x, .f = .f, name = name, n_checkpoint = ceiling(n_checkpoint / workers), currr_folder = currr_folder, ... = ...)
            })
            parallel::stopCluster(cl)
          }

        }, title = stringr::str_c("Currr: ", name))

        if (name %in% names(job_ids)) {
          job_ids[[name]] <- job_id
          saveRDS(job_ids, file = paste0(currr_folder, "/currr_job_ids.rds"))

        } else {
          job_ids[[length(job_ids) + 1]] <- job_id
          names(job_ids)[length(job_ids)] <- name
          saveRDS(job_ids, file = paste0(currr_folder, "/currr_job_ids.rds"))
        }
      }

    }
  } else {
    suppressWarnings({
      tryCatch({
        job_ids <- readRDS(paste0(currr_folder, "/currr_job_ids.rds"))
        rstudioapi::jobRemove(job_ids[[name]])
      }, error = \(e) {})
    })
  }

  # Read back
  Sys.sleep(.01)
  still_wait <- wait != 0

  if (wait < 0) {
    stop("Wait must be a positive integer OR positive numeric between 0 an 1.")
  }

  while (still_wait) {

    output_file_names <- list.files(name_dir) |>
      purrr::keep(stringr::str_starts, "out_")

    if (length(output_file_names) > 0) {

      out_ids <- output_file_names |>
        stringr::str_replace("out", "id") |>
        (\(x) stringr::str_c(name_dir, "/", x)) () |>
        purrr::map(readr::read_rds)

      finished_n <- length(purrr::reduce(out_ids, c))

      if (finished_n == length(.x)) {
        still_wait <- FALSE
      }

      if (wait > 0 & wait < 1) {
        if ((finished_n /  length(.x)) >= wait) {
          still_wait <- FALSE
        }
      } else if (finished_n >= wait) {
        still_wait <- FALSE
      }
    }

    if (still_wait & length(output_file_names) > 0) {

      if (!exists("message_dots")) {
        message_dots <- 0
      }
      message_dots <- message_dots + 1
      suppressWarnings({
        tryCatch({
          eta(name) |>
            (\(x) update_status(name = name, done = x$done, n = x$n, eta = x$eta)) ()
        }, error = \(e) {
          utils::flush.console()
          cat(stringr::str_flatten(c("Calculating ETA", rep(".", (message_dots -1) %% 3 + 1), rep(" ", 2 - (message_dots -1) %% 3),  " \r"), collapse = ""))
          utils::flush.console()
        })
      })
      Sys.sleep(.5)
    }
  }

  tryCatch({ # close the job if finished
    if (length(purrr::reduce(out_ids, c)) >= length(.x)) {
      rstudioapi::jobRemove(job_ids[[name]])
    }
  }, error = \(e) {})

  return(
    tibble::tibble(id = out_ids, out = purrr::map(output_file_names, ~ readr::read_rds(stringr::str_c(name_dir, "/", .)))) |>
      tidyr::unnest(c(id, out)) |>
      dplyr::left_join(
        x = tibble::tibble(id = seq_along(.x)),
        by = "id"
      ) |>
      (\(.data) {
        if (fill) {
          return(.data)
        } else {
          return(dplyr::filter(.data, id %in% purrr::reduce(out_ids, c)))
        }
      }) () |>
      dplyr::pull(out)
  )
}
