#' Run a map with the function, but saves after a given number of execution.
#'
#' @export

cp_map <- function(.x, .f, ..., name = NULL, cp_options = list()) {

  if (is.null(name)) {
    auto_generated_name <- TRUE

    name <- str_c(deparse(substitute(.f)), deparse(substitute(.x))) |>
      str_remove_all("\\W") |>
      str_to_lower() |>
      str_flatten("")
  } else {
    auto_generated_name <- FALSE
  }

  read_options(cp_options)

  if (!dir.exists(".currr.data")) {
    dir.create(".currr.data")
  }

  name_dir <- paste0(".currr.data/", name)
  if (!dir.exists(name_dir)) {
    dir.create(name_dir)
  }

  ids <- seq_along(.x)

  if ("x.rds" %in% list.files(name_dir) & "f.rds" %in% list.files(name_dir)) {

    old_f <- read_rds(paste0(name_dir, "/f.rds"))

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
        rstudioapi::jobRemove(.currr.jobId[[name]])
      }, error = \(e) {})

      list.files(name_dir, full.names = TRUE) |>
        walk(unlink, recursive = TRUE)

      saveRDS(.f, paste0(name_dir, "/f.rds"))
      # TODO if
      saveRDS(.x, paste0(name_dir, "/x.rds"))

      cache <- 0
    } else {


      old_x <- read_rds(paste0(name_dir, "/x.rds"))

      if (!identical(.x, old_x)) { # save the ones that matches and save
        flush.console()
        message(crayon::blue(clisymbols::symbol$warning), " .x has changed. ", crayon::red("Looking for mathcing result to save them as cache\r"))

        tryCatch({ # remove previous job
          rstudioapi::jobRemove(.currr.jobId[[name]])
        }, error = \(e) {})

        matching_x_df <- dplyr::inner_join(
          tibble(x = old_x) |>
            dplyr::mutate(old_x_id = row_number()),
          tibble(x = .x) |>
            dplyr::mutate(new_x_id = row_number()),
          by = "x"
        ) |>
          dplyr::select(old_x_id, new_x_id)

        x_file_names <- list.files(name_dir) |>
          keep(str_starts, "out_")

        old_x_ids <- x_file_names |>
          str_replace("out_", "id_") |>
          (\(x) str_c(name_dir, "/", x)) () |>
          map(read_rds)

        contains_relevant_x <- map_lgl(old_x_ids, \(x) any(matching_x_df$old_x_id %in% x))

        old_relevant_outcomes_df <- x_file_names[contains_relevant_x] |>
          map(\(x) str_c(name_dir, "/", x)) |>
          map(read_rds) |>
          (\(x) tibble(old_x_id = old_x_ids[contains_relevant_x], outcome = x)) () |>
          unnest(c(old_x_id, outcome))

        matching_x_df <- matching_x_df |>
          dplyr::inner_join(
            old_relevant_outcomes_df,
            by = "old_x_id"
          )

        old_x_file_name <- str_c(name_dir, "/out_", "cached_from_old", ".rds")
        old_x_id_name <- str_c(name_dir, "/id_", "cached_from_old", ".rds")

        saveRDS(matching_x_df$outcome, file = old_x_file_name)
        saveRDS(matching_x_df$new_x_id, file = old_x_id_name)
        cache <- nrow(matching_x_df)

        ids <- setdiff(ids, matching_x_df$new_x_id)
        flush.console()
        message(crayon::cyan(clisymbols::symbol$circle_dotted), " Cache updated based on the new .x values\r")
        saveRDS(.x, paste0(name_dir, "/x.rds")) # update x

        list.files(name_dir, full.names = TRUE) |> # remove everything else
          setdiff(c(old_x_file_name, old_x_id_name, paste0(name_dir, "/x.rds"), paste0(name_dir, "/f.rds"))) |>
          walk(unlink, recursive = TRUE)

      } else {
        flush.console()
        message(crayon::green("\u2713"), " Everything is unchanged. Reading cache.\r")

        x_file_names <- list.files(name_dir) |>
          keep(str_starts, "out_")

        old_x_ids <- x_file_names |>
          str_replace("out", "id") |>
          (\(x) str_c(name_dir, "/", x)) () |>
          map(read_rds)

        ids <- setdiff(ids, reduce(old_x_ids, c))
        cache <- sum(map_dbl(old_x_ids, length))

      }
    }

  } else { # first run...,
    if (auto_generated_name) {
      message(crayon::blue(clisymbols::symbol$warning), " Using name is suggested. Currently named to ", crayon::cyan(name), ".")
    }
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

    if (!workers %in% 1:100) {
      stop("workers must be an integer!")
    }

    if (wait == Inf) {

      if (workers == 1) {
        saving_map(.ids = ids, .f = .f, name = name, n_checkpoint = n_checkpoint, ... = ...)
      }

      if (workers > 1) {
        library(parallel)
        id_groups <- seq_along(ids) %% workers
        id_list <- map(unique(id_groups), ~ ids[which(id_groups == .)])
        loaded_packages <- pacman::p_loaded()
        cl <- makeCluster(min(workers, detectCores()))
        clusterExport(cl, varlist = c("loaded_packages", "id_list", ".f", "..."), envir = environment())
        clusterExport(cl, varlist = ls(envir = environment()), envir = environment())
        clusterEvalQ(cl, library(tidyverse))
        clusterEvalQ(cl, lapply(loaded_packages, library, character.only = TRUE))
        parLapply(X = id_list, cl = cl, function(x) {
          saving_map(.ids = x, .f = .f, name = name, n_checkpoint = ceiling(n_checkpoint / workers), ... = ...)
        })
        stopCluster(cl)
      }

    } else {

      # Job running? ------------------------------------------------

      job_running <- FALSE
      tryCatch({
        rstudioapi::jobAddOutput(.currr.jobId[[name]], str_c("This job is still, running. ", crayon::cyan(format(Sys.time(), "%H:%M:%S")), "\n"))
        job_running <- TRUE
        message(crayon::cyan(clisymbols::symbol$info), " This evaluation is still running in a bg job.\r")
      }, error = \(e) {})

      if (!job_running) {

        job_id <- job::job({
          if (workers == 1) {
            saving_map(.ids = ids, .f = .f, name = name, n_checkpoint = n_checkpoint, ... = ...)
          }

          if (workers > 1) {
            library(parallel)
            id_groups <- seq_along(ids) %% workers
            id_list <- map(unique(id_groups), ~ ids[which(id_groups == .)])
            loaded_packages <- pacman::p_loaded()
            cl <- makeCluster(min(workers, detectCores()))
            clusterExport(cl, varlist = c("loaded_packages", "id_list", ".f", "..."), envir = environment())
            clusterExport(cl, varlist = ls(envir = environment()), envir = environment())
            clusterEvalQ(cl, library(tidyverse))
            clusterEvalQ(cl, lapply(loaded_packages, library, character.only = TRUE))
            parLapply(X = id_list, cl = cl, function(x) {
              saving_map(.ids = x, .f = .f, name = name, n_checkpoint = ceiling(n_checkpoint / workers),... = ...)
            })
            stopCluster(cl)
          }

        }, title = str_c("Currr: ", name))

        if (!exists(".currr.jobId")) {
          .currr.jobId <<- list()
        }
        if (name %in% names(.currr.jobId)) {
          .currr.jobId[[name]] <<- job_id
        } else {
          .currr.jobId[[length(.currr.jobId) + 1]] <<- job_id
          names(.currr.jobId)[length(.currr.jobId)] <<- name
        }
      }

    }
  }

  # Read back
  Sys.sleep(.01)
  still_wait <- wait != 0

  if (wait < 0) {
    stop("Wait must be a positive integer OR positive numeric between 0 an 1.")
  }

  while (still_wait) {

    output_file_names <- list.files(name_dir) |>
      keep(str_starts, "out_")

    if (length(output_file_names) > 0) {

      out_ids <- output_file_names |>
        str_replace("out", "id") |>
        (\(x) str_c(name_dir, "/", x)) () |>
        map(read_rds)

      finished_n <- length(reduce(out_ids, c))

      if (finished_n == length(.x)) {
        still_wait <- FALSE
      }

      if (wait > 0 & wait < 1) {
        if ((finished_n /  length(.x)) >= wait) {
          still_wait <- FALSE
        } else {
          if (finished_n > wait) {
            still_wait <- FALSE
          }
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

      tryCatch({
        eta(name) |>
          (\(x) update_status(name = name, done = x$done, n = x$n, eta = x$eta)) ()
      }, error = \(e) {
        flush.console()
        cat(str_flatten(c("Calculating ETA", rep(".", (message_dots -1) %% 3 + 1), rep(" ", 2 - (message_dots -1) %% 3),  " \r"), collapse = ""))
        flush.console()
      })
      Sys.sleep(.5)
    }
  }

  tryCatch({ # close the job if finished
    if (length(reduce(out_ids, c)) == length(.x)) {
      rstudioapi::jobRemove(.currr.jobId[[name]])
    }
  }, error = \(e) {})

  return(
    tibble(id = out_ids, out = map(output_file_names, ~ read_rds(str_c(name_dir, "/", .)))) |>
      unnest(c(id, out)) |>
      dplyr::left_join(
        x = tibble(id = seq_along(.x)),
        by = "id"
      ) |>
      (\(.data) {
        if (fill) {
          return(.data)
        } else {
          return(filter(.data, id %in% reduce(out_ids, c)))
        }
      }) () |>
      pull(out)
  )
}
