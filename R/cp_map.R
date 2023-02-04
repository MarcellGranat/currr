#' Run a map with the function, but saves after a given number of execution.
#'
#' @export

cp_map <- function(.x, .f, ..., name = NULL, import = NULL, workers = NULL, fill_method = "default", .board = NULL, n_checkpoint = NULL, notification = NULL) {

  if (is.null(name)) {
    message(".id is suggested")

    name <- deparse(substitute(.f)) |>
      str_remove_all("\\W") |>
      str_to_lower() |>
      str_flatten("")
  }

  if (!dir.exists(".currr.data")) {
    dir.create(".currr.data")
  }

  name_dir <- paste0(".currr.data/", name)
  if (!dir.exists(name_dir)) {
    dir.create(name_dir)
  }

  ids <- seq_along(.x)

  if ("x.rds" %in% list.files(name_dir) & "f.rds" %in% list.files(name_dir)) {
    message("Loading from cache...\r")
    old_f <- read_rds(paste0(name_dir, "/f.rds"))

    equal_f <- tryCatch({
      out <- all.equal(old_f, .f)
      if (!is.logical(out) | length(out) != 1) {
        out <- FALSE
      }
      out
    }, error = \(e) FALSE)

    if (!equal_f) {
      flush.console()
      message("The function is not identical to the one you used previously. ", crayon::red("I restart the process.\r"), "\n")
      list.files(name_dir, full.names = TRUE) |>
        walk(unlink, recursive = TRUE)

      saveRDS(.f, paste0(name_dir, "/f.rds"))

      cache <- 0
    } else {


    old_x <- read_rds(paste0(name_dir, "/x.rds"))

    if (!identical(.x, old_x)) { # save the ones that matches and save
      flush.console()
      message(".x has changed. ", crayon::red("Looking for mathcing result to save them as cache\r"))
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
        map(str_extract_all, "_\\d{1,}") |>
        map(1) |>
        map(str_remove, "_") |>
        map(as.numeric)

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

      old_x_file_name <- str_c(name_dir, "/out_", str_flatten(matching_x_df$new_x_id, "_"), ".rds")

      saveRDS(matching_x_df$outcome, file = old_x_file_name)
      cache <- nrow(matching_x_df)

      ids <- setdiff(ids, matching_x_df$new_x_id)
      flush.console()
      message("Cache updated based on the new .x values\r")
      message("")
      saveRDS(.x, paste0(name_dir, "/x.rds")) # update x

      list.files(name_dir, full.names = TRUE) |> # remove everything else
        setdiff(c(old_x_file_name, paste0(name_dir, "/x.rds"), paste0(name_dir, "/f.rds"))) |>
        walk(unlink, recursive = TRUE)
    } else {
      flush.console()
      message("Everything is unchanged. Reading cache.\r")

      x_file_names <- list.files(name_dir) |>
        keep(str_starts, "out_")

      old_x_ids <- x_file_names |>
        map(str_extract_all, "_\\d{1,}") |>
        map(1) |>
        map(str_remove, "_") |>
        map(as.numeric) |>
        reduce(c)

      list.files(name_dir) |>
        keep(str_starts, "st|et") |>
        (\(x) str_c(name_dir, "/", x)) () |>
        walk(unlink, recursive = TRUE)

      ids <- setdiff(ids, old_x_ids)
      cache <- length(old_x_ids)

    }
    }

  } else { # first run...
    saveRDS(.x, paste0(name_dir, "/x.rds"))
    saveRDS(.f, paste0(name_dir, "/f.rds"))
    cache <- 0
  }


  # TODO check if same
  # exists
  # no: cache 0
  # y: check if the same
  # y: start from the same
  # no: check if f is the same
  # yes: find same x's

  list(
    n = length(.x),
    cache = cache # TODO
  ) |>
    saveRDS(paste0(name_dir, "/meta.rds"))


  if (workers == 1) {
    saving_map(.ids = ids, .f = .f, ... = ..., name = name)
  }

  # Read back

}
