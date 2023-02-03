cp_map <- function(.x, .f, ..., import = NULL,  workers = NULL, fill = "default", name = NULL, .board = NULL, n_checkpoint = NULL, notification = NULL) {

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

  if (!dir.exists(paste0(".currr.data/", name))) {
    dir.create(paste0(".currr.data/", name))
  }

  saveRDS(.x, "x.rds")

  if (.f %in% ls()) {
    if (is.function(.f)) {
      saveRDS(.f, "f.RDS")
    }
  }

  if (workers == 1) {
    saving_map(.ids = seq(.x), name = name)
  }

}
