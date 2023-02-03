save_current <- function(out, runtime, id, name = "xx", .board = NULL) {
  # TODO name
  if (!dir.exists(".currr.data")) {
    dir.create(".currr.data")
  }

  if (!dir.exists(paste0(".currr.data/", name))) {
    dir.create(paste0(".currr.data/", name))
  }

  saveRDS(id, file = paste0(".currr.data/", name, "/id_", id, ".rds"))
  saveRDS(runtime, file = paste0(".currr.data/", name, "/runtime_", id, ".rds"))
  saveRDS(out, file = paste0(".currr.data/", name, "/out_", id, ".rds"))

  update_status()

}
