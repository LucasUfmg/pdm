#' Download data from drive
#'
#' Help download from gdrive
#'
#' @param folder_id Shps
#' @param folder_id_tabelas tabela
#' @param root1 caminho data shp
#' @export
download_data <- function(folder_id,
                          folder_id_tabelas,
                          root1 = "data/shp") {
  googledrive::drive_deauth()

  folder <- googledrive::as_id(folder_id)
  folder_tabelas <- googledrive::as_id(folder_id_tabelas)

  files <- googledrive::drive_ls(folder)
  files_tabelas <- googledrive::drive_ls(folder_tabelas)

  shp_ext <- "\\.(shp|shx|dbf|prj|cpg)$"
  shp_files <- files[grepl(shp_ext, files$name, ignore.case = TRUE), ]

  root2 <- file.path(root1, "tabelas")

  dir.create(root1, recursive = TRUE, showWarnings = FALSE)
  dir.create(root2, recursive = TRUE, showWarnings = FALSE)

  # download shapefiles
  for (i in seq_len(nrow(shp_files))) {
    local_path <- file.path(root1, shp_files$name[i])

    if (!file.exists(local_path)) {
      message("Skipping file already downloaded...")
      googledrive::drive_download(
        file = shp_files[i,],
        #file = dplyr::slice(shp_files, i),
        path = local_path,
        overwrite = FALSE
      )
    }
    Sys.sleep(5)
  }

  # download tables
  #download_and_unzip(folder_id_tabelas,root2)
  for (i in seq_len(nrow(files_tabelas))) {
    local_path <- file.path(root2, files_tabelas$name[i])

    if (!file.exists(local_path)) {
      message("Skipping file already downloaded...")
      googledrive::drive_download(
        file = dplyr::slice(files_tabelas, i),
        path = local_path,
        overwrite = FALSE
      )

      Sys.sleep(1)
    }
  }


  return(list(shp_dir = root1, tab_dir = root2))
}
