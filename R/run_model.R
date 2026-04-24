#' Run full MDP pipeline
#'
#' Executes def_prep, def_prio, and def_viz sequentially.
#'
#' @param folder Path to root folder
#' @param ano_inicial Start year
#' @param ano_final End year
#' @param mes_inicial Start month
#' @param mes_final End month
#' @param run_download Logical, whether to run download_data
#' @param run_prio Logical
#' @param run_prep Logical, whether to run def_prep
#' @export
run_pipeline <- function(
    folder,
    ano_inicial = NULL,
    ano_final = NULL,
    mes_inicial = NULL,
    mes_final = NULL,
    run_download = FALSE,
    run_prio = FALSE,
    run_prep = TRUE
) {

  if (is.null(ano_inicial)){
    message("Using default initial year...")
    ano_inicial <- 2018
  }

  if (is.null(ano_final)){
    message("Using default final year...")
    ano_final <- 2022
  }

  if (is.null(mes_inicial)){
    message("Using default initial month...")
    mes_inicial <- 1
  }

  if (is.null(mes_final)){
    message("Using default final month...")
    mes_final <- 12
  }

  if (isTRUE(run_download)) {
    message("Downloading data...")

    folder_id <- "1pvJ9BQ5WD-NT24X_kwKi-kZdLJMbINm9"
    folder_id_tabelas <-'1XinL78HmnY3skbKWRthZlgmeY5cKQBj6'

    paths <- download_data(
      folder_id = folder_id,
      folder_id_tabelas = folder_id_tabelas,
      root1 = folder
    )

    folder <- paths$shp_dir

  }

  if (isTRUE(run_prep)) {
    message("Running def_prep...")
    def_prep(
      folder = folder,
      ano_inicial = ano_inicial,
      ano_final = ano_final,
      mes_inicial = mes_inicial,
      mes_final = mes_final
    )
    message("def_prep completed.")
  } else {
    message("Skipping def_prep.")
  }

  if (isTRUE(run_prio)) {
  message("Running def_prio...")
  def_prio(
    folder = folder,
    ano_inicial = ano_inicial,
    ano_final = ano_final,
    mes_inicial = mes_inicial,
    mes_final = mes_final
  )
  message("def_prio completed.")} else {
    message("Skipping def_prio.")
  }

  message("Running def_viz...")
  def_viz(
    folder = folder,
    mes_inicial = mes_inicial,
    mes_final = mes_final
  )
  message("def_viz completed.")

  message("Pipeline finished.")
}
