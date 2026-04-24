#' Run prioritization model
#'
#' Executes model fitting and prioritization for each month.
#'
#' @param folder Path to data folder
#' @param ano_inicial Start year
#' @param ano_final End year
#' @param mes_inicial Start month
#' @param mes_final End month
#' @importFrom dplyr where
#' @importFrom utils assignInNamespace
#' @importFrom stats as.formula
#' @return Invisible list of output directories
#' @export
def_prio <- function(folder, ano_inicial, ano_final, mes_inicial, mes_final) {

  sf::sf_use_s2(FALSE)

  dir.create(file.path(folder, "outputs"), showWarnings = FALSE, recursive = TRUE)

  pt_infra_f <- utils::read.csv(file.path(folder, "tabelas", "infra_pa.csv"), header = T) |>
    dplyr::select(-X) |>
    dplyr::rename(
      area_PA = PA_area,
      dist_road = dist_rod,
      dist_hidro = dist_hid,
      dist_hidro_road = dist_hidro_rod
    )

  year_list <- utils::read.csv(
    file.path(folder, "tabelas",
              paste0("df_operacionalizado_", ano_inicial, "_", ano_final, ".csv"))
  )

  year_list$area_deter_m2[is.na(year_list$area_deter_m2)] <- 0

  out_dirs <- list()

  for (i in mes_inicial:mes_final) {

    df1_f <- year_list |>
      dplyr::filter(Month == i) |>
      dplyr::mutate(
        lagg = dplyr::case_when(
          lag == "-1" ~ "atual",
          lag == "0"  ~ "ano_ant",
          lag == "1"  ~ "dois_ant",
          lag == "3"  ~ "quatro_ant"
        )
      ) |>
      dplyr::select(-lag)

    df_long <- df1_f |>
      dplyr::mutate(Month_year = paste0(year, "_", sprintf("%02d", Month)))

    df_wide <- df_long |>
      tidyr::pivot_wider(
        id_cols = c(OBJECTID),
        names_from = c(lagg, Month_year),
        values_from = c(area_deter_m2, focos, dist_m),
        names_glue = "{.value}lag{lagg}{Month_year}",
        values_fn = mean
      )

    df11 <- df_wide |>
      dplyr::left_join(pt_infra_f, by = "OBJECTID") |>
      dplyr::mutate(
        dplyr::across(
          where(~ inherits(.x, "units") && as.character(units(.x)) == "m^2"),
          ~ .x / 1e6
        )
      ) |>
      dplyr::mutate(dplyr::across(where(~ inherits(.x, "units")), as.numeric))

    df11[is.na(df11)] <- 0

    out_dir <- file.path(folder, "outputs", paste0("v", i))
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    out_dirs[[paste0("month_", i)]] <- out_dir

    cols <- names(df11)[grepl("^area_deter_m2lagatual2022_\\d{2}$", names(df11))]

    # --- keep your internal functions ---
    read_data <- function(raw = FALSE) {

      col_sym <- rlang::sym(cols)
      print(col_sym)

      data_tb <- df11 |>
        dplyr::mutate(
          def = !!col_sym * 1000^2,
          log_def = log(!!col_sym)
        )

      if (!raw) {
        data_tb <- dplyr::filter(data_tb, def > 0)
      }

      return(data_tb)
    }

    predictors <- df11 |>
      dplyr::select(-OBJECTID) |>
      dplyr::select(where(is.numeric), - !!rlang::sym(cols)) |>
      names()

    my_formula <- stats::reformulate(
      termlabels = predictors,
      response = "log_def"
    )

    build_formula <- function() {
      as.formula(my_formula)
    }

    # keep but risky
    utils::assignInNamespace(".read_data", read_data,
                      ns = "prioritizedeforestationhotspots")

    utils::assignInNamespace(".build_formula", build_formula,
                      ns = "prioritizedeforestationhotspots")

    prioritizedeforestationhotspots::fit_model(out_dir)

    results_tb <- list.files(out_dir,
                             pattern = "new_data_tb.rds",
                             full.names = TRUE) |>
      readRDS() |>
      dplyr::mutate(pred_def_km2 = pred_def / 1e6)

    probs <- c(0, 0.6, 0.95, 1)
    labels <- c("Low", "Average", "High")

    results_year <- results_tb |>
      dplyr::mutate(
        priority = cut(
          pred_def_km2,
          labels = labels,
          include.lowest = TRUE,
          breaks = stats::quantile(pred_def_km2, probs = probs)
        )
      )

    results_year_tmp <- prioritizedeforestationhotspots::deforestation_grid |>
      dplyr::left_join(results_year |>
                         dplyr::rename(id = OBJECTID),
                       by = "id")

    sf::write_sf(results_year_tmp, file.path(out_dir, "priorizacao.gpkg"))
  }

  return(invisible(out_dirs))
}
