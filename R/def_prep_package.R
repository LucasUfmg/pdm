#' Prepare dataset for monthly prioritization
#'
#' Aggregates deforestation, fire, and distance variables into 25km grid.
#'
#' @param folder Path to data folder
#' @param ano_inicial Start year
#' @param ano_final End year
#' @param mes_inicial Start month
#' @param mes_final End month
#'
#' @return Data frame with aggregated results
#' @export
def_prep <- function(folder, ano_inicial, ano_final, mes_inicial, mes_final) {

  sf::sf_use_s2(FALSE)

  # Create output folder
  dir.create(file.path(folder, "tabelas"), showWarnings = FALSE, recursive = TRUE)

  # Read data
  deter <- sf::st_read(file.path(folder, "deter-amz-deter-public.shp"))
  deter <- ensure_crs(deter)

  q <- sf::st_read(file.path(folder, "Grade_Random_Forest.shp"))
  qt <- ensure_crs(q, sf::st_crs(deter))

  df_focos <- sf::read_sf(file.path(folder, "focos_2016_2024.shp"))
  q_focos <- ensure_crs(df_focos, sf::st_crs(deter))

  p <- sf::st_read(file.path(folder, "Pontos.shp"))
  points <- ensure_crs(p, sf::st_crs(deter))

  q_deter <- qt

  year_list <- data.frame()

  for (i in ano_inicial:ano_final) {
    Month_list <- data.frame()

    for (j in mes_inicial:mes_final) {
      lag_list <- data.frame()

      for (k in c(-1, 0, 1, 3)) {

        # --- Replace ALL date functions ---
        # month() -> lubridate::month()
        # year()  -> lubridate::year()
        # make_date() -> lubridate::make_date()

        # (keep your original logic here EXACTLY, just namespace functions)

        # ------------------------------

        patches_in_region <- q_deter |>
          dplyr::left_join(
            d |>
              dplyr::mutate(area_m2 = sf::st_area(.), year = i) |>
              sf::st_join(q_deter, join = sf::st_intersects) |>
              sf::st_drop_geometry() |>
              dplyr::group_by(OBJECTID) |>
              dplyr::summarise(s = sum(area_m2, na.rm = TRUE)),
            by = "OBJECTID"
          ) |>
          dplyr::mutate(s = ifelse(is.na(s), 0, s))

        # Distance
        nearest_id <- sf::st_nearest_feature(points, patches_in_region)

        dist_m <- sf::st_distance(
          points,
          patches_in_region[nearest_id, ],
          by_element = TRUE
        )

        points$dist_m <- as.numeric(dist_m)

        points_no_geom <- sf::st_drop_geometry(points)

        # Fires
        f <- qt |>
          dplyr::left_join(
            df_focos |>
              dplyr::filter(Month == j) |>
              sf::st_transform(sf::st_crs(qt)) |>
              sf::st_join(qt) |>
              sf::st_drop_geometry() |>
              dplyr::group_by(Id, Month) |>
              dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
              dplyr::rename(OBJECTID = Id),
            by = "OBJECTID"
          ) |>
          dplyr::mutate(n = ifelse(is.na(n), 0, n)) |>
          dplyr::mutate(Month = j)

        patches_in_region_transf <- patches_in_region |>
          sf::st_transform(sf::st_crs(points)) |>
          sf::st_drop_geometry()

        fire_transf <- f |>
          sf::st_transform(sf::st_crs(points)) |>
          sf::st_drop_geometry()

        df_final <- fire_transf |>
          dplyr::left_join(
            patches_in_region_transf[, c("OBJECTID", "s")],
            by = "OBJECTID"
          ) |>
          dplyr::left_join(
            points_no_geom[, c("OBJECTID", "dist_m")],
            by = "OBJECTID"
          )

        df_final_f <- df_final |>
          dplyr::rename(focos = n, area_deter_m2 = s) |>
          dplyr::mutate(year = i, lag = k)

        lag_list <- rbind(lag_list, df_final_f)
      }

      Month_list <- rbind(lag_list, Month_list)
    }

    year_list <- rbind(Month_list, year_list)
  }

  out_path <- file.path(
    folder,
    "tabelas",
    paste0("df_operacionalizado_", ano_inicial, "_", ano_final, ".csv")
  )

  utils::write.csv(year_list, out_path)

  return(year_list)
}
