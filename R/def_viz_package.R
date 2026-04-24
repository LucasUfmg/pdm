#' Visualize prioritization results
#'
#' @param folder Character. Path to root folder.
#' @param mes_inicial Integer. Initial month (1–12).
#' @param mes_final Integer. Final month (1–12).
#'
#' @return Invisibly returns NULL.
#' @export
def_viz <- function(folder, mes_inicial, mes_final) {

  sf::sf_use_s2(FALSE)

  deter <- sf::st_read(file.path(folder, "deter-amz-deter-public.shp"))

  #pf <- utils::read.csv(file.path(folder, "tabelas", "perc_floresta_por_grid.csv"))
  #pf <- dplyr::rename(pf, id = OBJECTID)

  result <- data.frame()
  dff <- data.frame()

  for (i in mes_inicial:mes_final) {

    prio <- sf::st_read(file.path(folder, "outputs", paste0("v", i), "priorizacao.gpkg"))

    prio <- prio |> dplyr::mutate(
      priority1 = dplyr::case_when(
        is.na(priority) ~ "Low",
        TRUE ~ as.character(priority)
      )
    )

    if (sf::st_crs(prio) != sf::st_crs(deter)) {
      prio <- sf::st_transform(prio, sf::st_crs(deter))
    }
    # agrupa deter jan-abril
    # if (i == 4) {
    #   d <- deter |>
    #     dplyr::mutate(
    #       VIEW_DATE = dplyr::if_else(
    #         lubridate::month(VIEW_DATE) < 4,
    #         lubridate::make_date(lubridate::year(VIEW_DATE), 4, lubridate::day(VIEW_DATE)),
    #         VIEW_DATE
    #       )
    #     ) |>
    #     dplyr::filter(
    #       lubridate::year(VIEW_DATE) == 2022 &
    #         lubridate::month(VIEW_DATE) == i &
    #         CLASSNAME %in% c("DESMATAMENTO_CR", "DESMATAMENTO_VEG", "MINERACAO")
    #     )
    #} else if (i == mes_final) { para dezembro
      # d <- deter |>
      #   dplyr::mutate(
      #     VIEW_DATE = dplyr::if_else(
      #       lubridate::month(VIEW_DATE) > mes_final,
      #       lubridate::make_date(lubridate::year(VIEW_DATE), mes_final, lubridate::day(VIEW_DATE)),
      #       VIEW_DATE
      #     )
      #   ) |>
      #   dplyr::filter(
      #     lubridate::year(VIEW_DATE) == 2022 &
      #       lubridate::month(VIEW_DATE) == i &
      #       CLASSNAME %in% c("DESMATAMENTO_CR", "DESMATAMENTO_VEG", "MINERACAO")
      #   )
    #} else {
      d <- deter |>
        dplyr::filter(
          lubridate::year(VIEW_DATE) == 2022 &
            lubridate::month(VIEW_DATE) == i &
            CLASSNAME %in% c("DESMATAMENTO_CR", "DESMATAMENTO_VEG", "MINERACAO")
        )
    #}

    d <- sf::st_make_valid(d)

    # -------- Plot 1 --------
    grDevices::png(file.path(folder, "outputs", paste0("v", i), paste0("plot_", i, ".png")),
        width = 3000, height = 2400, res = 300)

    print(
      ggplot2::ggplot(prio) +
        ggplot2::geom_sf(ggplot2::aes(fill = priority1)) +
        ggplot2::geom_sf(data = d) +
        ggplot2::theme_minimal()
    )

    grDevices::dev.off()

    # -------- Spatial join --------
    patches_touching1 <- d |>
      sf::st_join(
        prio |> dplyr::select(dplyr::all_of("priority1")),
        join = sf::st_intersects,
        left = FALSE
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(area_m2 = as.numeric(sf::st_area(geometry))) |>
      sf::st_drop_geometry()

    has_deter <- lengths(sf::st_intersects(prio, d)) > 0

    grid_with_patches <- prio |>
      dplyr::mutate(has_patch = lengths(sf::st_intersects(prio, d)) > 0) |>
      dplyr::mutate(
        category = dplyr::case_when(
          priority1 == "High" & has_deter ~ "High + Patch",
          priority1 == "High" & !has_deter ~ "High + No Patch",
          TRUE ~ "Other"
        )
      )

    # -------- Plot 2 (erro comissao mapa) --------
    grDevices::png(file.path(folder, "outputs", paste0("v", i), paste0("erro_comissao_MAPA_", i, ".png")),
        width = 3000, height = 2400, res = 300)

    print(
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = grid_with_patches, ggplot2::aes(fill = category),
                         color = "grey30", size = 0.1) +
        ggplot2::geom_sf(data = d, fill = NA, color = "black", size = 0.2) +
        ggplot2::scale_fill_manual(values = c(
          "High + Patch" = "green",
          "High + No Patch" = "red",
          "Other" = "lightgrey"
        )) +
        ggplot2::theme_minimal()
    )

    grDevices::dev.off()

    # -------- Serie historica --------
    prio1 <- prio |> dplyr::mutate(has_deter = has_deter)

    prio2 <- prio1 |>
      dplyr::mutate(high_with_deter = priority1 == "High" & has_deter)

    #prio3 <- merge(prio2, pf)

    prio_summary <- prio2 |>
      dplyr::mutate(interaction_var = interaction(priority1, has_deter)) |>
      dplyr::filter(interaction_var %in% c("High.FALSE", "High.TRUE")) |> #interaction(priority1, has_deter)
      dplyr::group_by(interaction_var) |> #interaction(priority1, has_deter)
      dplyr::summarise(n = dplyr::n()) |>
      sf::st_drop_geometry() |>
      dplyr::mutate(month = i)

    dff <- rbind(dff, prio_summary)

    # -------- Commission rate --------
    high_cells <- grid_with_patches |> dplyr::filter(priority1 == "High")
    n_total_high <- nrow(high_cells)

    commission_errors <- grid_with_patches |>
      dplyr::filter(priority1 == "High" & has_patch == FALSE)

    n_commission <- nrow(commission_errors)
    commission_rate <- n_commission / n_total_high

    area_by_region <- patches_touching1 |>
      dplyr::group_by(priority1) |>
      dplyr::summarise(area_tot = sum(area_m2)) |>
      dplyr::mutate(area_comp = sum(area_tot)) |>
      dplyr::group_by(priority1) |>
      #`100 * area_tot/area_comp`
      dplyr::summarise( p = 100 * area_tot / area_comp) |>
      dplyr::mutate(ref_yer = i, comission_rate = commission_rate)

    result <- rbind(result, area_by_region)
  }

  # -------- Plot 3 --------
  grDevices::png(file.path(folder, "outputs", "plot_percentual.png"),
      width = 4000, height = 3000, res = 300)

  print(
    result |>
      ggplot2::ggplot() +
      ggplot2::aes(x = ref_yer, y = p, color = priority1) + #`100 * area_tot/area_comp`
      ggplot2::geom_point(size = 2.5) +
      ggplot2::geom_line(size = 1.1) +
      ggplot2::scale_x_continuous(breaks = 1:12, labels = month.name) +
      ggplot2::xlab("Months") +
      ggplot2::ylab("%") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(p, 2)), #`100 * area_tot/area_comp`
        vjust = -0.5, size = 4
      ) +
      ggplot2::theme_classic(base_size = 16)
  )

  grDevices::dev.off()

  # -------- Plot 4 --------
  grDevices::png(file.path(folder, "outputs", "comissao.png"),
      width = 3000, height = 3000, res = 300)

  print(
    result |>
      dplyr::group_by(ref_yer) |>
      dplyr::summarise(n = mean(comission_rate * 100)) |>
      ggplot2::ggplot() +
      ggplot2::aes(x = ref_yer, y = n) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::geom_line(size = 1.1) +
      ggplot2::scale_x_continuous(breaks = 1:12, labels = month.name) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(n, 2)),
        vjust = -1, size = 4
      ) +


      ggplot2::ggtitle("Comission Erros")
  )

  grDevices::dev.off()

  # -------- Plot 5 --------
  grDevices::png(file.path(folder, "outputs", "erro_comissao_final.png"),
      width = 3000, height = 2400, res = 300)

  print(
    dff |>
      dplyr::mutate(
        month = factor(month, levels = 1:12,
                       labels = month.abb)
      ) |>
      dplyr::group_by(month) |>
      dplyr::mutate(perc = n / sum(n) * 100) |>
      ggplot2::ggplot(ggplot2::aes(x = month, y = n,
                                   fill = interaction_var)) + #`interaction(priority1, has_deter)`
      ggplot2::geom_col(position = "dodge") +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(perc, 1), "%")),
        position = ggplot2::position_dodge(0.8),
        vjust = -0.5
      ) +
      ggplot2::theme_minimal()
  )

  grDevices::dev.off()
}
