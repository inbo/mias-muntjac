if (!exists("mapfile")) mapfile_exampl <- "data/gis/sample/sampleframe_maplist_31370.Rds"

maplist_31370 <- readRDS(mapfile_exampl)

# Park Vordenstein and surroundings
# see: https://belevingskaart.natuurenbos.be/?zoomtoobject=339&findinlayer=18
if (FALSE) maplist_31370$map_antw_hab[3,] |> mapview::mapview()


poly <- maplist_31370$map_antw_hab[3,]

data_poly <- rbind(
  poly |>
    dplyr::mutate(polygon = "original polygon..."),
  poly |>
    sf::st_buffer(dist = -100) |>
    dplyr::mutate(polygon = "...minus buffer (erosion)..."),
  poly |>
    sf::st_buffer(dist = -100) |>
    sf::st_buffer(dist = +100) |>
    dplyr::mutate(polygon = "...plus buffer (dilation)")
) |>
  dplyr::mutate(
    polygon = as.factor(polygon) |> relevel(ref = "original polygon...")
  )

data_contour <- poly |>
  dplyr::bind_cols(polygon = data_poly$polygon)


plot_poly_smooth <- ggplot2::ggplot(data = data_poly) +
  ggplot2::geom_sf(fill = INBOtheme::vl_lightgreen, color = NA) +
  ggplot2::geom_sf(data = data_contour, fill = NA) +
  ggplot2::facet_wrap(ggplot2::vars(polygon)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )
