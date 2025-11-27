# rm(list = ls())
if (!exists("gis_data_path"))
  gis_data_path <- "data/gis"
if (!exists("gbif_data_file"))
  gbif_data_file <- "data/gbif_occ/occ_mr_4326.rda"
if (!exists("save_plot_path"))
  save_plot_path <- "output/gbif_occ/plots_ggspatial_muntjac"
#
# -------------------------------------------------------------

# get map data
map_fla <- sf::st_read(
  file.path(gis_data_path, "prius/flanders_wgs84.geojson")
)
map_bbox <- sf::st_read(
  file.path(gis_data_path, "gbif_occ/fla_31370_bbox_30000m.shp")
) |>
  sf::st_transform(x = _, crs = "EPSG:4326")

# get occurrence data
occ_mr <- get(load(gbif_data_file))

# process
occ_mr <- occ_mr |>
  dplyr::filter(!is.na(identificationVerificationStatus)) |>
  dplyr::mutate(
    data_INBO = dplyr::case_when(
      grepl("Invasive species - Chinese muntjac", datasetName) ~ TRUE,
      grepl("MUNTJAC_ANTWERP", datasetName) ~ TRUE,
      TRUE ~ FALSE
    )
  )


# check crs
sf::st_crs(map_fla)$Name # "WGS 84"
sf::st_crs(map_bbox)$Name # "WGS 84"
sf::st_crs(occ_mr)$Name # "WGS 84"

# plot function
plot_map  <- function(
    data_occ,
    data_bbox = map_bbox,
    data_fla =map_fla,
    plot_title,
    plot_subtitle = NULL,
    transform = FALSE,
    facet_year = FALSE
) {
  if (transform) {
    # transform to Pseudo mercator used by open street maps
    data_fla <- data_fla |> sf::st_transform(x = _, crs = "EPSG:3857")
    data_bbox <- data_bbox |> sf::st_transform(x = _, crs = "EPSG:3857")
    data_occ <- data_occ |> sf::st_transform(x = _, crs = "EPSG:3857")
  }
  ggplot2::ggplot() +
    ggspatial::annotation_map_tile(
      type = "cartolight", zoom = 9, cachedir = tempdir(), alpha = .3
    ) +
    ggplot2::geom_sf(
      data = data_bbox,
      fill = NA, size = 0.2, color = "black", linetype = "dashed"
    ) +
    ggplot2::geom_sf(
      data = data_fla,
      fill = NA, size = 0.2, color = "black", linetype = "solid"
    ) +
    ggplot2::geom_sf(
      data = data_occ,
      ggplot2::aes(color = data_INBO),
      size = 2,
      fill = NA,
      shape = 21
    ) +
    ggplot2::geom_sf(
      data = data_occ,
       ggplot2::aes(color = data_INBO),
      alpha = 0.2, size = 2
    ) +
    ggplot2::scale_color_manual(
      values = c(INBOtheme::inbo_oranje, INBOtheme::inbo_hoofd)
      ) +
    ggplot2::theme_void() + # remove axes
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::coord_sf() +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      color = "data published by INBO"
      ) +
    ggplot2::facet_wrap(
      facets = if (facet_year) ggplot2::vars(year) else NULL,
      nrow = 2
      )
}

# facet years
plot_facet <- plot_map(
  data_occ =   occ_mr,
  plot_title = "Occurrences of Chinese muntjac",
  transform = TRUE,
  facet_year = TRUE
)


# loop over years
if (FALSE) {
years <- occ_mr$year |> unique() |> sort()
plot_list <- purrr::map(
  years,
  function(year_i){
    plot_i <- plot_map(
      data_occ =   occ_mr |>
        dplyr::filter(year == year_i),
      plot_title = year_i,
      transform = TRUE
    )
  }
)
plot_list <- setNames(plot_list, years)
}
