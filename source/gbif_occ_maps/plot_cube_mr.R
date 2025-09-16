rm(list = ls())
list.files("source/functions", full.names = TRUE) |>
  lapply(source) |>
  invisible()
gis_data_path <- "data/gis"
species_list_path <- "data/processed/2025-04-30_species_list.Rda"
#
# -------------------------------------------------------------
#
# get map data
map_fla_1km <- sf::st_read(
  file.path(gis_data_path, "prius/vla_1km.geojson")
)
map_fla_borders <- sf::st_read(
  file.path(gis_data_path, "prius/flanders_wgs84.geojson")
)
map_bg <- sf::st_read(
  file.path(gis_data_path, "eea_fla_30000m.shp")
)
map_bg_borders <- sf::st_read(
  file.path(gis_data_path, "fla_buffer_30000m.shp")
)
#
#
# check crs
sf::st_crs(map_fla_1km)$Name
sf::st_crs(map_fla_borders)$Name
sf::st_crs(map_bg)$Name
sf::st_crs(map_bg_borders)$Name
#
# transform to wgs84
map_bg_wgs84 <- map_bg |> sf::st_transform(
  x = _,
  crs = paste0("EPSG:", sf::st_crs(map_fla_1km)$epsg)
)
sf::st_crs(map_bg_wgs84)$Name
#
# get occurrence cube data
# GBIF.org (17 December 2024)
# GBIF Occurrence Download https://doi.org/10.15468/dl.63mdsh
cube <- readr::read_tsv(file = "data/gbif_occcubes/0037665-241126133413365.csv")
cube_poly <- cube |> dplyr::filter(withinpolygon == TRUE)
#
# get species information
species_list <- get(load(species_list_path))
species_data <- species_list$data
#
# extract muntjac data
key_mr <- species_data |>
  dplyr::filter(grepl("Chinese muntjac", vern_name_gbif_eng)) |>
  dplyr::pull(key_gbif_acc)
cube_mr <- cube_poly |>
  dplyr::filter(specieskey == key_mr)
#
#
# plot function
plot_map  <- function(
    data_cube,
    data_bg_borders = map_bg_borders,
    data_fla_borders = map_fla_borders,
    plot_title,
    plot_subtitle = NULL,
    transform = FALSE,
    occurr_max
) {
  if (transform) {
    # transform to epsg:3857 used by open street maps
    data_cube <- sf::st_transform(x = data_cube, crs = "EPSG:3857")
    data_bg_borders <- sf::st_transform(x = data_bg_borders, crs = "EPSG:3857")
    data_fla_borders <- sf::st_transform(x = data_fla_borders, crs = "EPSG:3857")
  }
  ggplot2::ggplot() +
    ggspatial::annotation_map_tile(
      type = "cartolight", zoom = 9, cachedir = tempdir(), alpha = .8
    ) +
    ggplot2::geom_sf(
      data = data_bg_borders,
      fill = NA, size = 0.2, color = "black", linetype = "dashed"
    ) +
    ggplot2::geom_sf(
      data = data_fla_borders,
      fill = NA, size = 0.2, color = "black", linetype = "solid"
    ) +
    ggplot2::geom_sf(
      mapping = ggplot2::aes(fill = occurrences),
      data = data_cube,
      alpha = 1, size = 1
      #,color = "black"
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "plasma",
      rescaler = function(x, from, to){
        ifelse(x > occurr_max, 1, x / occurr_max)
        },
      direction = -1
      ) +
    ggplot2::theme_void() + # remove axes
    ggplot2::coord_sf() +
    ggplot2::labs(title = plot_title, subtitle = plot_subtitle)
}
#
# loop over years
for (year_i in cube_mr$year |> unique() |> sort()){
  cube_mr_i <- cube_mr |>
    dplyr::filter(year == year_i)
  map_cube_i <- merge(
    map_bg_wgs84,
    cube_mr_i,
    by.x = "cellcode",
    by.y = "eeacellcode"
  )
  #
  # ggplot2
  plot_i <- plot_map(
    data_cube = map_cube_i,
    plot_title = paste("Occurrences of Chinese muntjac in", year_i),

    occurr_max =
      # use third largest occurrence value for rescaling
      #cube_mr$occurrences |> unique() |> sort() |> tail(3) |> head(1),
      # use the 95th percentile
      cube_mr$occurrences |> quantile(x = _, probs = .95),
    transform = TRUE
  )
  filepath_i <- paste0(
    "media/gbif_occcubes/plots_ggspatial_muntjac/", year_i, ".png"
    )
  ggplot2::ggsave(
    filename =  filepath_i,
    plot = (plot_i + ggplot2::theme(text = ggplot2::element_text(size = 10))),
    width = 1500,
    height = 800,
    units = "px",
    dpi = 200,
    bg = "white"
  )
}
