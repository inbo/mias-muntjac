if (!exists("mapfile")) mapfile <- "data/gis/sample/sampleframe_maplist_31370_smooth.Rds"

maplist_31370 <- readRDS(mapfile)

# get sizes of different areas
get_size <- function(sf_obj) {
  sf_obj |>
    sf::st_area() |>
    sum() |>
    data.frame(area_m2 = _) |>
    dplyr::mutate(
      area_km2 = units::set_units(area_m2, "km^2"),
      area_dm2 = units::set_units(area_m2, "dm^2"),
      area_ha = units::set_units(area_m2, "hectares")
    )
}
maplist_31370_area <- purrr::map(
  maplist_31370[c("map_antw_hab", "map_buff_hab")],
  get_size
  )

# create grid overlapping with areas
create_grid <- function(sf_obj, cellsize) {
  tmp <- sf_obj |>
    sf::st_bbox() |>
    sf::st_make_grid(cellsize = cellsize) |> # result: list of polygons
    as.data.frame() |>
    sf::st_as_sf(
      crs = sf::st_crs(31370)
    ) |>
    sf::st_filter(
      x = _,
      y = sf_obj
      #,.predicate = sf::st_within
    )
}
maplist_31370_grid <- purrr::map(
  maplist_31370[c("map_antw_hab", "map_buff_hab")],
  create_grid,
  cellsize = 100
  )
if (FALSE) mapview::mapview(
  list(maplist_31370$map_antw_hab, maplist_31370_grid$map_antw_hab)
  )


# get number of grid cells
maplist_31370_grid_size <- purrr::map(
  maplist_31370_grid,
  nrow
)
