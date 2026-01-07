rm(list = ls())

# load maplist sample frame
maplist_31370 <- readRDS("data/gis/sample/sampleframe_maplist_31370.Rds")
maplist_31370 <- maplist_31370[c("map_antw_hab", "map_buff_hab")]


# --- merge overlapping polygons ---------------------------------------------

maplist_31370_merged <- maplist_31370 |>
  purrr::map(.x = _, .f = sf::st_union)
if (FALSE) mapview::mapview(maplist_31370_merged$map_antw_hab)

# --- smooth sample frame ---------------------------------------------

smooth_sampleframe <- function(sf_obj, dist = 20){
  sf_obj |>
    sf::st_buffer(dist = - dist) |>
    sf::st_buffer(dist = + dist)
}

maplist_31370_smooth <- maplist_31370_merged |>
  purrr::map(.x = _, .f = smooth_sampleframe)


if (FALSE) mapview::mapview(
    list(maplist_31370_merged$map_antw_hab, maplist_31370_smooth$map_antw_hab),
    col.regions = list("blue", "red")
  )

# --- multipolygon to polygon ---------------------------------------------

maplist_31370_smooth <- maplist_31370_smooth |>
  purrr::map(.x = _,
             .f = \(x){
               x |>
                 sf::st_cast(to = "POLYGON") |>
                 sf::st_sf()
             })


# --- add original cols ---------------------------------------------

if (FALSE) {

maplist_31370_smooth
tmp <- purrr::map2(
  .x = maplist_31370_smooth,
  .y = maplist_31370,
  .f = \(x, y){
    x |>
      dplyr::rename(geometry = 1) |>
      dplyr::mutate(helper = paste(geometry, collapse = "")) |>
      as.data.frame() |>
      dplyr::left_join(
        x = _ ,
        y = y |>
          as.data.frame() |>
          dplyr::mutate(helper = paste(geometry, collapse = "")) |>
          dplyr::select(-geometry)
        ) |>
      sf::st_sf()
  })

}


# --- save sample frame ---------------------------------------------

saveRDS(maplist_31370_smooth, file = "data/gis/sample/sampleframe_maplist_31370_smooth.Rds")
