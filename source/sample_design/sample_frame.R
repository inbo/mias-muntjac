rm(list = ls())

# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
# https://rspatialdata.github.io/osm.html
# osmdata::available_features()
# osmdata::available_tags("landuse")

# --- get municipalities ---------------------------------------------

# municipalities
map_mun <- inbospatial::get_feature_wfs(
  wfs = "https://geo.api.vlaanderen.be/VRBG/wfs",
  layer = "VRBG:Refgem"
)
# https://www.vlaanderen.be/datavindplaats/catalogus/voorlopig-referentiebestand
# -gemeentegrenzen-toestand-01-01-2025-correctie-11-02-2025

# CRS: BD72 / Belgian Lambert 72
if (FALSE) sf::st_crs(map_mun)$Name # EPSG:31370

# select relevant municipalities
map_antw <- map_mun |>
  dplyr::filter(
    grepl("Schoten|Brasschaat|Wuustwezel", NAAM)
  )

# extract polygons from multisurface to enable further processing
# (mapview, intersection)
lapply(1:nrow(map_antw),
       function (i) {
         assertthat::are_equal(
           sf::st_as_text(map_antw$SHAPE[i]) |> grep("POLYGON", x = _),
           1
         )
       })
map_antw <- sf::st_cast(map_antw, "GEOMETRYCOLLECTION") |>
  sf::st_collection_extract("POLYGON")
if (FALSE) mapview::mapview(map_antw) # verify object

# create buffer around municipalities
buffer_meters = 2000 # meters, since Belgian Lambert
map_antw_merged <- # merge polygons
  map_antw |>
  dplyr::summarise()
tmp_buff <- map_antw_merged |>
  sf::st_buffer(
    dist = buffer_meters
  )
map_buff <- sf::st_difference(
  tmp_buff,
  map_antw_merged
)
if (FALSE) mapview::mapview(map_buff) # verify object


# --- get osm data ---------------------------------------------

# bounding box municipalities + buffer
bbox_buff <- sf::st_bbox(
  map_buff |>
    sf::st_transform(x = _, crs = "EPSG:4326") # transform to wgs84
  )

# overpass queries
# https://wiki.openstreetmap.org/wiki/Forest
q <- osmdata::opq(bbox = bbox_buff)
q_wood <- q |> osmdata::add_osm_feature(key = "natural", value = "wood")
q_forest <- q |> osmdata::add_osm_feature(key = "landuse", value = "forest")
q_orchard <- q |> osmdata::add_osm_feature(key = "landuse", value = "orchard")
q_combined <- q |>
  osmdata::add_osm_features(
  features = list(
    "natural" = "wood",
    "landuse" = "forest",
    "landuse" = "orchard"
  )
)

# retrieve osm data for bounding box
if (FALSE) {
  data_osm_wood <- osmdata::osmdata_sf(q = q_wood)
  mapview::mapview(data_osm_wood$osm_polygons)
  data_osm_forest <- osmdata::osmdata_sf(q = q_forest)
  mapview::mapview(data_osm_forest$osm_polygons)
  data_osm_orchard <- osmdata::osmdata_sf(q = q_orchard)
  mapview::mapview(data_osm_orchard$osm_polygons)
  mapview::mapview(
    list(
      data_osm_wood$osm_polygons,
      data_osm_forest$osm_polygons,
      data_osm_orchard$osm_polygons
    ),
    col.regions = list("blue", "red", "green")
  )
}
data_osm <- osmdata::osmdata_sf(q = q_combined)
if (FALSE) mapview::mapview(data_osm$osm_polygons)

# prepare map habitat
map_hab <- data_osm$osm_polygons |>
  sf::st_transform(x = _, crs = "EPSG:31370")

if (FALSE) sf::st_crs(map_hab)$Name
if (FALSE) mapview::mapview(map_hab)

# --- intersect osm data with municipalities ---------------------------------------------

# intersect osm with municipality maps
map_antw_hab <- sf::st_intersection(
  map_antw,
  map_hab
)
if (FALSE) mapview::mapview(map_antw_hab)

# intersect osm with municipality maps
map_buff_hab <- sf::st_intersection(
  map_buff,
  map_hab
)
if (FALSE) mapview::mapview(map_buff_hab)

# --- list maps ---------------------------------------------

mapnames <-   c(
  "map_antw",
  "map_antw_hab",
  "map_buff",
  "map_buff_hab",
  "map_antw_merged"
)
maplist_31370 <- lapply(
  mapnames,
  \(i){ get(i)}
)
maplist_3857 <- lapply(
  mapnames,
  \(i){
    tmpmap <- sf::st_transform(x = get(i), crs = "EPSG:3857")
  }
)
names(maplist_3857) <- names(maplist_31370) <- mapnames
lapply(maplist_3857, \(x) sf::st_crs(x)$Name) # check transformation
lapply(maplist_31370, \(x) sf::st_crs(x)$Name) # check transformation


# --- save sample frame ---------------------------------------------

saveRDS(maplist_3857, file = "data/gis/sample/sampleframe_maplist_3857.Rds")
saveRDS(maplist_31370, file = "data/gis/sample/sampleframe_maplist_31370.Rds")
