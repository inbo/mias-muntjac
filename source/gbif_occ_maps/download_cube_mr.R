rm(list = ls())
file_gis_source <- "data/gis/prius/flanders_wgs84.geojson"
path_gis_destin <- "data/gis/gbif_occ"
path_gbif_destin <- "data/gbif_occ"

# --- prepare download ------------------------------------------------------

# get accepted GBIF key Chinese muntjac
key_mr <- rgbif::name_backbone_checklist(
  name_data = data.frame(scientificName = "Muntiacus reevesi")
) |>
  dplyr::mutate(
    key_acc = ifelse(status == "SYNONYM", acceptedUsageKey, usageKey)
  ) |>
  dplyr::pull(key_acc)

# define time frame
year_end <- lubridate::year(Sys.Date())
year_begin <- year_end - 3

# define polygon flanders
fla_4326 <- sf::st_read(file_gis_source)
fla_31370 <-  sf::st_transform(x = fla_4326, crs = "EPSG:31370")
fla_3857 <-  sf::st_transform(x = fla_4326, crs = "EPSG:3857")
if (FALSE) mapview::mapview(fla_31370)

# define buffer + bounding box
buffer <- 1000 * 30 # in meters
fla_31370_buffer <- sf::st_buffer(x = fla_31370, dist = buffer)
fla_31370_bbox <- sf::st_bbox(fla_31370_buffer)
fla_3857_buffer <- sf::st_buffer(x = fla_3857, dist = buffer)
fla_3857_bbox <- sf::st_bbox(fla_3857_buffer)
if (FALSE) mapview::mapview(fla_31370_buffer)
if (FALSE) mapview::mapview(fla_31370_bbox)
if (FALSE) mapview::mapview(fla_3857_bbox)

# save bounding box
if (FALSE){
  sf::st_write(
    obj = fla_31370_bbox |> sf::st_as_sfc(),
    dsn = file.path(path_gis_destin, paste0("fla_31370_bbox_", buffer, "m.shp"))
  )
  sf::st_write(
    obj = fla_3857_bbox |> sf::st_as_sfc(),
    dsn = file.path(path_gis_destin, paste0("fla_3857_bbox_", buffer, "m.shp"))
  )
}

# convert bounding box to WGS84 to polygon to wkt format
fla_bbox_wkt <- fla_31370_bbox |>
  # to WGS 84
  sf::st_transform(x = _, crs = "EPSG:4326") |>
  # to sfc_polygon
  sf::st_as_sfc() |>
  # to wkt
  sf::st_geometry() |>
  sf::st_as_text() |>
  wk::wkt() |>
  wk::wk_orient()

# --- download occurrences ------------------------------------------------------

data_mr <- rgbif::occ_search(
  taxonKey = key_mr,
  geometry = fla_bbox_wkt,
  year = paste0(year_begin, ",", year_end),
  hasCoordinate = TRUE,
  occurrenceStatus = "PRESENT",
  limit = 100000, # set high enough
  hasGeospatialIssue = FALSE
)
occ_mr <- data_mr$data |>
  dplyr::filter(
    !identificationVerificationStatus %in% c(
        "unverified",
        "unvalidated",
        "not validated",
        "under validation",
        "not able to validate",
        "control could not be conclusive due to insufficient knowledge",
        "uncertain",
        "unconfirmed",
        "unconfirmed - not reviewed",
        "validation requested"
    ),
    !is.null(identificationVerificationStatus),
    !is.null(speciesKey)
  )

# check
occ_mr$basisOfRecord |> unique()

# save
save(occ_mr, file = file.path(path_gbif_destin, "occ_mr.rda"))

# --- download cubes via sql ------------------------------------------------------

# used to work end of 2024 but fails now
if (FALSE) {

  source("source/_functions/write_sql_query_occcubes.R")

  # write sql query & validate
  sql_query <- write_sql_query_occcubes(
    species_keys = key_mr,
    year_begin = year_begin,
    year_end = year_end,
    polygon_wtk = fla_bbox_wkt
  )
  rgbif::occ_download_sql_prep(q = sql_query)
  write.table(sql_query, file.path(path_gbif_destin, "sql_query.txt"))

  # download
  gbif_download_key <- rgbif::occ_download_sql(q = sql_query)

  # check status of download
  rgbif::occ_download_meta(key = gbif_download_key) |> purrr::pluck("status")
  # downloads fails due to unknown reasons

  # save download key & metadata
  gbif_download_meta <- list(
    key = gbif_download_key,
    sql_query = sql_query,
    date = Sys.Date()
  )
  save(gbif_download_meta,
       file = file.path(
         path_gbif_destin,
         paste0(
           "gbif_download_", gbif_download_meta$key |> as.character(), ".rda"
         )
       )
  )

  # download zip file
  zip_file <- file.path(path_gbif_destin, paste0(gbif_download_key, ".zip"))
  if (!file.exists(zip_file)) {
    occ <- rgbif::occ_download_get(
      key = gbif_download_key,
      path = occcubes_data_path
    )
  }
  #
  # unzip csv
  occ_file <- paste0(gbif_download_key, ".csv")
  if (!file.exists(occ_file)) {
    unzip(zipfile = zip_file,
          files = occ_file,
          exdir = occcubes_data_path)
    file.remove(zip_file)
  }

}
