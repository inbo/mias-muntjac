rm(list = ls())

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
year_begin <- year_end - 10


# --- download occurrences for Flanders ------------------------------------------------------

data_mr <- rgbif::occ_search(
  taxonKey = key_mr,
  year = paste0(year_begin, ",", year_end),
  gadmLevel1Gid = 'BEL.2_1',
  hasCoordinate = TRUE,
  occurrenceStatus = "PRESENT",
  limit = 100000, # set high enough
  hasGeospatialIssue = FALSE
)
