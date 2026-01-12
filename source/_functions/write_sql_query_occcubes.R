write_sql_query_occcubes <- function(
    species_keys,
    year_begin,
    year_end,
    polygon_wtk
) {
  sql_query <- paste0(
    "SELECT
\"year\",
    GBIF_EEARGCode(
      1000,
      decimalLatitude,
      decimalLongitude,
      COALESCE(coordinateUncertaintyInMeters, 1000)
    ) AS eeaCellCode,",
    paste0(
      "\n   GBIF_Within(",
      paste0("'", polygon_wtk, "'"),
      ",\n",
      "      decimalLatitude,
      decimalLongitude
      ) AS withinPolygon,"
    ),
    "
    classKey,
    class,
    speciesKey,
    species,
    COUNT(*) AS occurrences,
    MIN(
      COALESCE(coordinateUncertaintyInMeters, 1000)
    ) AS minCoordinateUncertaintyInMeters,
    MIN(
      GBIF_TemporalUncertainty(eventDate)
    ) AS minTemporalUncertainty,
    IF(
      ISNULL(classKey), NULL, SUM(COUNT(*)) OVER (PARTITION BY classKey)
    ) AS classCount
  FROM
    occurrence
  WHERE
    occurrenceStatus = 'PRESENT'
    AND speciesKey = ",
    species_keys,
    "\nAND continent = 'EUROPE'
    \nAND \"year\" >= ",
    year_begin,
    "\nAND \"year\" <= ",
    year_end,
    "\nAND hasCoordinate = TRUE
    AND speciesKey IS NOT NULL
    AND NOT ARRAY_CONTAINS(issue, 'ZERO_COORDINATE')
    AND NOT ARRAY_CONTAINS(issue, 'COORDINATE_OUT_OF_RANGE')
    AND NOT ARRAY_CONTAINS(issue, 'COORDINATE_INVALID')
    AND NOT ARRAY_CONTAINS(issue, 'COUNTRY_COORDINATE_MISMATCH')
AND (LOWER(identificationVerificationStatus) NOT IN (
      'unverified',
      'unvalidated',
      'not validated',
      'under validation',
      'not able to validate',
      'control could not be conclusive due to insufficient knowledge',
      'uncertain',
      'unconfirmed',
      'unconfirmed - not reviewed',
      'validation requested'
      ) OR identificationVerificationStatus IS NULL)
  GROUP BY
    \"year\",
    eeaCellCode,
    withinPolygon,
    classKey,
    class,
    speciesKey,
    species
  ORDER BY
    \"year\" DESC,
    eeaCellCode ASC,
    speciesKey ASC;
  ")
  return(sql_query)
}
