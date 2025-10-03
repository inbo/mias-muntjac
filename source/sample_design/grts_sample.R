if (FALSE) rm(list  = ls())
# https://inbo.github.io/grtsdb/articles/basic.html
#
#
# --- draw sample bounding box ---------------------------------------------

# load maplist sample frame
if (FALSE) maplist_31370 <- readRDS("data/gis/sample/sampleframe_maplist_31370.Rds")

# create and connect to sqlite data base
# if on disc: "data/gis/sample/grts_sample.sqlite"
# if in memory: ":memory:"
db <- grtsdb::connect_db(":memory:")

# bounding box municipalities + buffer
bbox_buff <- sf::st_bbox(
  maplist_31370$map_buff
) |>
  matrix(nrow = 2)
colnames(bbox_buff) <- c("min", "max")
rownames(bbox_buff) <- c("x", "y")

# cell size (in meters)
cellsize <- 100

# sample size
samplesize <- 100

# tesselation
grtsdb::add_level(
  grtsdb = db,
  bbox = bbox_buff,
  cellsize = cellsize
  )

# draw sample
sample <- grtsdb::extract_sample(
  grtsdb = db,
  bbox = bbox_buff,
  cellsize = cellsize,
  samplesize = samplesize * 10
)

# compact data base for storage
grtsdb::compact_db(db)

# disconnect from data base
grtsdb::dbDisconnect(db)


# --- convert sample data into spatial object ---------------------------------------------


# grid cell coordinates
sample_upd <- sample |>
  dplyr::mutate(
    xmin = x1c - cellsize/2,
    xmax = x1c + cellsize/2,
    ymin = x2c - cellsize/2,
    ymax = x2c + cellsize/2
  )
if (FALSE){
sample_upd <- sample_upd |>
  tidyr::pivot_longer(
    cols = c("xmin", "xmax", "ymin", "ymax"),
    names_pattern = "(.)(min|max)",
    names_to = c(".value", "type")
  )
}

# convert to sf object
sample_sf <- sf::st_as_sf(
  x = sample_upd,
  # coords of length 4 taken as xmin, ymin, xmax, ymax
  coords = c("xmin", "ymin", "xmax", "ymax"),
  crs = sf::st_crs(31370)
  )

# --- sample for sample frame ---------------------------------------------


# discard grid cells outside of the sample frame
# HERE: define some minimum overlap
# or define overlap with center of grid cell
sample_hab <- sf::st_filter(
  sample_sf,
  maplist_31370$map_antw_hab
  #,.predicate = sf::st_within
)
if (FALSE) {
  m = mapview::mapview(list(maplist_31370$map_antw_hab, sample_sf), col.regions = list("#0000ff", "#ff8000"))
  m
  mapview::mapshot2(m, url = "media/mapview_snapshot.html", remove_controls = c("homeBotton", "scaleBar", "drawToolbar", "easyButton"))
}

# keep N grid cells with smallest ranking within sample frame
sample_hab_upd <- sample_hab |>
  dplyr::arrange(ranking) |>
  dplyr::slice_head(n = samplesize)


# --- visualize sample ---------------------------------------------

# HERE: convert to Pseudo-mercator

if (FALSE){
#
# base plot
plot_base <- ggplot2::ggplot(data = maplist_31370$map_antw) +
  ggspatial::annotation_map_tile(
    type = "cartolight", zoom = 12, cachedir = tempdir(), alpha = .5
  )
plot_names <- ggplot2::geom_sf_text(ggplot2::aes(label = NAAM),
                                    size = 5,
                                    color = INBOtheme::vl_black,
                                    check_overlap = TRUE
)

# distribution area
geoms_dist <- list(
  ggplot2::geom_sf(fill = INBOtheme::vl_yellow,
                   alpha = .6),
  ggplot2::geom_sf(data = maplist_31370$map_antw_hab,
                   fill = INBOtheme::vl_lightgreen,
                   color = INBOtheme::vl_lightgreen,
                   alpha = .6)
)

# buffer area
geoms_buffer <- list(
  ggplot2::geom_sf(data = maplist_31370$map_buff,
                   fill = INBOtheme::vl_darkyellow,
                   alpha = .6),
  ggplot2::geom_sf(data = maplist_31370$map_buff_hab,
                   fill = INBOtheme::vl_darkgreen,
                   color = INBOtheme::vl_darkgreen,
                   alpha = .6)
)

# combined plot
plot_dist_buff <- plot_base + geoms_dist + geoms_buffer + plot_names +
  # emphasize dist
  ggplot2::geom_sf(data = maplist_31370$map_antw_merged,
                   fill = NA,
                   color = INBOtheme::vl_black,
                   linewidth = 0.8)
# add samples
plot_dist_buff  +
  ggplot2::geom_sf(data = sample_hab_upd,
                   color = INBOtheme::vl_lightred,
                   fill = INBOtheme::vl_lightred)


}












