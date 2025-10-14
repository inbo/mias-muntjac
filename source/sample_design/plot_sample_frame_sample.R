maplist_31370 <- readRDS("data/gis/sample/sampleframe_maplist_31370.Rds")
source("source/sample_design/grts_sample.R")

# ------------
# outputs are:
# ------------
# maplist_31370$map_antw_hab: sampling frame antw
# maplist_31370$map_buff_hab: sampling frame buffer
# sample_sf: sampling frame
# sample_hab_upd: sample within habitat of size N


# --- visualize sample ---------------------------------------------

# HERE: convert to Pseudo-mercator

# base plot
plot_base <- ggplot2::ggplot(data = maplist_31370$map_antw) +
  ggspatial::annotation_map_tile(
    type = "cartolight", zoom = 12, cachedir = tempdir(), alpha = .5
  )
plot_names <- ggplot2::geom_sf_text(ggplot2::aes(label = NAAM),
                                    size = 4,
                                    color = INBOtheme::vl_black,
                                    check_overlap = TRUE
)

# geoms distribution area
geoms_dist <- list(
  ggplot2::geom_sf(fill = INBOtheme::vl_yellow,
                   alpha = .3,
                   color = NA),
  ggplot2::geom_sf(data = maplist_31370$map_antw_hab,
                   fill = INBOtheme::vl_lightgreen,
                   color = INBOtheme::vl_lightgreen,
                   alpha = .6)
)


# geoms buffer area
geoms_buffer <- list(
  ggplot2::geom_sf(data = maplist_31370$map_buff,
                   fill = INBOtheme::vl_darkyellow,
                   alpha = .3,
                   color = NA),
  ggplot2::geom_sf(data = maplist_31370$map_buff_hab,
                   fill = INBOtheme::vl_darkgreen,
                   color = INBOtheme::vl_darkgreen,
                   alpha = .6)
)

# plot distribution area
plot_dist <- plot_base + geoms_dist + plot_names + ggplot2::theme_void()

# plot distribution + buffer area
plot_dist_buff <- plot_base + geoms_dist + geoms_buffer + plot_names +
  # emphasize dist
  ggplot2::geom_sf(data = maplist_31370$map_antw_merged,
                   fill = NA,
                   color = INBOtheme::vl_black,
                   linewidth = 0.8)

# plot distribution area + sample
plot_dist_sample <- plot_base + geoms_dist + plot_names +
  ggplot2::geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                     fill = "white", alpha = .3) +
  ggplot2::geom_sf(data = sample_hab_upd,
                   color = INBOtheme::vl_lightred,
                   fill = INBOtheme::vl_lightred) +
  ggplot2::theme_void()
tmp <- mapview::mapview(
  list(maplist_31370$map_antw_hab, sample_hab_upd),
  col.regions = list(INBOtheme::vl_yellow, INBOtheme::vl_lightred)
)

# save pngs for presentation
library(patchwork)
if (FALSE) ggplot2::ggsave("media/img/sample_design/plot_dist_sample.png", (plot_dist + plot_dist_sample))

