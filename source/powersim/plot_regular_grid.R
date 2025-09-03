# number of cells in the grid along x and y dimension
n_cells_x <- 3
n_cells_y <- 3

# number of points per grid cell along x and y dimension
n_points_x <- 2
n_points_y <- 3

# create data wth coordinates
data_grid <- data.frame(
  cell_x_max = seq(1, n_cells_x)
) |>
  tidyr::crossing(
    cell_y_max = seq(1, n_cells_y)
  ) |>
  dplyr::mutate(
    cell_id = dplyr::row_number() |> paste(),
    cell_x_min = cell_x_max - 1,
    cell_y_min = cell_y_max - 1,
    .before = 1
  ) |>
  dplyr::mutate(
    cell_x_width = cell_x_max - cell_x_min,
    cell_y_width = cell_y_max - cell_y_min,
    cell_x_nseg = n_points_x,
    cell_y_nseg = n_points_y,
    cell_x_lengthseg = cell_x_width / cell_x_nseg,
    cell_y_lengthseg = cell_y_width / cell_y_nseg
  ) |>
  tidyr::crossing(
    point_x = seq(1, n_points_x),
    point_y = seq(1, n_points_y)
  ) |>
  dplyr::mutate(
    point_x = cell_x_min + (point_x - 1 / 2) * cell_x_lengthseg,
    point_y = cell_y_min + (point_y - 1 / 2) * cell_y_lengthseg
  ) |>
  dplyr::mutate(
    point_id = dplyr::row_number() |> paste(),
    .by = cell_id,
    .before = point_x
  )

# plot data
plot_grid <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = data_grid |> dplyr::distinct(cell_id, .keep_all = TRUE),
    mapping = ggplot2::aes(
      xmin = cell_x_min, xmax = cell_x_max,
      ymin = cell_y_min, ymax = cell_y_max
    ),
    fill = NA,
    color = "red"
  ) +
  ggplot2::geom_point(
    data = data_grid,
    mapping = ggplot2::aes(
      x = point_x, y = point_y
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    x = "x",
    y = "y"
  )
plot_grid

# simulate observations
data_y <- data_grid |>
  dplyr::select(tidyselect::all_of(c("cell_id", "point_id"))) |>
  # effect of the cell
  dplyr::mutate(
    b_cell = rnorm(n = 1, m = 0, sd = 2),
    .by = cell_id
  ) |>
  # effect of the point
  dplyr::mutate(
    b_point = rnorm(n = 1, m = 0, sd = .1),
    .by = point_id
  ) |>
  # linear predictor & event rate
  dplyr::mutate(
    eta = b_cell + b_point,
    exp_eta = exp(eta)
  ) |>
  # observed counts
  dplyr::rowwise() |>
  dplyr::mutate(
    y = rpois(n = 1, lambda = exp_eta)
  )

# plot simulated observations
data_merged <- data_grid |>
  dplyr::full_join(data_y)
plot_grid + ggplot2::geom_point(
  data = data_merged,
  mapping = ggplot2::aes(
    x = point_x,
    y = point_y,
    fill = y
  ),
  shape = 21,
  size = 3
) +
  ggplot2::scale_fill_viridis_c(direction = -1)
