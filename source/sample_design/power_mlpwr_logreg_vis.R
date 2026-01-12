if (FALSE){
  rm(list = ls())
  #load("output/sample_design/mlpwr_logreg/gridpars_20251022-133925.Rda")
  #load("output/sample_design/mlpwr_logreg/res_list_20251022-133925.Rda")
  grid_pars <- get(load("output/sample_design/mlpwr_logreg/gridpars_20251217-203416.Rda"))
  load("output/sample_design/mlpwr_logreg/res_list_20251217-203416.Rda")
}

# --- extract results -----------------------------------------------

# extract results from designresult object
res_data <- purrr::map(
  seq_along(res_list),
  \(i){
    if (inherits(res_list[[i]], "try-error")) {
      mdd_i <- NA
      power_i <- NA
      se_i <- NA
    } else {
      mdd_i <- res_list[[i]]$final$design[[1]]
      power_i <- res_list[[i]]$final$power[[1]]
      se_i <- res_list[[i]]$final$se[[1]]
    }
    data.frame(
      mdd = mdd_i,
      power_s = power_i,
      power_s_se = se_i,
      index = i
    )
  }
) |>
  dplyr::bind_rows() |>
  dplyr::full_join(
    x = grid_pars,
    y = _,
    by = "index"
  )

# extract individual iterations from designresult object
res_iter <- purrr::map(
  seq_along(res_list),
  \(i){
    if (!inherits(res_list[[i]], "try-error")) {
      tmp_i <- mlpwr::simulations_data(res_list[[i]]) |>
        dplyr::mutate(
          index = i
        )
    }
  }
) |>
  dplyr::bind_rows() |>
  dplyr::rename(power_sim = power, SE_sim = SE) |>
  dplyr::full_join(
    x = _,
    y = grid_pars,
    by = "index"
  )

# --- prep data  -----------------------------------------------

# prepare data for visualization
tmp_mutate <- function(x) {
  x |>
    dplyr::mutate(
      b0_char = paste(b0),
      b0_fct = as.factor(b0_char) |> forcats::fct_rev(),
      x_end_char = paste("max(year*_t) =", x_end),
      x_end_fct = as.factor(x_end_char) |> forcats::fct_rev(),
      u0_var_char = paste("VAR_u0 =", u0_var),
      n_fct = paste("n =", n) |> as.factor() |> forcats::fct_reorder(.x = n),
      cond_char = paste0("b0 =", b0_fct, ",\n", x_end_fct,",\n", u0_var_char)
      # add P_x0, P_x1
    )
}

res_vis <- res_data |>
  tmp_mutate()

res_iter_vis <- res_iter |>
  dplyr::rename(power_nom = power) |>
  tidyr::pivot_longer(
    cols = c("power_surrogate", "SE_surrogate", "power_sim", "SE_sim"),
    names_pattern = "(.*)_(.*)",
    names_to = c(".value", "type")
  ) |>
  tmp_mutate()



# --- visualize -----------------------------------------------


# aggregated simulation results
plot_res <- ggplot2::ggplot(
  data = res_vis,
  mapping = ggplot2::aes(x = n, y = mdd, linetype = b0_fct, shape = b0_fct)
    ) +
  #ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = mdd - mdd_se, ymax = mdd + mdd_se)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_line() +
  ggplot2::geom_point(ggplot2::aes(size = power_s_se)) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(x_end_fct),
    cols = ggplot2::vars(u0_var_char)
    ) +
  ggplot2::labs(
    x = "Number of sampling locations",
    y = "Minimal detectable slope (90 % power)",
    shape = "gamma_00",
    linetype = "gamma_00",
    size = "SE_power"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_blank())


# detailed results
plot_res_iter <- function(
  filter_cond
) {
  ggplot2::ggplot(
    data = res_iter_vis |> dplyr::filter(grepl(filter_cond, b0_fct)),
    ggplot2::aes(
      x = b1, y = power, color = type,
      group = interaction(n_fct, cond_char))
  ) +
    ggplot2::geom_vline(
      data = res_vis |> dplyr::filter(grepl(filter_cond, b0_fct)),
      ggplot2::aes(xintercept = mdd)) +
    ggplot2::geom_hline(yintercept = 0.9) +
    ggplot2::geom_point(alpha = .3) +
    ggplot2::stat_smooth(
      method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE,
      #method = "lm", formula = y ~ poly(x, 3), se = FALSE,
      linetype = "dashed", col = "black", linewidth = 0.5
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(n_fct), rows = ggplot2::vars(cond_char), scales = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, vjust = 0.1, hjust = 0.1))
}

plot_res_iter_b0large <- plot_res_iter("0")
plot_res_iter_b0small <- plot_res_iter("-1")


# plot SE: problem: no SE for b1 (b1 is fixed during simulations)
# post-hoc investigate fit



# --- posthoc -----------------------------------------------

# results in terms of the odds of presence
res_odds <- res_vis |> dplyr::mutate(
  mdd = (exp(mdd)-1)*100, #exp(b0 + mdd)/(1 + exp(b0 + mdd))
  b0 = exp(b0),
  b0_char = paste(b0 |> round(digits  = 3)),
  b0_fct = as.factor(b0_char) |> forcats::fct_rev()
)
plot_res_odds <- ggplot2::`%+%`(plot_res, res_odds) +
  ggplot2::labs(
    y = "Minimal detectable percentage change in the odds of presence (90 % power)",
    shape = "baseline odds",
    linetype = "baseline odds",
    size = "power_s_se"
  ) +
  # re-order legends
  ggplot2::guides(
    size = ggplot2::guide_legend(order = 2),
    linetype = ggplot2::guide_legend(order = 1),
    shape = ggplot2::guide_legend(order = 1)
    )


# results in terms of probability of presence after X=x years
res_prob <- res_vis |>
  dplyr::mutate(
    n_fct = n_fct |> forcats::fct_rev()
  ) |>
  tidyr::crossing(
    xmax = c(1, res_vis$x_end |> unique())
  ) |>
  dplyr::mutate(
    p1x0 = exp(b0)/(1 + exp(b0)),
    p1xmax = exp(b0 + mdd * xmax)/(1 + exp(b0 + mdd * xmax)),
    label_p1x0 = dplyr::case_when(
      (n == max(n) & xmax == min(xmax)) ~
        paste(p1x0 |> round(digits = 3) |> format(digits = 3, nsmall = 3)),
      TRUE ~ NA_character_
      ),
    label_p1xmax = paste(p1xmax |> round(digits = 3) |> format(digits = 3, nsmall = 3)),
    xmax_fct = as.factor(xmax)
  ) |>
  dplyr::filter(
    xmax <= x_end
  )

plot_res_prob <- ggplot2::ggplot(
  data = res_prob,
  mapping = ggplot2::aes(y = n_fct)
) +
  gggenes::geom_gene_arrow(
    ggplot2::aes(xmin = p1x0, xmax = p1xmax, fill = xmax_fct),
    arrowhead_height = grid::unit(3, "mm"),
    arrow_body_height = grid::unit(2, "mm"),
    arrowhead_width = grid::unit(2, "mm"),
    color = NA,
    position = ggplot2::position_dodge2(width = .7, preserve = "single")
  ) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = p1x0)) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::geom_text(
    ggplot2::aes(x = p1x0 + 0.02, label = label_p1x0,
                 y = (res_prob$n |> unique() |> length()) + 1),
    size = 3
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = p1xmax - 0.03, label = label_p1xmax, color = xmax_fct),
    size = 3,
    position = ggplot2::position_dodge2(width = .7, preserve = "single")
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(x_end_fct),
    cols = ggplot2::vars(u0_var_char)
  ) +
  ggplot2::labs(
    title = "Detectable change in probability of presence after x years",
    x = "Probability of presence",
    y = "Number of sampling locations",
    fill = "Number of years"
  ) +
  ggplot2::expand_limits(y = c(0, length(res_prob$n_fct |> levels()) + 1)) +
  ggplot2::scale_color_manual(values = INBOtheme::inbo_palette(3)) +
  ggplot2::scale_fill_manual(values = INBOtheme::inbo_palette(3)) +
  ggplot2::guides(color = "none") +
  ggplot2::scale_x_reverse() +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_blank()) +
  ggplot2::coord_cartesian(ylim = c(1, (res_prob$n |> unique() |> length()) + 1))



data_logit <- data.frame(
  p1 = seq(0.001, 0.999, 0.01)
  ) |> dplyr::mutate(
    eta = log(p1/(1-p1))
  )
data_tmp <- res_prob |>
  dplyr::mutate(mdd = mdd * xmax) |>
  dplyr::filter(u0_var == min(u0_var), x_end == min(x_end), n == 75, xmax == 6) |> # min(n)
  dplyr::select(c("b0", "mdd", "b0_fct", "p1x0", "p1xmax", "b0_fct"))

data_lines <- data_tmp |>
  dplyr::mutate(
    eta_min = b0,
    eta_max = b0 + mdd,
    p1_min = p1x0,
    p1_max = p1xmax
    ) |>
  tidyr::pivot_longer(
    cols = c("eta_min", "eta_max", "p1_min", "p1_max"),
    names_pattern = "(.*)(_min|_max)",
    names_to = c(".value", "type")
  )
plot_logit <- ggplot2::ggplot(
  data = data_tmp
) +
  ggplot2::geom_line(
    data = data_logit,
    ggplot2::aes(x = eta, y = p1)
  ) +
  # markers
  ggplot2::geom_segment(
    ggplot2::aes(
      x = b0,
      xend = b0 + mdd,
      y = 0,
      color = b0_fct
    ),
    linewidth = 2
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = data_logit$eta |> min() |> floor(),
      y = p1x0,
      yend = p1xmax,
      color = b0_fct
    ),
    linewidth = 2
  ) +
  # lines
  ggplot2::geom_segment(
    data = data_lines,
  ggplot2::aes(
    x = eta,
    y = 0,
    yend = p1,
    color = b0_fct
  ),
  linetype = "dashed"
  ) +
  ggplot2::geom_segment(
    data = data_lines,
    ggplot2::aes(
      x = data_logit$eta |> min() |> floor(),
      xend = eta,
      y = p1,
      color = b0_fct
    ),
    linetype = "dashed"
  ) +
  ggplot2::labs(
    x = "Logit(Probability of presence)",
    y = "Probability of presence",
    color = "fixed intercept"
  ) +
  ggplot2::theme_bw()


# results in terms of simulated data
if (FALSE) {
  source("source/_functions/sim_logreg.R")
  data_y <- res_vis |>
    dplyr::mutate(block = rep(seq(1,8), each = 9), .after = index) |>
    dplyr::filter(!is.na(mdd), x_end == min(x_end), u0_var > 0) |>
    dplyr::filter(n %in% c(min(n), max(n)), .by = block) |>
    dplyr::select(c("n", b0_true = "b0", "mdd", "x_end", "VAR_u0")) |>
    dplyr::mutate(
      simdata = purrr::pmap(
        list(
          b0 = b0_true,
          b1 = mdd,
          x_end = x_end, # using seq here causes trouble
          n = 100,
          u0_var = u0_var
        ),
        \(b0, b1, x_end, n, u0_var) {
          sim_logreg(b0 = b0, b1 = b1, x = seq(0, x_end), n = n, u0_var = u0_var)
        }
      )
    ) |>
    tidyr::unnest(simdata)
  plot_res_data <- data_y |>
    dplyr::mutate(
      cond_fct = paste0("b0 = ",b0, ",\nn = ", n, ",\nb1 = ", round(b1, 2)),
      cond_fct = factor(cond_fct, unique(cond_fct))
    ) |>
    ggplot2::ggplot(
      data = _,
      mapping = ggplot2::aes(x = x, y = y, group = id)
    ) +
    ggplot2::geom_line(ggplot2::aes(y = psi), linetype = "longdash", linewidth = 0.5, alpha = .5) +
    ggplot2::geom_line(ggplot2::aes(color = id), linewidth = 1, alpha = .2,
                       position = ggplot2::position_jitter(w = 0.03, h = 0.03)) +
    ggplot2::facet_wrap(facets = ggplot2::vars(cond_fct), ncol = 2) +
    ggplot2::labs(x = "year*", y = "outcome", color = "id") +
    ggplot2::scale_x_continuous(breaks = data_y$x |> unique()) +
    ggplot2::scale_colour_viridis_d(direction = -1, guide = "none") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank())

}
