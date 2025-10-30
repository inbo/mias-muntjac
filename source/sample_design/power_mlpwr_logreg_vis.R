if (FALSE){
  rm(list = ls())
  load("output/sample_design/mlpwr_logreg/gridpars_20251022-133925.Rda")
  load("output/sample_design/mlpwr_logreg/res_list_20251022-133925.Rda")
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
      u0_var_char = paste("u0_var =", u0_var),
      #n_fct = as.factor(n),
      n_fct = paste("n =", n) |> as.factor() |> forcats::fct_reorder(.x = n),
      cond_char = paste0("b0 =", b0_fct, ",\n", x_end_char,",\n", u0_var_char)
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
    rows = ggplot2::vars(x_end_char),
    cols = ggplot2::vars(u0_var_char)
    ) +
  ggplot2::labs(
    x = "Number of sampling locations",
    y = "Minimal detectable slope (90 % power)",
    shape = "b0",
    linetype = "b0"
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


# results in terms of probability of presence after X years
res_prob <- res_vis |>
  tidyr::crossing(
    x = c(0, 4)
  ) |>
  dplyr::mutate(
    p1x = exp(b0 + mdd * x)/(1 + exp(b0 + mdd * x))
) |>
  tidyr::pivot_wider(
    values_from = p1x,
    names_from = x,
    names_prefix = "x_"
  )

ggplot2::ggplot(
  data = res_prob,
  mapping = ggplot2::aes(x = n, color = b0_fct)
) +
  #ggplot2::geom_linerange(
  #  ggplot2::aes(ymin = x_0, ymax = x_4),
  #  position = ggplot2::position_dodge2(width = 0)
  #) +
  ggplot2::geom_segment(
    ggplot2::aes(y = x_0, yend = x_4),
    arrow = grid::arrow(length = grid::unit(0.7, "lines"), type = "open", angle = 40),
    linewidth = 2, lineend = "butt", linejoin = "mitre"
  ) +
  #gggenes::geom_gene_arrow(ggplot2::aes(ymin = x_0, ymax = x_4)) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(x_end_char),
    cols = ggplot2::vars(u0_var_char)
  ) +
  ggplot2::labs(
    x = "Number of sampling locations",
    y = "Probability of presence",
    shape = "b0",
    linetype = "b0"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_blank())
