alpha <- 0.1

data_ec <- data.frame(
  psi_0 = 0,
  nl = seq(1,100)
) |>
  dplyr::mutate(
    mdd = purrr::map_dbl(nl, \(x){
      get_ciu_k0(nl = x, conf_level = 1 - alpha)
    })
  ) |>
  dplyr::mutate(
    obs = purrr::map(
      nl,
      \(x){ seq(0, x)}
    )
  ) |>
  tidyr::unnest(obs) |>
  dplyr::mutate(
    psi_obs = obs/nl,
    # conduct binomial test
    bt = purrr::pmap(
      list(
        x = .data$obs,
        n = .data$nl,
        p = .data$psi_0,
        alternative = "two.sided", # inconsistent
        conf.level = 1 - alpha
      ),
      binom.test
    ),
    # get ci estimate
    ci = purrr::map(.data$bt, "conf.int"),
    ci_l = purrr::map_dbl(.data$ci, ~ .x[1]),
    ci_u = purrr::map_dbl(.data$ci, ~ .x[2]),
    # effect class classification
    ec_class = purrr::pmap(
      list(
        lcl = .data$ci_l,
        ucl = .data$ci_u,
        threshold = mdd,
        reference = .data$psi_0
      ),
      effectclass::classification
    ) |> unlist()
  )

plot_ec <- data_ec |>
  ggplot2::ggplot(ggplot2::aes(x = nl, y = psi_obs)) +
  ggplot2::geom_point(ggplot2::aes(color = ec_class)) +
  ggplot2::geom_line(ggplot2::aes(y = mdd)) +
  ggplot2::labs(
    x = "Number of sampling locations",
    y = "Probability of success (corresponding to observable effects)" #(observed)
    #title = "Classification of observable effects with respect to target and minimal detectable effect"
  ) +
  ggplot2::theme_bw() +
  ggplot2::scale_color_manual(
    values = effectclass:::detailed_signed_palette,
    name = "Interpretation",
    labels = c(
      "++" = "strongly over\ntarget",
      "+" = "over\ntarget",
      "+~" = "moderately over\ntarget",
      "~" = "around\ntarget",
      "-~" = "moderately under\ntarget",
      "-" = "under\ntarget",
      "--" = "strongly under\ntarget",
      "?+" = "potentially over\ntarget",
      "?-" = "potentially under\ntarget",
      "?" = "unclear"
    )
  ) +
  ggplot2::theme(legend.key.size = grid::unit(1, "lines"))



