sim_binom_ci <- function(
    n_max = 500, # max number of trials
    n_min = 10, # min number of trials
    successes = 0, # number of successes
    psi_0 = 0.5, # probability of success under h_0 - does not matter for ci
    alpha = 0.05, # type 1 error
    alternative = c("two.sided", "less", "greater"),
    plot = TRUE
){
  assertthat::assert_that(
    successes <= n_min && successes <= n_max,
    msg = "the number of successes is impossible given the number of trials
    (or the reverse)"
  )
  alternative <- match.arg(alternative)
  data_ci <- data.frame(
    n = seq_len(n_max - n_min) + n_min,
    psi_0 = psi_0,
    successes = successes,
    alpha = alpha
  ) |>
    # conduct binomial test
    dplyr::mutate(
      bt = purrr::pmap(
        list(
          x = .data$successes,
          n = .data$n,
          p = .data$psi_0,
          alternative = alternative,
          conf.level = 1 - alpha
        ),
        binom.test
      ),
      # get ci estimate
      ci = purrr::map(.data$bt, "conf.int"),
      ci_l = purrr::map_dbl(.data$ci, ~ .x[1]),
      ci_u = purrr::map_dbl(.data$ci, ~ .x[2]),
    ) |>
    # highlight points
    dplyr::mutate(
      # fixed sample sizes
      ci_hl_n = dplyr::case_when(
        n %in% c(50, 100, 200, 400) ~ ci_u,
        TRUE ~ NA_real_
      ),
      # fixed percentages
      tmp = (ci_u - 0.10) |> abs(),
      ci_hl_p = dplyr::case_when(
        tmp == min(tmp) ~ ci_u,
        TRUE ~ NA_real_
      ),
      tmp = (ci_u - 0.05) |> abs(),
      ci_hl_p = dplyr::case_when(
        tmp == min(tmp) ~ ci_u,
        TRUE ~ ci_hl_p
      ),
      tmp = (ci_u - 0.01) |> abs(),
      ci_hl_p = dplyr::case_when(
        tmp == min(tmp) ~ ci_u,
        TRUE ~ ci_hl_p
      ),
      # labels
      ci_hl_n_lab = dplyr::case_when(
        !is.na(ci_hl_n) ~ sprintf("(%.*f, %.*f)", 0, n, 4, ci_u),
        TRUE ~ NA_character_
      ),
      ci_hl_p_lab = dplyr::case_when(
        !is.na(ci_hl_p) ~ sprintf("(%.*f, %.*f)", 0, n, 4, ci_u),
        TRUE ~ NA_character_
      )
    ) |>
    # reformat for plotting
    tidyr::pivot_longer(
      cols = c("ci_l", "ci_u"),
      names_to = "ci_name",
      values_to = "ci_value"
    ) |>
    dplyr::mutate(
      ci_name = ci_name |> forcats::fct_rev()
    )
  plot_ci <- if (plot) {ggplot2::ggplot(
    data = data_ci,
    ggplot2::aes(
      x = n,
      y = ci_value,
      group = ci_name,
      linetype = ci_name
    )) +
      ggplot2::geom_line() +
      ggplot2::geom_point(
        ggplot2::aes(
          x = n,
          y = ci_hl_p
        )
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = n,
          y = ci_hl_p,
          label = ci_hl_p_lab
        ),
        hjust = -0.1,
        vjust = -0.1
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = "Number of trials",
        y = "Probability of success",
        linetype = sprintf("CI (%.*f%%)", 2, 1 - alpha),
        title = sprintf("Confidence interval estimate given %.*f successes", 0, successes)

      ) +
      ggplot2::scale_linetype_manual(
        values = c("longdash", "solid"),
        labels = c("Upper bound", "Lower bound")
      )
  } else {
    NULL
  }
  return(
    setNames(
      list(data_ci, plot_ci), c("data_ci", "plot_ci")
    )
  )
}
