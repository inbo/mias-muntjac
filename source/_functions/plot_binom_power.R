plot_binom_power <- function(
    n = 100, # number of trials
    psi_0 = 0.05, # probability of succes under h_0
    psi_a = 0.06, # probability of succes under h_a, or vector if multiple values
    alpha = 0.05 # type 1 error
){
  successes <- seq_len(n + 1) - 1 # all potential events for full probability mass distribution

  data_power <- dplyr::bind_rows(
    data.frame(
      n,
      successes
    ) |> tidyr::crossing(
      data.frame(
      psi = psi_a,
      hypothesis = sprintf("h_a%i", seq_along(psi_a))
      )
    ),
    data.frame(
      n,
      successes,
      hypothesis = "h_0",
      psi = psi_0
    )
  ) |>
    dplyr::mutate(
      probmass = dbinom(
        x = successes,
        size = n,
        prob = psi
      ),
      quant_h0 = qbinom(
        p = alpha,
        size = n,
        prob = psi_0,
        lower.tail = FALSE # P(X > x)
      ),
      power = dplyr::case_when(
        grepl("h_a", hypothesis) ~ pbinom(
        q = quant_h0,
        size = n,
        prob = psi,
        lower.tail = FALSE # P(X > x)
      )
      ),
      decision = dplyr::case_when(
        successes > quant_h0 ~ "reject h_0",
        successes <= quant_h0 ~ "retain h_0"
      )
    ) |>
    #dplyr::mutate(
    #  power_check = dplyr::case_when(decision == "reject h_0" ~ sum(probmass)),
    #  .by = c(hypothesis, decision)
    #) |>
    dplyr::mutate(
      decision = paste0(decision, " (alpha = ",alpha,")"),
      lab_hypothesis = dplyr::case_when(
        !is.na(power) ~ paste0(hypothesis, " (psi = ", psi, "; power \u2248 ", round(power, 4), ")"),
        is.na(power) ~ paste0(hypothesis, " (psi = ", psi, ")")
      )
    )

  plot_power <- ggplot2::ggplot(
    data = data_power,
    ggplot2::aes(
      x = successes,
      y = probmass,
      group = hypothesis
    )) +
    ggplot2::geom_vline(xintercept = data_power$quant_h0 + 1) +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = 0,
        yend = probmass,
        color = decision
      ),
      linewidth = 1.1
    ) +
    ggplot2::geom_line(linetype = "dashed") +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::xlim(c(0,n)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of successes",
      y = "Probability"
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(lab_hypothesis),
      ncol = 1
    )

  return(
    setNames(
      list(data_power, plot_power), c("data_power", "plot_power")
    )
  )

  if (FALSE){
    p <- binom.test(
      x = successes[10],
      n = n, # number of trials
      p = psi_0, # h_0
      alternative = "greater"
    )$p.value

    qbinom(
      p = p,
      size = n,
      prob = psi_0,
      lower.tail = FALSE
    )
  }
}


