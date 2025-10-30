source("source/_functions/sim_logreg.R")
source("source/_functions/get_logreg_pars.R")

# set parameter values
fixed_effects <- get_logreg_pars(P_x0 = 0.2, P_x1 = 0.15)
fixed_effects$b0 <- -1.4
fixed_effects$b1 <- -0.2


# simulate data
data_y <- sim_logreg(
  b0 = fixed_effects$b0, fixed_effects$b1, x = seq(0,4), n = 100, u0_var = 0.5
)
# note: level-1 observations should also be high to get accurate estimates of the random effect variances

# fit model for a check
fit <- glm(y ~ x, family = binomial(link = "logit"), data = data_y)
fit <- lme4::glmer(formula = y ~ (1|id) + x, data = data_y, family = binomial(link = "logit"))
summary(fit)


# visualize: trajectories (observed & latent)
data_y |>
ggplot2::ggplot(
  data = _,
  mapping = ggplot2::aes(x = x, y = y, group = id)
) +
  ggplot2::geom_line(ggplot2::aes(y = psi), linetype = "longdash", linewidth = 0.5, alpha = .5) +
  ggplot2::geom_line(ggplot2::aes(color = id), linewidth = 1, alpha = .2,
                     position = ggplot2::position_jitter(w = 0.03, h = 0.03)) +
  ggplot2::labs(x = "year*", y = "outcome", color = "id") +
  ggplot2::scale_x_continuous(breaks = data_y$x |> unique()) +
  ggplot2::scale_colour_viridis_d(direction = -1, guide = "none") +
  ggplot2::theme_bw()


# visualize: overall frequencies
data_sum <- data_y |>
  dplyr::summarize(
    freq_obs = mean(y),
    .by = x
  )
ggplot2::ggplot(
  data = data_sum
) +
  ggplot2::geom_line(
    data = data_y,
    ggplot2::aes(x = x, y = psi, group = id),
    linetype = "longdash", linewidth = 0.5, alpha = .3) +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(x = x, xend = x, y = 0, yend = freq_obs),
    linewidth = 3
  ) +
  ggplot2::labs(x = "year*", y = "frequencies") +
  ggplot2::theme_bw()
