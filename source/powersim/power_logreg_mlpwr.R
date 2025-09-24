
#?? https://cran.r-project.org/web/packages/pwrss/vignettes/examples.html#6_Regression:_Logistic_(Wald%E2%80%99s_Z-Test)

# --- minimal n with desired power ---------------------------------------------

# optimize for n

simfun <- function(n) {
  # simulate eta
  data_eta <- data.frame(
    # size of linear effect of year
    b0 = log(5), # intercept, event rate of 5 at t=0
    b1 = log(-50/100 + 1) # % decrease per year
  ) |>
    tidyr::crossing(
      year_star = seq(0,1)
    ) |>
    dplyr::mutate(
      eta =  b0 + b1 * year_star,
      mu = exp(eta)
    )

  # simulate y
  data_y <- data_eta |>
    tidyr::crossing(
      id = seq(1,n)
    ) |>
    dplyr::arrange(id) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      y = rpois(n = 1, lambda = mu)
    )

  # fit glm
  fit_pois <- glm(y ~ year_star, family = poisson, data = data_y)

  # test hypothesis
  p_value <- summary(fit_pois)$coefficients["year_star", "Pr(>|z|)"]
  p_value < 0.1
}

res <- mlpwr::find.design(
  simfun = simfun,
  boundaries = c(1*30, 100*30),
  power = 0.90
)

summary(res)
plot(res)

# n = 1744 for 1% per jaar over 5 jaar
# 1744/30 = 58 cameras per jaar


# beta0: log(2)
# perc_change: -50
# n_opt:

# beta0: log(5)
# perc_change: -50 % over 2 jaar
# looptijd 2 jaar
# n_opt:


# --- XXX ---------------------------------------------
