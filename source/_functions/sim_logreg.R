sim_logreg <- function(
    b0, # intercept
    b1, # slope
    x, # values of x
    n, # replications
    u0_var = 0, # variance random effect intercept
    u1_var = 0, # variance random effect slope
    u0u1_covar = 0, # covariance random effects
    test_sigma = FALSE # test whether sigma is valid covmatrix
){
  sigma <- matrix(c(u0_var, u0u1_covar, u0u1_covar, u1_var), nrow = 2)

  # test whether sigma is valid covariance matrix
  if (test_sigma){
    sigma_test <- try(matrixcalc::is.positive.definite(sigma))
    if (inherits(sigma_test, "try-error") || !sigma_test) {
      stop("covariance matrix of random effects not valid")
    }
  }

  # simulate data
  data_sim <- data.frame(
    b0 = b0,
    b1 = b1,
    x = x
  ) |>
    # add replications
    tidyr::crossing(
      id = seq(1,n) |> as.factor()
    ) |>
    dplyr::arrange(id) |>
    # draw random effects
    dplyr::mutate(
      tmp = MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = sigma) |> list(),
      .by = id
    ) |>
    tidyr::unnest_wider(tmp, names_sep = "_") |>
    dplyr::rename(u0_i = "tmp_1", u1_i = "tmp_2") |>
    # simulate eta and derivatives
    dplyr::mutate(
      eta =  (b0 + u0_i) + (b1 + u1_i)* x,
      odds = exp(eta),
      psi = odds / (1 + odds)
    ) |>
    # simulate y
    dplyr::rowwise() |>
    dplyr::mutate(
      y = rbinom(n = 1, size = 1, p = psi)
    ) |>
    dplyr::ungroup()
  return(data_sim)
}
