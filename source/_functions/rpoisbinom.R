# generate draws from the poisson binomial distribution
rpoisbinom <- function(
    I, # number of locations
    p_min = 0.3,
    p_max = 0.3,
    runif_seed = NULL,
    rbinom_seed = NULL
){
  if (!is.null(runif_seed)) set.seed(runif_seed)
  psi <- runif(n = I, min = p_min, max = p_max)
  if (!is.null(rbinom_seed)) set.seed(rbinom_seed)
  obs <- sapply(
    psi,
    \(x){
      rbinom(n = 1, size = 1, p = x)
    }
  )
  data.frame(
    psi = psi,
    y = obs
  ) |>
    dplyr::summarise(
      psi_mean = mean(psi),
      psi_var = var(psi),
      y_sum = sum(y),
      y_var = var(y)
    )
}
