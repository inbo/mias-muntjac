
# define sim function for sim_mlpwr

sim_mlpwr <- function(n, k, psi_h0, psi_ha, alpha) {

  # simulate data
  successes <- rbinom(n = k, size = n, prob = psi_ha)

  # conduct binomial test
  out <- binom.test(
    x = successes,
    n = n,
    p = psi_h0,
    conf.level = 1 - alpha,
    alternative = "greater"
  )

  # test hypothesis
  p_value <- out$p.value
  p_value < alpha
}


# --- minimal n with desired power ---------------------------------------------

# optimize for n

res <- mlpwr::find.design(
  simfun = \(n){
    sim_mlpwr(n = n, k = 1, psi_h0 = 0, psi_ha = 0.1, alpha = 0.1)
    },
  boundaries = c(1,100),
  power = 0.90,
  surrogate = "logreg",
  integer = TRUE
)

summary(res)
plot(res)


# --- minimal detectable effect size with desired power ---------------------------------------------

# optimize for prob, keep n fixed

res <- mlpwr::find.design(
  simfun = \(psi_ha){
    sim_mlpwr(n = 50, k = 1, psi_h0 = 0, psi_ha = psi_ha, alpha = 0.1)
  },
  boundaries = c(0.001,0.999),
  power = 0.90,
  surrogate = "gpr",
  integer = FALSE,
  ci = 0.001
)

summary(res)
plot(res)

