

# --- minimal n with desired power ---------------------------------------------

# optimize for n

simfun <- function(n) {

  # fixed prob
  prob = 0.1

  # simulate data
  successes <- rbinom(n = 1, size = n, prob = prob)

  # conduct binomial test
  out <- binom.test(
    x = successes,
    n = n,
    p = 0,
    conf.level = 0.1,
    alternative = "greater"
  )

  # test hypothesis
  p_value <- out$p.value
  p_value < 0.1
}

res <- mlpwr::find.design(
  simfun = simfun,
  boundaries = c(1,100),
  power = 0.90,
  surrogate = "logreg",
  integer = TRUE
)

summary(res)
plot(res)


# --- minimal detectable effect size with desired power ---------------------------------------------

# optimize for prob, keep n fixed

simfun <- function(prob) {

  # fixed sample size
  n = 50

    # simulate data
  successes <- rbinom(n = 1, size = n, prob = prob)

  # conduct binomial test
  out <- binom.test(
    x = successes,
    n = n,
    p = 0,
    conf.level = 0.1,
    alternative = "greater"
  )

  # test hypothesis
  p_value <- out$p.value
  p_value < 0.1
}

res <- mlpwr::find.design(
  simfun = simfun,
  boundaries = c(0.001,0.999),
  power = 0.90,
  surrogate = "gpr",
  integer = FALSE,
  ci = 0.001
)

summary(res)
plot(res)

