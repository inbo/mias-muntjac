rm(list = ls())
source("source/_functions/design_power.R")

# ---- definitions -----------------------------------------------

# behavior of clopper-pearson interval for binomial proportions
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval


# args
successes = 5
n = 20
psi_0 = 0.1
alpha = 0.1
alternative = "two.sided"

# binomial test
out <- binom.test(
  x = successes,
  n = n,
  p = psi_0,
  conf.level = 1 - alpha,
  alternative = alternative
)
out$conf.int

# calculation of CI via quantiles of beta distribition
ci_beta <- c(
  qbeta(
    p = alpha/2,
    shape1 = successes,
    shape2 = n - successes + 1,
    ncp = 0,
    lower.tail = TRUE,
    log.p = FALSE
  ),
  qbeta(
    p = 1 - alpha/2,
    shape1 = successes + 1,
    shape2 = n - successes,
    ncp = 0,
    lower.tail = TRUE,
    log.p = FALSE
  )
)
assertthat::are_equal(ci_beta[1], out$conf.int[1])
assertthat::are_equal(ci_beta[2], out$conf.int[2])


# ---- 0 successes, two-sided-----------------------------------------------

# binomial test
out_0 <- binom.test(
  x = 0,
  n = n,
  p = psi_0,
  conf.level = 1 - alpha,
  alternative = alternative
)
out_0$conf.int

# closed form expression two-sided
ci_man_0 <- c(
  0,
  1 - ((alpha/2)^(1/n))
  )
assertthat::are_equal(ci_man_0[1], out_0$conf.int[1])
assertthat::are_equal(ci_man_0[2], out_0$conf.int[2])

# quantile function beta distribution
ci_beta_0 <- c(
  qbeta(
    p = alpha/2,
    shape1 = 0,
    shape2 = n - 1,
    ncp = 0,
    lower.tail = TRUE,
    log.p = FALSE
  ),
  qbeta(
    p = 1 - alpha/2,
    shape1 = 1,
    shape2 = n,
    ncp = 0,
    lower.tail = TRUE,
    log.p = FALSE
  )
)
assertthat::are_equal(ci_beta_0[1], out_0$conf.int[1]) # ci_beta_0[1] is NA
assertthat::are_equal(ci_beta_0[2], out_0$conf.int[2])

pbinom(q = ci_man_0[1], size = n, prob = 0)
pbinom(q = ci_man_0[2], size = n, prob = 0, lower.tail = FALSE)

# ---- 0 successes, one-sided-----------------------------------------------

# binomial test
out_0 <- binom.test(
  x = 0,
  n = n,
  p = psi_0,
  conf.level = 1 - alpha,
  alternative = "less" # "greater" gives CI(x,1), which does not make sense
)
out_0$conf.int

# closed form expression one-sided
ci_man_0 <- c(
  0,
  1 - ((alpha)^(1/n))
)
assertthat::are_equal(ci_man_0[1], out_0$conf.int[1])
assertthat::are_equal(ci_man_0[2], out_0$conf.int[2])

# quantile function beta distribution (ref. Cai 05)
ci_beta_0 <- c(
  qbeta(
    p = alpha,
    shape1 = 0,
    shape2 = n - 1,
    ncp = 0,
    lower.tail = TRUE,
    log.p = FALSE
  ),
  qbeta(
    p = 1 - alpha,
    shape1 = 1,
    shape2 = n,
    ncp = 0,
    lower.tail = TRUE,
    log.p = FALSE
  )
)
assertthat::are_equal(ci_beta_0[1], out_0$conf.int[1]) # ci_beta_0[1] is NA
assertthat::are_equal(ci_beta_0[2], out_0$conf.int[2])


# ---- 0 successes, onde sided, show that alpha ~= beta -----------------------------------------------


# 95%-percentile under h_0
qbinom(
  p = 1 - alpha,
  size = n,
  prob = 0
)

# 5%-percentile under h_a with psi = CI upper bound
qbinom(
  p = alpha,
  size = n,
  prob = ci_man_0[2]
)

# next possible* percentile under h_a > 95%-percentile under h_0
# *based on observable events
qbinom(
  p = alpha,
  size = n,
  prob = ceiling(n * ci_man_0[2])/n
)

# beta (power = 1 - beta)
pbinom(
  q = 0, # number of successes
  size= n,
  prob = ci_man_0[2]
)

# beta (Thierry's function)
1 - design_power(
    h_a = ci_man_0[2],
    h_0 = 0,
    n = n,
    alpha = alpha,
    alternative = c("greater")
)


test_CI_mdd <- function(
    alpha,
    n
){
  ci_upper <- 1 - ((alpha)^(1/n)) # one-sided
  beta <- pbinom(
    q =   qbinom(p = alpha, size = n, prob = 0),
    size= n,
    prob = ci_upper
  )
  beta_test <- 1 - design_power(
    h_a = ci_upper,
    h_0 = 0,
    n = n,
    alpha = alpha,
    alternative = c("greater")
  )
  q_h0 <- qbinom(p = 1 - alpha, size = n, prob = 0)
  q_ha <- qbinom(p = alpha, size = n, prob = ci_upper)
  psi_next <- ceiling(n * ci_upper)/n
  psi_prev <- floor(n * ci_upper)/n
  q_ha_next <- qbinom(p = alpha, size = n, prob = psi_next)
  q_ha_prev <- qbinom(p = alpha, size = n, prob = psi_prev)
  beta_next <- pbinom(
    q =   qbinom(p = alpha, size = n, prob = 0),
    size= n,
    prob = psi_next
  )
  beta_prev <- pbinom(
    q =   qbinom(p = alpha, size = n, prob = 0),
    size= n,
    prob = psi_prev
  )
  data.frame(
    n = n,
    alpha = alpha,
    beta = beta,
    beta_test = beta_test,
    beta_next = beta_next,
    beta_prev,
    diff_alpha_beta = alpha - beta,
    diff_alpha_beta_test = alpha - beta_test,
    diff_alpha_beta_next = alpha - beta_next,
    diff_alpha_beta_prev = alpha - beta_prev,
    q_h0 = q_h0,
    q_ha = q_ha,
    q_ha_next = q_ha_next,
    diff_q_h0_ha = q_ha - q_h0,
    diff_q_ha_next = q_ha_next - q_ha,
    ci_upper = ci_upper,
    psi_next = psi_next,
    conf_level = 1 - alpha,
    power = 1 - beta,
    power_next = 1 - beta_next,
    power_prev = 1 - beta_prev
  )

}

test <- data.frame(
  n_in = seq_len(100)
) |> tidyr::crossing(
  alpha_in = seq(1,10)/100
) |>
  dplyr::mutate(
    tmp = purrr::pmap(
      list(
        n = .data$n_in,
        alpha = .data$alpha_in
      ),
      test_CI_mdd
    )
  ) |>
  tidyr::unnest(tmp)

if (FALSE){
# should be very close to 0
test$diff_alpha_beta |> range()
test$diff_alpha_beta_test |> range()
test$diff_alpha_beta |> hist()
test$diff_alpha_beta_test |> hist()
# not necessarily close to 0 / should involve larger differences
test$diff_alpha_beta_next |> range()
test$diff_alpha_beta_prev |> range()
test$diff_alpha_beta_next |> hist()
test$diff_alpha_beta_prev |> hist()
}

# visualize
test |>
  dplyr::mutate(
    alpha_lab = sprintf("alpha = %.2f", alpha)
  ) |>
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("power"),
    names_to = "power_type",
    values_to = "power_value"
  ) |>
ggplot2::ggplot(data = _) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = n, y = conf_level),
    size = 1
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = n, y = power_value, color = power_type),
    linetype = "dashed",
    size = 1
  ) +
  ggplot2::facet_wrap(facets = ggplot2::vars(alpha_lab)) +
  ggplot2::coord_cartesian(ylim = c((min(test$conf_level) - 0.05), 1)) +
  ggplot2::theme_bw()



