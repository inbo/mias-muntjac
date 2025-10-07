# function written by Thierry
# returns probability of any event detected as deviating from h_0 under h_a
# = power
design_power <- function(
    h_a,
    h_0 = 0.05,
    n = 90, # number of trials
    alpha = 0.1,
    alternative = c("two.sided", "less", "greater")
) {
  alternative <- match.arg(alternative)
  # number of all potential success numbers ranging from 0 to n (all events)
  potential <- seq_len(n + 1) - 1
  # densities of potential successes under h_a (must sum to 1)
  dens <- dbinom(
    potential, # number of successes
    size = n, # number of trials
    prob = h_a # probability of success
  )
  # p-values all potential success numbers under h_0
  p <- sapply(
    potential,
    FUN = function(x, n, p, alternative) {
      binom.test(
        x, # number of successes
        n = n, # number of trials
        p = p, # h_0
        alternative = alternative
      )$p.value
    },
    n = n,
    p = h_0,
    alternative = alternative
  )
  # sum densities, for significant/detectable deviations from h0
  sum(dens[p <= alpha])
}
