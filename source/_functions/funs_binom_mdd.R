# functions written by Thierry

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

# finds value of h_a for which (design_power - power) assumes zero
# actual power = desired power
# = finds minimal detectable deviation with desired power
# = finds value of ha for which the supplied values hold
find_ha <- function(
    power = 0.9,
    h_0 = 0.1,
    n = 90,
    alpha = 0.1,
    alternative = c("two.sided", "less", "greater"),
    lower = FALSE # search for h_a in range 0 - h_a (if TRUE) OR h_a - 1 (if FALSE)
) {
  alternative <- match.arg(alternative)
  if (alternative == "less" || (alternative == "two.sided" && lower)) {
    lower <- 0
    upper <- h_0
  } else {
    lower <- h_0
    upper <- 1
  }
  result <- try(
    # error in case actual power never reaches desired,
    # then no minimal detectable effect
    uniroot(
      function(h_a) {
        design_power(
          h_a = h_a,
          h_0 = h_0,
          n = n,
          alpha = alpha,
          alternative = alternative
        ) -
          power
      },
      lower = lower,
      upper = upper
    ),
    silent = TRUE
  )
  if (inherits(result, "try-error")) {
    return(NA)
  }
  result$root
}

message("functions 'design_power' and 'find_ha' sourced")
