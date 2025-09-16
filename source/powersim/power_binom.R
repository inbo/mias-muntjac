library(tidyverse)
library(scales)
library(effectclass)

# --------------------------------------------------------------------------
# --- define functions -----------------------------------------------------
# --------------------------------------------------------------------------

# returns probability of any event detected as deviating from h_0 under h_a
# = actual power
design_power <- function(
    h_a,
    h_0 = 0.05,
    n = 90,
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
# probability of any event detected as deviating from h_0 under h_a equals desired power exactly
# actual power = desired power
# = finds minimal detectable deviation with desired power
# = finds value of ha for which the supplied values hold
find_ha <- function(
    power = 0.9,
    h_0 = 0.1,
    n = 90,
    alpha = 0.1,
    alternative = c("two.sided", "less", "greater"),
    lower = TRUE
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

batch_size <- function(
    n_batch = 100, # max number of trials
    n_rat = 1,
    h_0 = 0.05,
    alpha = 0.1,
    alternative = c("two.sided", "less", "greater"),
    power = 0.9,
    n_test = 1,
    lower = TRUE
) {
  alternative <- match.arg(alternative)
  data.frame(
    n_batch = seq_len(n_batch),
    n_rat = n_rat,
    h_0 = h_0,
    h_a = sapply(
      seq_len(n_batch) * n_rat,
      FUN = find_ha,
      h_0 = h_0,
      alpha = 1 - (1 - alpha)^(1 / n_test),
      alternative = alternative,
      power = power,
      lower = lower
    ),
    alternative = alternative,
    alpha = alpha,
    n_test = n_test,
    power = power,
    lower = lower
  )
}

# --------------------------------------------------------------------------
# --- analyse power -----------------------------------------------------
# --------------------------------------------------------------------------

args_batch_size <- list(
  n_batch = 100,
  n_rat = 1,
  h_0 = 0,
  alpha = 0.1,
  power = 0.9,
  n_test = 1,
  lower = FALSE,
  alternative = "greater"
)

powers <- do.call(
  batch_size,
  args_batch_size
) |>
  bind_rows(
    do.call(
      batch_size,
      args_batch_size |>
        purrr::assign_in("h_0", 0.05) |>
        purrr::assign_in("alternative", "two.sided")
    ) ,
    do.call(
      batch_size,
      args_batch_size |>
        purrr::assign_in("h_0", 0.05) |>
        purrr::assign_in("alternative", "two.sided") |>
        purrr::assign_in("lower", TRUE)
    )
  ) |>
  mutate(label_h0 = sprintf("Target value <= %.2f", h_0))
plot_power <- ggplot(powers, aes(x = n_batch, y = h_a, color = alternative, group = lower)) +
  geom_hline(aes(yintercept = h_0), linetype = 2) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, NA)) + #labels = percent
  facet_wrap(~ label_h0) +
  labs(
    x = "Number of sampling locations",
    y = "Probability of success",
    title = "Minimal detectable deviation from target",
    color = "Testing"
  ) +
  theme_bw()
plot_power

# --------------------------------------------------------------------------
# --- effectclass -----------------------------------------------------
# --------------------------------------------------------------------------

# point plot
if (FALSE){
  powers |>
    filter(!is.na(.data$h_a)) |>
    mutate(
      size = .data$n_batch * .data$n_rat,
      success = map2(.data$h_0, .data$size, ~ seq_len(1 + floor(.x * .y)) - 1)
    ) |>
    unnest("success") |>
    mutate(
      bt = pmap(
        list(
          x = .data$success,
          n = .data$size,
          p = .data$h_0,
          alternative = .data$alternative
        ),
        binom.test
      ),
      p_value = 1 - (1 - map_dbl(.data$bt, "p.value"))^.data$n_test,
      significant = .data$p_value <= .data$alpha,
      lcl = map(.data$bt, "conf.int"),
      ucl = map_dbl(.data$lcl, ~ .x[2]),
      lcl = map_dbl(.data$lcl, ~ .x[1]),
      threshold = ifelse(
        .data$alternative == "less",
        1,
        2 * .data$h_0 - .data$h_a
      ) |>
        map2(.data$h_a, ~ c(.y, .x)),
      interpretation = pmap(
        list(
          lcl = .data$lcl,
          ucl = .data$ucl,
          threshold = .data$threshold,
          reference = .data$h_0
        ),
        classification
      ) |>
        unlist(),
      fraction = .data$success / .data$size
    ) |>
    filter(.data$alternative == "two.sided") |>
    ggplot(aes(x = n_batch, y = fraction)) +
    geom_hline(aes(yintercept = h_0), linetype = 2) +
    geom_point(size = 4, aes(colour = interpretation)) +
    geom_line(
      data = filter(powers, .data$alternative == "two.sided"),
      aes(y = h_a),
      linewidth = 1
    ) +
    scale_y_continuous("observed effect", limits = c(0, NA), labels = percent) +
    facet_wrap(~h_0) +
    scale_colour_manual(
      values = effectclass:::detailed_signed_palette,
      name = "Interpretation",
      labels = c(
        "++" = "strongly over\nreference",
        "+" = "over\nreference",
        "+~" = "moderately over\nreference",
        "~" = "around\nreference",
        "-~" = "moderately under\nreference",
        "-" = "under\nreference",
        "--" = "strongly under\nreference",
        "?+" = "potentially over\nreference",
        "?-" = "potentially under\nreference",
        "?" = "unclear"
      )
    )
}

# polygon-plot, two-sided
effect_size <- do.call(
  batch_size,
  args_batch_size |>
    purrr::assign_in("lower", TRUE) |>
    purrr::assign_in("h_0", 0.05) |>
    purrr::assign_in("alternative", "two.sided")
) |>
  select(
    "n_batch",
    "n_rat",
    "h_0",
    lower = "h_a", # use h_a as lower threshold
    "n_test",
    "alpha",
    "alternative"
  ) |>
  dplyr::filter(!is.na(.data$lower)) |>
  inner_join(
    do.call(
      batch_size,
      args_batch_size |>
        purrr::assign_in("lower", FALSE) |>
        purrr::assign_in("h_0", 0.05) |>
        purrr::assign_in("alternative", "two.sided")
    ) |>
      select(
        "n_batch",
        "n_rat",
        "h_0",
        upper = "h_a", # use h_a as upper threshold
        "n_test",
        "alpha",
        "alternative"
      ) |>
      dplyr::filter(!is.na(.data$upper)),
    by = c("n_batch", "n_rat", "n_test", "alpha", "h_0", "alternative")
  )

results <- effect_size |>
  mutate(
    size = .data$n_batch * .data$n_rat,
    success = map(.data$size, ~ seq_len(.x + 1) - 1)
  ) |>
  unnest("success") |>
  mutate(
    bt = pmap(
      list(
        x = .data$success,
        n = .data$size,
        p = .data$h_0,
        alternative = .data$alternative,
        conf.level = 1 - .data$alpha
      ),
      binom.test
    ),
    p_value = map_dbl(.data$bt, "p.value"),
    significant = .data$p_value <= .data$alpha,
    lcl = map(.data$bt, "conf.int"),
    ucl = map_dbl(.data$lcl, ~ .x[2]),
    lcl = map_dbl(.data$lcl, ~ .x[1]),
    threshold = map2(.data$lower, .data$upper, c),
    interpretation = pmap(
      list(
        lcl = .data$lcl,
        ucl = .data$ucl,
        threshold = .data$threshold,
        reference = .data$h_0
      ),
      classification
    ) |>
      unlist(),
    fraction = .data$success / .data$size
  )

plot_classification <- results |>
  slice_min(
    .data$fraction,
    n = 1,
    by = c("h_0", "n_batch", "interpretation")
  ) |>
  select("h_0", "n_batch", "interpretation", "fraction") |>
  mutate(direction = 1) |>
  bind_rows(
    results |>
      slice_max(
        .data$fraction,
        n = 1,
        by = c("h_0", "n_batch", "interpretation")
      ) |>
      select("h_0", "n_batch", "interpretation", "fraction") |>
      mutate(direction = -1)
  ) |>
  arrange(.data$direction * .data$n_batch) |>
  mutate(label = sprintf("Target value <= %.2f", .data$h_0)) |>
  ggplot(aes(x = n_batch, y = fraction)) +
  geom_polygon(aes(fill = interpretation)) +
  geom_hline(aes(yintercept = h_0), linetype = 2) +
  facet_wrap(~label) +
  labs(
    x = "Number of sampling locations",
    y = "Probability of success", # (observed),
    title = "Classification of observable effects with respect to target and minimal detectable effect"
  ) +
  theme_bw() +
  scale_fill_manual(
    values = effectclass:::detailed_signed_palette,
    name = "Interpretation",
    labels = c(
      "++" = "strongly over\ntarget",
      "+" = "over\ntarget",
      "+~" = "moderately over\ntarget",
      "~" = "around\ntarget",
      "-~" = "moderately under\ntarget",
      "-" = "under\ntarget",
      "--" = "strongly under\ntarget",
      "?+" = "potentially over\ntarget",
      "?-" = "potentially under\ntarget",
      "?" = "unclear"
    )
  )
plot_classification
