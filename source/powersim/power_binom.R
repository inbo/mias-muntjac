library(tidyverse)
library(scales)
library(effectclass)

design_power <- function(
  h_a,
  h_0 = 0.05,
  n = 90,
  alpha = 0.1,
  alternative = c("two.sided", "less", "greater")
) {
  alternative <- match.arg(alternative)
  potential <- seq_len(n + 1) - 1
  dens <- dbinom(potential, size = n, prob = h_a)
  p <- sapply(
    potential,
    FUN = function(x, n, p, alternative) {
      binom.test(x, n = n, p = p, alternative = alternative)$p.value
    },
    n = n,
    p = h_0,
    alternative = alternative
  )
  sum(dens[p <= alpha])
}

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
    )
  )
  if (inherits(result, "try-error")) {
    return(NA)
  }
  result$root
}

batch_size <- function(
  n_batch = 20,
  n_rat = 18,
  h_0 = 0.05,
  alpha = 0.1,
  alternative = c("two.sided", "less", "greater"),
  power = 0.9,
  n_test = 3,
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

batch_size() |>
  bind_rows(
    batch_size(alternative = "less"),
    batch_size(alternative = "less", h_0 = 0.1),
    batch_size(h_0 = 0.1)
  ) -> powers
ggplot(powers, aes(x = n_batch, y = h_a, colour = alternative)) +
  geom_hline(aes(yintercept = h_0), linetype = 2) +
  geom_line() +
  geom_point() +
  scale_y_continuous("detectable effect", limits = c(0, NA), labels = percent) +
  facet_wrap(~h_0)

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

batch_size(h_0 = 0.95) |>
  select(
    "n_batch",
    "n_rat",
    "h_0",
    lower = "h_a",
    "n_test",
    "alpha",
    "alternative"
  ) |>
  filter(!is.na(.data$lower)) |>
  inner_join(
    batch_size(h_0 = 0.95, lower = FALSE) |>
      select(
        "n_batch",
        "n_rat",
        "h_0",
        upper = "h_a",
        "n_test",
        "alpha",
        "alternative"
      ) |>
      filter(!is.na(.data$upper)),
    by = c("n_batch", "n_rat", "n_test", "alpha", "h_0", "alternative")
  ) |>
  bind_rows(
    batch_size(h_0 = 0.9) |>
      select(
        "n_batch",
        "n_rat",
        "h_0",
        lower = "h_a",
        "n_test",
        "alpha",
        "alternative"
      ) |>
      filter(!is.na(.data$lower)) |>
      inner_join(
        batch_size(h_0 = 0.9, lower = FALSE) |>
          select(
            "n_batch",
            "n_rat",
            "h_0",
            upper = "h_a",
            "n_test",
            "alpha",
            "alternative"
          ) |>
          filter(!is.na(.data$upper)),
        by = c("n_batch", "n_rat", "n_test", "alpha", "h_0", "alternative")
      ),
    batch_size(h_0 = 0.5) |>
      select(
        "n_batch",
        "n_rat",
        "h_0",
        lower = "h_a",
        "n_test",
        "alpha",
        "alternative"
      ) |>
      filter(!is.na(.data$lower)) |>
      inner_join(
        batch_size(h_0 = 0.5, lower = FALSE) |>
          select(
            "n_batch",
            "n_rat",
            "h_0",
            upper = "h_a",
            "n_test",
            "alpha",
            "alternative"
          ) |>
          filter(!is.na(.data$upper)),
        by = c("n_batch", "n_rat", "n_test", "alpha", "h_0", "alternative")
      )
  ) -> effect_size
effect_size |>
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
        conf.level = 0.9^(1 / .data$n_test)
      ),
      binom.test
    ),
    p_value = 1 - (1 - map_dbl(.data$bt, "p.value"))^3,
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
  ) -> results
results |>
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
  mutate(label = sprintf("target >= %.0f%%", 100 * .data$h_0)) |>
  ggplot(aes(x = n_batch, y = fraction)) +
  geom_polygon(aes(fill = interpretation)) +
  geom_hline(aes(yintercept = h_0), linetype = 2) +
  facet_wrap(~label) +
  scale_x_continuous("number of batches of 18 rats") +
  scale_y_continuous("fraction rodent resistant", labels = percent) +
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
