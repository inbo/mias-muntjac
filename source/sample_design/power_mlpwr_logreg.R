rm(list = ls())

# source function to simulate data from logistic regression
source("source/_functions/sim_logreg.R")
source("source/_functions/get_logreg_pars.R")

# --- define simulation function mlpwr ---------------------------------------------

sim_mlpwr <- function(
    b0, b1,
    u0_var, u1_var, u0u1_covar,
    x, n,
    alpha, fit_mixed = TRUE
    ) {

  # simulate data
  data_y <- sim_logreg(
    b0 = b0, b1 = b1,
    u0_var = u0_var, u1_var = u1_var, u0u1_covar = u0u1_covar,
    n = n, x = x
    )

  # fit glm
  fit_logi <- if (fit_mixed) {
    # random intercept
    lme4::glmer(
      formula = y ~ (1|id) + x,
      data = data_y,
      family = binomial(link = "logit")
    )
  } else {
    # simple model
    glm(y ~ x, family = binomial(link = "logit"), data = data_y)
  }

  # test hypothesis
  p_value <- summary(fit_logi)$coefficients["x", "Pr(>|z|)"]
  p_value < alpha

}

# --- ILLUSTRATION ---------------------------------------------

if (FALSE){

# --- set desired parameter values ---------------------------------------------

fixed_effects <- get_logreg_pars(P_x0 = 0.2, P_x1 = 0.15)
u0_var <- 0.3
u1_var <- 0
u0u1_covar <- 0
x <- seq(0, 4)
n <- 100

# --- minimal n with desired power ---------------------------------------------

# optimize for n
res <- mlpwr::find.design(
  simfun = \(n){
    sim_mlpwr(
      b0 = fixed_effects$b0, b1 = fixed_effects$b1,
      u0_var = u0_var, u1_var = u1_var, u0u1_covar = u0u1_covar,
      n = n, x = x,
      alpha = 0.1, fit_mixed = TRUE
      )
  },
  boundaries = c(10, 200), # very small values here cause problems
  power = 0.90
)

summary(res)
plot(res)

# --- minimal detectable effect size with desired power ---------------------------------------------

# optimize for b1, keep n fixed

res <- mlpwr::find.design(
  simfun = \(b1){
    sim_mlpwr(
      b0 = fixed_effects$b0, b1 = b1,
      u0_var = u0_var, u1_var = u1_var, u0u1_covar = u0u1_covar,
      n = n, x = x,
      alpha = 0.1, fit_mixed = TRUE
    )
  },
  boundaries = c(-1, 0), # very large values (steep decline) here cause problems
  power = 0.90,
  integer = FALSE
  #surrogate = "gpr",
  #ci = 0.001
)

summary(res)
plot(res)

# --- inspect results ---------------------------------------------

tmp <- mlpwr::simulations_data(res)

# manual plotting
ggplot2::ggplot(
  data = tmp,
  ggplot2::aes(x = b1, y = power)
  ) +
  ggplot2::geom_vline(xintercept = res$final$design[[1]]) +
  ggplot2::geom_hline(yintercept = 0.9) +
  ggplot2::geom_point(color = "red", alpha = .3) +
  ggplot2::geom_point(ggplot2::aes(y = power_surrogate), color = "blue", alpha = .3) +
  ggplot2::stat_smooth(
    method = lm, formula = y ~ poly(x, 3), se = FALSE,
    linetype = "dashed", col = "black", linewidth = 0.5
  ) +
  ggplot2::theme_bw()


# --- re-simulate power for optimal solution ---------------------------------------------

# minimal detectable effect size with desired power
n_rep <- 500
check <- purrr::map(
  seq(1,n_rep,1),
  \(replicate){
    # dont use sim_mlpwr directly
    sim_mlpwr(
      b0 = fixed_effects$b0,
      b1 = res$final$design[[1]],
      u0_var = u0_var,  u1_var = u1_var, u0u1_covar = u0u1_covar,
      n = n, x = seq(0,4),
      alpha = 0.1, fit_mixed = TRUE
    )
  }
) |>
  unlist()

# here: check individual modeling solutions
check_power <- mean(check)
check_power_se <- sqrt(check_power*(1-check_power)/n_rep)

}

# --- SIMULATION ---------------------------------------------

# the larger the sample size, the more difficult the search
#
# set simulation parameters
grid_pars <- data.frame(
  n = c(50, 75, 100, 125, 150, 175, 200, 225, 250)
) |>
  # set lower bound in function of design (important for surrogate model)
  dplyr::mutate(
    bound_l_tmp = - 1.3, #seq(-5, -0.2, length.out = 9),
    tmp = log(seq(1,600,length.out = 9)),
    tmp = (tmp - min(tmp)) / (ceiling(max(tmp)) + 0.5 - min(tmp)),
    tmp = 1 - tmp,
    bound_l_tmp = bound_l_tmp * tmp
) |>
  tidyr::crossing(
    b0 = purrr::map(
      c(0.5, 0.2),
      \(x) {
        get_logreg_pars(P_x0 = x)$b0
      }
    ) |> unlist(),
    x_end = c(6, 12),
    u0_var = c(0, 0.5),
    u1_var = 0,
    u0u1_covar = 0
  ) |>
  dplyr::arrange(
    u0_var, x_end, b0 |> dplyr::desc(), n
  ) |>
  dplyr::mutate(
    alpha = 0.10,
    power = 0.90,
    fit_mixed = ifelse(u0_var == 0, FALSE, TRUE),
    evaluations = 4000,
    surrogate = "logreg",
    bound_l = dplyr::case_when(
      (n < 125 & b0 == min(b0) & x_end == min(x_end)) ~ bound_l_tmp * 1.2,
      (n < 125 & b0 == max(b0) & x_end == max(x_end)) ~ bound_l_tmp * 0.4,
      (n < 125 & b0 == min(b0) & x_end == max(x_end)) ~ bound_l_tmp * 0.6,
      TRUE ~ bound_l_tmp
    ),
    index = dplyr::row_number()
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    boundaries = list(c(bound_l, 0)),
    .after = bound_l
  ) |>
  dplyr::ungroup() |>
  dplyr::relocate(
    tmp,
    bound_l_tmp,
    .before = bound_l
  ) |>
  dplyr::relocate(
    index,
    .before = 1
  )
# test
if (FALSE) {
  grid_pars <- grid_pars |>
    dplyr::slice_head(n = 2) |>
    dplyr::mutate(evaluations = 400)
  }

# find designresult objects
indices <- grid_pars$index # full matrix
#indices <- grid_pars |> dplyr::filter(n < 150 | n > 225) |> dplyr::pull(index)

res_list <- purrr::map(
  indices,
  \(i) {
    sprintf(
      "parameter combination %i of %i",
      which(indices %in% i),
      length(indices)
      ) |>
      print()
    set.seed(i)
    try(
      mlpwr::find.design(
        simfun = \(b1){
          sim_mlpwr(
            b1 = b1,
            b0 = grid_pars$b0[i],
            u0_var = grid_pars$u0_var[i],
            u1_var = grid_pars$u1_var[i],
            u0u1_covar = grid_pars$u0u1_covar[i],
            n = grid_pars$n[i],
            x = seq(0, grid_pars$x_end[i]),
            alpha = grid_pars$alpha[i],
            fit_mixed = grid_pars$fit_mixed[i]
          )
        },
        boundaries = grid_pars$boundaries[i] |> unlist(),
        power = grid_pars$power[i],
        evaluations = grid_pars$evaluations[i],
        integer = FALSE,
        surrogate = grid_pars$surrogate[i],
        ci = if (grid_pars$surrogate[i] == "logreg") {NULL} else {0.001}
      )
    )
  }
)

# update grid_pars
grid_pars <- grid_pars[indices,]

# save designresult objects
datetime <- format(Sys.time(), "%Y%m%d-%H%M%S")
save(res_list, file = paste0("output/sample_design/mlpwr_logreg/res_list_", datetime ,".Rda")) # _index1-36
save(grid_pars, file = paste0("output/sample_design/mlpwr_logreg/gridpars_", datetime ,".Rda"))



