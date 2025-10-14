# clopper-pearson interval upper bound for 0 successes
# can be used to get the minimum detectable deviation from h_0: psi = 0, if alpha = beta
get_ciu_k0 <- function(
    nl,
    conf_level, # power if alpha = beta
    one_sided = TRUE
    ) {
  if (one_sided) {
    1 - ((1 - conf_level)^(1/nl))
  } else {
    1 - (((1 - conf_level)/2)^(1/nl))
  }
}

# number of locations for a certain upper bound / mdd
get_nl <- function(
    ciu, # mdd if alpha = beta
    conf_level, # power if alpha = beta
    one_sided = TRUE
    ) {
  tmp <-   if (one_sided) {
    log(1 - conf_level)/log(1 - ciu)
  } else {
    log((1 - conf_level)/2)/log(1 - ciu)
  }
  tmp |> ceiling()
}

message("functions 'get_ciu_k0' and 'get_nl' sourced")
