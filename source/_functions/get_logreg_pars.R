get_logreg_pars <- function(
    P_x0 = NULL, # P(Y = 1|X = 0)
    P_x1 = NULL, # P(Y = 1|X = 1)
    n_digits = 2
){
  b0 <- if(is.null(P_x0)) {
    NULL
  } else {
    log(P_x0/(1 - P_x0)) |> round(x = _, digits = n_digits)
  }
  b1 <- if(is.null(P_x1)) {
    NULL
  } else {
    log(P_x1/(1 - P_x1)*(1 - P_x0)/P_x0) |> round(x = _, digits = n_digits)
  }
  setNames(list(b0, b1), c("b0", "b1"))
}




