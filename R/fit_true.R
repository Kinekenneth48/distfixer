# =============================================================================#
# fit_true function(internal) declaration
# =============================================================================#
#' @title Fit true distribution
#' @description This internal function fits a log normal distribution to the
#' true response variable values.
#' @param train_data Training data of class data.frame.
#' @param label Response/dependent variable name in the train_data.
#' @param snowload Logical variable indicating that the final response variable
#'  for fitting the distribution is snowload. In this case, the initial response
#'  variable(actual/predicted) is multiplied against snow depth. Default is
#'  TRUE.  When FALSE, initial response variable (snowload is not computed) for
#'  distribution fitting.
#' @param snowload_col Specify the snowload column name needed for computing
#'  the true parameter values. Default: "snowload"
#' @return Fitted distribution object
#' @rdname fit_true
#' @importFrom fitdistrplus fitdist
#' @importFrom stats sd
fit_true <- function(train_data, label, snowload, snowload_col) {
  if (snowload) {
    fit <- fitdistrplus::fitdist(
      data = train_data[[snowload_col]], distr = "lnorm",
      start = list(
        meanlog = mean(log(train_data[[snowload_col]])),
        sdlog = stats::sd(log(train_data[[snowload_col]]))
      ),
      method = "mle"
    )
  } else {
    fit <- fitdistrplus::fitdist(
      data = train_data[[label]], distr = "lnorm",
      start = list(
        meanlog = mean(log(train_data[[label]])),
        sdlog = stats::sd(log(train_data[[label]]))
      ),
      method = "mle"
    )
  }

  return(fit)
}
