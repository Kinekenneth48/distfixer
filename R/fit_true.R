# =============================================================================#
# fit_true function(internal) declaration
# =============================================================================#
#' @title Fit true distribution
#' @description This internal function fits a log normal distribution to the
#' true response variable values.
#' @param train_data Training data of class data.frame.
#' @param direct_label Response/dependent/label variable name that is
#' predicted by the fitted model.
#' @param distr A character string that represent the distribution to fit for
#'  the label/response. Default is "lnorm" for normal distribution.
#'  See fitdistrplus::fitdist for string names for other distributions.
#' @param label_convert Logical variable indicating that the final
#' response/label variable for fitting the distribution should be changed
#' from direct_label to the indirect_label. Default is FALSE, where the
#' direct_label is considered. If TRUE, the predicted values from the
#' fitted model must be multiplied by the "multiplier" to get the estimated
#' indirect_label.
#' @param indirect_label Specify the actual indirect_label column name needed
#' for computing the true parameter values. Default: "snowload".
#' @param ... Other arguments to send to the distribution function.
#'  fitdistrplus::fitdist
#' @return Fitted distribution object
#' @rdname fit_true
#' @importFrom fitdistrplus fitdist
#' @importFrom stats sd
fit_true <- function(train_data, distr, direct_label, label_convert,
                     indirect_label, ...) {
  if (label_convert) {
    fit <- fitdistrplus::fitdist(
      data = train_data[[indirect_label]], distr = distr
    )
  } else {
    fit <- fitdistrplus::fitdist(
      data = train_data[[direct_label]], distr = distr
    )
  }

  # return object
  return(fit)
}
