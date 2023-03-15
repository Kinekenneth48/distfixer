# =============================================================================#
# predict_param function(external) declaration
# =============================================================================#
#' @title Predict unbiased log normal distribution parameters
#' @description This function predicts an unbiased  log normal distribution
#' parameters of the response variable for a given set of predictor variables.
#' @param test_data Test data of class data.frame.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param direct_label Response/dependent/label variable name that is
#' predicted by the fitted model.
#' @param distr A character string that represent the distribution to fit for
#'  the label/response. Default is "lnorm" for normal distribution.
#'  See fitdistrplus::fitdist for string names for other distributions.
#' @param all.missing Logical variable indicating whether the all or some
#' of the response variable is missing. Default: TRUE
#' @param label_convert Logical variable indicating that the final
#' response/label variable for fitting the distribution should be changed
#' from direct_label to the indirect_label. Default is FALSE, where the
#' direct_label is considered. If TRUE, the predicted values from the
#' fitted model must be multiplied by the "multiplier" to get the estimated
#' indirect_label.
#' @param multiplier Specify the multiplier column needed to compute the
#'  indirect_label quantity. Default: "snowdepth".
#' @param mean Mean of the error distribution. Distribution is normal.
#' @param sd Standard deviation of the error distribution. Distribution
#' is normal.
#' @param percentile Percentile of  bootstrap distribution parameter to get,
#' Default: 0.9.
#' @param nboot Number of bootstraps to run before getting the distribution
#' parameters, Default: 200.
#' @param indirect_label Specify the actual indirect_label column name needed
#' for computing the true parameter values. Default: "snowload".
#' @param param_adjust A character string that represent the distribution
#' parameter that needs adjustment. Default is "sdlog" from the log normal
#' distribution. See fitdistrplus::fitdist for string
#' names for other distribution parameters.
#' @param ... Other arguments to send to the distribution function
#'  fitdistrplus::fitdist
#' @return Log normal distribution parameters.
#' @rdname predict_param
#' @export
#' @importFrom stats rnorm quantile predict
predict_param <- function(test_data, direct_label, fitted_model, distr = "lnorm",
                          label_convert = FALSE, all.missing = TRUE,
                          multiplier = "snowdepth", indirect_label = "snowload",
                          param_adjust = "sdlog",
                          mean = 0, sd = 1, percentile = 0.9,
                          nboot = 200, ...) {
  if (all.missing) {
    # compute the bootstrap of parameters
    params_matrix <- boot_sample_all_missing(
      test_data, fitted_model, distr, mean, sd, nboot, label_convert, multiplier
    )
  } else {
    params_matrix <- boot_sample_some_missing(
      test_data, direct_label, indirect_label, fitted_model, mean, sd, distr,
      nboot, label_convert, multiplier
    )
  }


  # get the unbiased parameter
  quantile_value <- quantile(params_matrix[, param_adjust],
    probs = percentile,
    na.rm = TRUE
  )

  diffs <- abs(params_matrix[, param_adjust] - quantile_value)
  closest_row_position <- which.min(diffs)

  result <- params_matrix[closest_row_position, ]


  return(result)
}
