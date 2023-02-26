# =============================================================================#
# predict_param function(external) declaration
# =============================================================================#
#' @title Predict log normal distribution parameters
#' @description This function predicts the log normal distribution parameters of
#'  of the response variable for a given set of predictor variables.
#' @param test_data Test data of class data.frame.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param snowload Logical variable indicating that the final response variable
#'  for fitting the distribution is snowload. In this case, the initial response
#'  variable(actual/predicted) is multiplied against snow depth. Default is
#'  TRUE.  When FALSE, initial response variable (snowload is not computed) for
#'  distribution fitting.
#' @param snowdepth_col Specify the snow depth column needed to compute the
#'  snowload quantity. Default: "snowdepth".
#' @param snowload_col Specify the snowload column name needed for computing
#'  the true parameter values. Default: "snowload".
#' @param mean Mean of the normal distribution of fitted model errors,
#'  Default: 0.
#' @param sd Standard deviation of the normal distribution of fitted model
#'  errors, Default: 1.
#' @param percentile Percentile of  bootstrap distribution parameter to get,
#' Default: 0.9.
#' @param nboot Number of bootstraps to run before getting the distribution
#' parameters, Default: 200.
#' @return Log normal distribution parameters.
#' @rdname predict_param
#' @export
#' @importFrom stats rnorm quantile predict
predict_param <- function(test_data, fitted_model, snowload = TRUE,
                          snowdepth_col = snowdepth, snowload_col = snowload,
                          mean = 0, sd = 1, percentile = 0.9,
                          nboot = 200) {
  predictions <- predict(fitted_model, test_data)

  lnorm_params_matrix <- boot_sample_test(
    test_data, nboot, mean, sd,
    predictions, snowload, snowdepth_col
  )



  quantile_value <- quantile(lnorm_params_matrix[, 2],
    probs = percentile,
    na.rm = TRUE
  )

  diffs <- abs(lnorm_params_matrix[, 2] - quantile_value)
  closest_row_position <- which.min(diffs)

  result <- lnorm_params_matrix[closest_row_position, ]


  return(result)
}
