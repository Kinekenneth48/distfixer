# =============================================================================#
# best_percentile function(external) declaration
# =============================================================================#
#' @title Best Percentile
#' @description This function finds the best percentile out of the bootstraps of
#'  scale parameters that minimizes the distance between the true and predicted
#'  scale parameters. The bootstrap of log normal distribution parameters are
#'  created by bootstrapping the error distribution from the fitted model and
#'  adding it to the predict response variable.
#' @param train_data Training data of class data.frame.
#' @param direct_label Response/dependent/label variable name that is
#' predicted by the fitted model.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param mean Mean of the error distribution. Distribution is normal.
#' @param sd Standard deviation of the error distribution. Distribution
#' is normal.
#' @param nboot Number of times to bootstrap the error distribution. This is an
#'  integer type parameter. Default is 200, which creates 200 different log
#'  normal distribution parameters.
#' @param distr A character string that represent the distribution to fit for
#'  the label/response. Default is "lnorm" for normal distribution.
#'  See fitdistrplus::fitdist for string names for other distributions.
#' @param param_adjust A character string that represent the distribution
#' parameter that needs adjustment. Default is "sdlog" from the log normal
#' distribution. See fitdistrplus::fitdist for string
#' names for other distribution parameters.
#' @param label_convert Logical variable indicating that the final
#' response/label variable for fitting the distribution should be changed
#' from direct_label to the indirect_label. Default is FALSE, where the
#' direct_label is considered. If TRUE, the predicted values from the
#' fitted model must be multiplied by the "multiplier" to get the estimated
#' indirect_label.
#' @param multiplier Specify the multiplier column needed to compute the
#'  indirect_label quantity. Default: "snowdepth".
#' @param indirect_label Specify the actual indirect_label column name needed
#' for computing the true parameter values. Default: "snowload".
#' @param ... Other arguments to send to the distribution function
#'  fitdistrplus::fitdist
#' @return A value the represents the percentile of the bootstraps that gets the
#'  predicted scale parameter close to the true parameter.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # generate data for modelfitting
#'   data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#'
#'   # fit a gradient boosting machine to data
#'   model <- fit_model(
#'     data = data, method = "gbm", label = "y",
#'     features = c("x1", "x2")
#'   )
#'
#'   # find the best percentile that corrects the biasness in the distr. fitting
#'   best_percentile(
#'     train_data = data, direct_label = "y", fitted_model = model,
#'     label_convert = FALSE
#'   )
#' }
#' }
#' @seealso
#' [fitdistrplus::fitdist()], [distfixer::fit_model()] .
#' @rdname best_percentile
#' @export
#' @importFrom fitdistrplus fitdist
#' @importFrom stats rnorm quantile predict


best_percentile <- function(train_data, direct_label, fitted_model, mean = 0,
                            sd = 1, nboot = 200, distr = "lnorm",
                            param_adjust = "sdlog", label_convert = FALSE,
                            multiplier = "snowdepth",
                            indirect_label = "snowload", ...) {
  # Fit the specified distribution to the true label
  fit_true <- fit_true(
    train_data, distr, direct_label, label_convert,
    indirect_label
  )

  # Get the estimated param_adjust of the true data
  param_adjust_true <- fit_true$estimate[[param_adjust]]

  # compute the bootstrap of parameters
  params_matrix <- boot_sample_train(
    train_data, fitted_model, distr, mean, sd, nboot, label_convert, multiplier,
    fit_true
  )

  # Find the percentile of the param_adjust that's closest to param_adjust_true
  closest_param_index <- which.min(abs(params_matrix[, param_adjust] -
    param_adjust_true))

  closest_param <- params_matrix[closest_param_index, param_adjust]

  percentile <- sum(params_matrix[, param_adjust] <= closest_param) / nboot


  return(percentile)
}
