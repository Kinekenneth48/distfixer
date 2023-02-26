# =============================================================================#
# best_percentile function(external) declaration
# =============================================================================#
#' @title Best Percentile
#' @description This function finds the best percentile out of the bootstraps of
#'  scale parameters that minimizes the distance between the true and predicted
#'  scale parameter. The bootstrap of log normal distribution parameters are
#'  created by bootstrapping the error distribution from the fitted model and
#'  adding it to the predict response variable.
#' @param train_data Training data of class data.frame.
#' @param label Response/dependent variable name in the train_data.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param method Machine learning model to fit. This is a character class type
#'  where "rf" - random forest, "svr" - support vector regression, and
#'  "gbm" - gradient boosting machine.
#' @param nboot Number of times to bootstrap the error distribution. This is an
#'  integer type parameter. Default is 200, which creates 200 different log
#'  normal distribution parameters.
#' @param snowload Logical variable indicating that the final response variable
#'  for fitting the distribution is snowload. In this case, the initial response
#'  variable(actual/predicted) is multiplied against snow depth. Default is
#'  TRUE.  When FALSE, initial response variable (snowload is not computed) for
#'  distribution fitting.
#' @param snowdepth_col Specify the snow depth column needed to compute the
#'  snowload quantity. Default: "snowdepth".
#' @param snowload_col Specify the snowload column name needed for computing
#'  the true parameter values. Default: "snowload".
#' @return A value the represents the percentile gets the bootstrap of
#'  predicted scale parameter close to the true parameter.
#'
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[fitdistrplus]{fitdist}}
#' @rdname best_percentile
#' @export
#' @importFrom fitdistrplus fitdist
#' @importFrom stats rnorm quantile predict

best_percentile <- function(train_data, label, fitted_model, method, nboot = 200,
                            snowload = TRUE, snowdepth_col = "snowdepth",
                            snowload_col = "snowload") {
  error_distr <- switch(method,
    "rf" = {
      res <- train_data[[label]] - predict(fitted_model, train_data)
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    "svr" = {
      res <- train_data[[label]] - predict(fitted_model, train_data)
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    "gbm" = {
      res <- train_data[[label]] - predict(fitted_model, train_data)
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    stop(paste("Unknown method:", method))
  )




  lnorm_params_matrix <- boot_sample_train(
    train_data, error_distr, nboot,
    label, snowload, snowdepth
  )



  # Fit a lognormal distribution to the true SWE
  fit_true <- fit_true(train_data, label, snowload, snowload_col)

  # Get the estimated standard deviation of the true data
  sd_true <- fit_true$estimate[["sdlog"]]

  # Find the percentile of the sdlog that is closest to sd_true
  closest_sd_index <- which.min(abs(lnorm_params_matrix[, "sdlog"] - sd_true))
  closest_sd <- lnorm_params_matrix[closest_sd_index, "sdlog"]
  percentile <- sum(lnorm_params_matrix[, "sdlog"] <= closest_sd) / nboot


  return(list(
    percentile = percentile,
    error_distr = list(
      distribution = error_distr$distname,
      parameter_sd = error_distr$estimate[["mean"]],
      parameter_sd = error_distr$estimate[["sd"]]
    )
  ))
}
