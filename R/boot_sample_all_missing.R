# =============================================================================#
# boot_sample_all_missing function(internal) declaration
# =============================================================================#
#' @title Bootstrap fitted model error for test data
#' @description This is an internal function that bootstraps the error from
#'  the fitted model and add them to the predicted response values. The new
#'  values are then fitted to a log normal distribution.
#' @param test_data Test data of class data.frame.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param distr A character string that represent the distribution to fit for
#'  the label/response. Default is "lnorm" for normal distribution.
#'  See fitdistrplus::fitdist for string names for other distributions.
#' @param mean Mean of model error. Distribution is normal.
#' @param sd Standard deviation of model error. Distribution is normal.
#' @param nboot Number of times to bootstrap the error distribution. This is an
#'  integer type parameter.
#' @param label_convert Logical variable indicating that the final
#' response/label variable for fitting the distribution should be changed
#' from direct_label to the indirect_label. Default is FALSE, where the
#' direct_label is considered. If TRUE, the predicted values from the
#' fitted model must be multiplied by the "multiplier" to get the estimated
#' indirect_label.
#' @param multiplier Specify the multiplier column needed to compute the
#'  indirect_label quantity. Default: "snowdepth".
#' @return A matrix of parameters based on nboot.
#' @param ... Other arguments to send to the distribution function
#'  fitdistrplus::fitdist
#' @rdname boot_sample_all_missing
#' @importFrom stats sd
boot_sample_all_missing <- function(test_data,  fitted_model, distr, 
                                    mean, sd, nboot, label_convert,
                                    multiplier, ...) {

  # get class of model
  model_type <- class(fitted_model)

  predictions <- switch(model_type,
    "rf" = {
      predictions <- predict(fitted_model, test_data)[["predictions"]]
    },
    "svr" = {
      predictions <- predict(fitted_model, test_data)
    },
    "gbm" = {
      predictions <- predict(fitted_model, test_data)
    },
    stop(paste("Unknown method:", model_type))
  )

  #fit the specified distr. to data(dumb function version)
  dumb_distr_model <- fitdist(
    data = predictions, distr = distr
  )
  
  # Initialize a matrix to store the parameters
  params_matrix <- matrix(nrow = nboot, ncol = length(dumb_distr_model$estimate))
  colnames(params_matrix) <- names(dumb_distr_model$estimate)
  
  for (i in 1:nboot) {
    bootstrap_samples <- rnorm(n = nrow(test_data), mean = mean, sd = sd)

    # Add the bootstrapped residuals to the full data predictions
    y_bootstrap <- ifelse((as.vector(predictions) + bootstrap_samples) <= 0,
      as.vector(predictions),
      (as.vector(predictions) + bootstrap_samples)
    )

    # compute estimated indirect_label if label_convert is TRUE
    if (label_convert) {
      new_y_bootstrap <- as.vector(test_data[[multiplier]]) * y_bootstrap
    } else {
      new_y_bootstrap <- y_bootstrap
    }

    # Fit specified distribution to the boot samples
    distr_model <- fitdist(
      data = as.numeric(new_y_bootstrap), distr = distr
    )

    # Store the parameters in the matrix
    params_matrix[i, ] <- distr_model$estimate
  }

  
  return(params_matrix)
}
