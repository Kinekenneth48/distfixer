# =============================================================================#
# boot_sample_train function(internal) declaration
# =============================================================================#
#' @title Bootstrap fitted model error for train data
#' @description This is an internal function that bootstraps the error from
#'  the fitted model and add them to the predicted response values. The new
#'  values are then fitted to a log normal distribution.
#' @param train_data Training data of class data.frame.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param bounds Specify the bounds of the label of interest to ensure
#'  a cap during the bootstrap process.
#' @param distr A character string that represent the distribution to fit for
#'  the label/response. Default is "norm" for normal distribution.
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
#' @param fit_true_object An object of class "fitdist".
#' @param multiplier Specify the multiplier column needed to compute the
#'  indirect_label quantity. Default: "snowdepth".
#' @param ... Other arguments to send to the distribution function
#'  fitdistrplus::fitdist
#' @return A matrix of location and scale parameters based on nboot.
#' @rdname boot_sample_train
#' @importFrom stats sd
boot_sample_train <- function(train_data, fitted_model, bounds, distr,
                              mean, sd, nboot,
                              label_convert, multiplier,
                              fit_true_object, ...) {
  # get class of model
  model_type <- class(fitted_model)

  predictions <- switch(model_type,
    "ranger" = {
      predictions <- predict(fitted_model, train_data)[["predictions"]]
    },
    "ksvm" = {
      predictions <- predict(fitted_model, train_data)
    },
    "gbm" = {
      predictions <- predict(fitted_model, train_data)
    },
    stop(paste("Unknown method:", model_type))
  )


  # Initialize a matrix to store the parameters
  params_matrix <- matrix(nrow = nboot, ncol = length(fit_true_object$estimate))
  colnames(params_matrix) <- names(fit_true_object$estimate)


  for (i in 1:nboot) {
    bootstrap_samples <- rnorm(n = nrow(train_data), mean = mean, sd = sd)

    # Add the bootstrapped residuals to the full data predictions
    y_bootstrap <- pmin(
      pmax(predictions + bootstrap_samples, bounds[1]),
      bounds[2]
    )

    # compute indirect_label if label_convert is TRUE
    if (label_convert == TRUE) {
      new_y_bootstrap <- as.vector(train_data[[multiplier]]) * y_bootstrap
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
