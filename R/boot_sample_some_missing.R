# =============================================================================#
# boot_sample_some_missing function(internal) declaration
# =============================================================================#
#' @title Bootstrap fitted model error for data
#' @description This is an internal function that bootstraps the error from
#'  the fitted model and add them to the predicted response values. The new
#'  values are then fitted to a log normal distribution.
#' @param test_data Test data of class data.frame.
#' @param label Response/dependent variable name in the data.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param mean Mean of model error. Distribution is normal.
#' @param sd Standard deviation of model error. Distribution is normal.
#' @param nboot Number of times to bootstrap the error distribution. This is an
#'  integer type parameter.
#' @param snowload Logical variable indicating that the final response variable
#'  for fitting the distribution is snowload. In this case, the initial response
#'  variable(actual/predicted) is multiplied against snow depth. Default is
#'  TRUE.  When FALSE, initial response variable (snowload is not computed) for
#'  distribution fitting.
#' @param snowdepth_col Specify the snow depth column needed to compute the
#'  snowload quantity. Default: "snowdepth".
#' @return A matrix of location and scale parameters based on nboot.
#' @rdname boot_sample_some_missing
#' @importFrom stats sd
boot_sample_some_missing <- function(test_data, label, fitted_model, mean, sd,
                                     nboot, snowload, snowdepth_col) {
  # Initialize a matrix to store the parameters
  lnorm_params_matrix <- matrix(nrow = nboot, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")

  # split data into missing and not missing
  test_data_missing <- test_data[is.na(test_data[[label]]), ]
  test_data_not_missing <- test_data[!is.na(test_data[[label]]), ]

  # get class of model
  model_type <- class(fitted_model)

  predictions <- switch(model_type,
    "rf" = {
      predictions <- predict(fitted_model, test_data_missing)[["predictions"]]
    },
    "svr" = {
      predictions <- predict(fitted_model, test_data_missing)
    },
    "gbm" = {
      predictions <- predict(fitted_model, test_data_missing)
    },
    stop(paste("Unknown method:", model_type))
  )

  # boot residuals and add to predictions
  for (i in 1:nboot) {
    bootstrap_samples <- rnorm(
      n = nrow(test_data_missing), mean = mean,
      sd = sd
    )

    # Add the bootstrapped residuals to the predictions
    y_bootstrap <- ifelse((as.vector(predictions) + bootstrap_samples) <= 0,
      as.vector(predictions),
      (as.vector(predictions) + bootstrap_samples)
    )

    # combine the missing and non-missing data after imputation and bootstrap
    full_data <- c(y_bootstrap, as.numeric(test_data_not_missing[[label]]))

    # compute swe if snowload is TRUE
    if (snowload) {
      new_y_bootstrap <- as.vector(test_data[[snowdepth_col]]) * full_data
    } else {
      new_y_bootstrap <- full_data
    }


    # Fit a lnorm distribution to the boot samples
    lnorm_model <- fitdist(
      data = as.numeric(new_y_bootstrap), distr = "lnorm",
      start = list(
        meanlog = mean(log(new_y_bootstrap)), sdlog =
          stats::sd(log(new_y_bootstrap))
      ),
      method = "mle"
    )


    # Store the parameters in the matrix
    lnorm_params_matrix[i, ] <- lnorm_model$estimate
  }

  return(lnorm_params_matrix)
}
