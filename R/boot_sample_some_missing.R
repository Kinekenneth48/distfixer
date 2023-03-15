# =============================================================================#
# boot_sample_some_missing function(internal) declaration
# =============================================================================#
#' @title Bootstrap fitted model error for data
#' @description This is an internal function that bootstraps the error from
#'  the fitted model and add them to the predicted response values. The new
#'  values are then fitted to a log normal distribution.
#' @param test_data Test data of class data.frame.
#' @param direct_label Response/dependent/label variable name that is
#' predicted by the fitted model.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param distr A character string that represent the distribution to fit for
#'  the label/response. Default is "lnorm" for normal distribution.
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
#' @param indirect_label Specify the actual indirect_label column name needed
#' for computing the true parameter values. Default: "snowload".
#' @param multiplier Specify the multiplier column needed to compute the
#'  indirect_label quantity. Default: "snowdepth".
#' @param ... Other arguments to send to the distribution function
#'  fitdistrplus::fitdist
#' @return A matrix of location and scale parameters based on nboot.
#' @rdname boot_sample_some_missing
#' @importFrom stats sd
boot_sample_some_missing <- function(test_data, direct_label, indirect_label,
                                     fitted_model, mean, sd, distr,
                                     nboot, label_convert, multiplier, ...) {
  if (label_convert) {
    # split data into missing and not missing
    test_data_missing <- test_data[is.na(test_data[[indirect_label]]), ]
    test_data_not_missing <- test_data[!is.na(test_data[[indirect_label]]), ]
  } else {
    # split data into missing and not missing
    test_data_missing <- test_data[is.na(test_data[[direct_label]]), ]
    test_data_not_missing <- test_data[!is.na(test_data[[direct_label]]), ]
  }


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

  # fit the specified distr. to data
  dumb_distr_model <- dumb_function(
    test_data_missing = test_data_missing,
    test_data_not_missing = test_data_not_missing,
    predictions = predictions, label_convert, multiplier, mean, sd,
    indirect_label, direct_label, distr
  )

  # Initialize a matrix to store the parameters
  params_matrix <- matrix(nrow = nboot, ncol = length(dumb_distr_model$estimate))
  colnames(params_matrix) <- names(dumb_distr_model$estimate)


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

    # compute swe if snowload is TRUE
    if (label_convert) {
      # combine the missing and non-missing data after imputation and bootstrap
      full_data <- c(
        y_bootstrap,
        as.numeric(test_data_not_missing[[indirect_label]])
      )

      # compute estimated indirect_label if label_convert is TRUE
      new_y_bootstrap <- as.vector(test_data[[multiplier]]) * full_data
    } else {
      # combine the missing and non-missing data after imputation and bootstrap
      new_y_bootstrap <- c(
        y_bootstrap,
        as.numeric(test_data_not_missing[[direct_label]])
      )
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
