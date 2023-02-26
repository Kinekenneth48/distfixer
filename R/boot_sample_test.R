# =============================================================================#
# boot_sample_test function(internal) declaration
# =============================================================================#
#' @title Bootstrap fitted model error for test data
#' @description This is an internal function that bootstraps the error from
#'  the fitted model and add them to the predicted response values. The new
#'  values are then fitted to a log normal distribution.
#' @param test_data Test data of class data.frame.
#' @param nboot Number of times to bootstrap the error distribution. This is an
#'  integer type parameter.
#' @param mean Mean of the normal distribution of fitted model errors,
#'  Default: 0.
#' @param sd Standard deviation of the normal distribution of fitted model
#'  errors, Default: 1.
#' @param predictions PARAM_DESCRIPTION
#' @param snowload Logical variable indicating that the final response variable
#'  for fitting the distribution is snowload. In this case, the initial response
#'  variable(actual/predicted) is multiplied against snow depth. Default is
#'  TRUE.  When FALSE, initial response variable (snowload is not computed) for
#'  distribution fitting.
#' @param snowdepth_col Specify the snow depth column needed to compute the
#'  snowload quantity. Default: "snowdepth".
#' @return A matrix of location and scale parameters based on nboot.
#' @rdname boot_sample_test
#' @importFrom stats sd
boot_sample_test <- function(test_data, nboot, mean, sd,
                             predictions, snowload, snowdepth_col) {
  # Initialize a matrix to store the parameters
  lnorm_params_matrix <- matrix(nrow = nboot, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")

  for (i in 1:nboot) {
    bootstrap_samples <- rnorm(n = nrow(test_data), mean = mean, sd = sd)

    # Add the bootstrapped residuals to the full data predictions
    y_bootstrap <- ifelse((as.vector(predictions) + bootstrap_samples) <= 0,
      as.vector(predictions),
      (as.vector(predictions) + bootstrap_samples)
    )


    # compute swe if snowload is TRUE
    if (snowload) {
      new_y_bootstrap <- as.vector(test_data[[snowdepth_col]]) * y_bootstrap
    } else {
      new_y_bootstrap <- y_bootstrap
    }


    # Fit a lnorm distribution to the boot samples
    lnorm_model <- fitdist(
      data = as.numeric(new_y_bootstrap), distr = "lnorm",
      start = list(
        meanlog = mean(log(new_y_bootstrap)), sdlog = stats::sd(log(new_y_bootstrap))
      ),
      method = "mle"
    )


    # Store the parameters in the matrix
    lnorm_params_matrix[i, ] <- lnorm_model$estimate
  }

  return(lnorm_params_matrix)
}
