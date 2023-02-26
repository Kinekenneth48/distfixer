# =============================================================================#
# best_percentile function(internal) declaration
# =============================================================================#
#' @title Bootstrap fitted model error for train data
#' @description This is an internal function that bootstraps the error from
#'  the fitted model and add them to the predicted response values. The new
#'  values are then fitted to a log normal distribution.
#' @param train_data Training data of class data.frame.
#' @param error_distr Fitted normal distribution of model errors.
#' @param nboot Number of times to bootstrap the error distribution. This is an
#'  integer type parameter.
#' @param label Response/dependent variable name in the train_data.
#' @param snowload Logical variable indicating that the final response variable
#'  for fitting the distribution is snowload. In this case, the initial response
#'  variable(actual/predicted) is multiplied against snow depth. Default is
#'  TRUE.  When FALSE, initial response variable (snowload is not computed) for
#'  distribution fitting.
#' @param snowdepth_col Specify the snow depth column needed to compute the
#'  snowload quantity. Default: "snowdepth".
#' @return A matrix of location and scale parameters based on nboot.
#' @rdname boot_sample_train
#' @importFrom stats sd
boot_sample_train <- function(train_data, error_distr, nboot,
                              label, snowload, snowdepth_col) {
  # Initialize a matrix to store the parameters
  lnorm_params_matrix <- matrix(nrow = nboot, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")

  for (i in 1:nboot) {
    bootstrap_samples <- rnorm(
      n = nrow(train_data),
      mean = error_distr[["estimate"]][["mean"]],
      sd = error_distr[["estimate"]][["sd"]]
    )



    # Add the bootstrapped residuals to the full data predictions
    y_bootstrap <- ifelse((as.vector(train_data[[label]]) + bootstrap_samples) <= 0,
      as.vector(train_data[[label]]),
      (as.vector(train_data[[label]]) + bootstrap_samples)
    )


    # compute swe if snowload is TRUE
    if (snowload) {
      new_y_bootstrap <- as.vector(train_data[[snowdepth_col]]) * y_bootstrap
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
