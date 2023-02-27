# =============================================================================#
# best_percentile function(internal) declaration
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
#' @param method Machine learning model of the fitted model. This is a character class type
#'  where "rf" - random forest, "svr" - support vector regression, and
#'  "gbm" - gradient boosting machine.
#' @param mean Mean of model error. Distribution is normal.
#' @param sd Standard deviation of model error. Distribution is normal.
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
boot_sample_train <- function(train_data,fitted_model,method, mean, sd, nboot, 
                              label, snowload,
                              snowdepth_col) {
  # Initialize a matrix to store the parameters
  lnorm_params_matrix <- matrix(nrow = nboot, ncol = 2)
  colnames(lnorm_params_matrix) <- c("meanlog", "sdlog")

  
 predictions <- switch(method,
            "rf" = {
              predictions = predict(fitted_model, train_data)[["predictions"]]
            },
            "svr" = {
              predictions = predict(fitted_model, train_data)
            },
            "gbm" = {
              predictions <- predict(fitted_model, train_data)
            },
            stop(paste("Unknown method:", method))
  )
 
  for (i in 1:nboot) {
    bootstrap_samples <- rnorm(n = nrow(train_data), mean = mean, sd = sd)


    # Add the bootstrapped residuals to the full data predictions
    y_bootstrap <- ifelse(
      (predictions + bootstrap_samples) <= 0,
      predictions,
      (predictions + bootstrap_samples)
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
