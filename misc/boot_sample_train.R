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
