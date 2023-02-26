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
