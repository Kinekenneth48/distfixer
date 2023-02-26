
best_percentile <- function(train_data, label, fitted_model, method, nboot = 200,
                            snowload = TRUE, snowdepth_col = snowdepth, snowload_col = snowload) {
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


  return(list(percentile, error_distr))
}
