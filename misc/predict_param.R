
predict_param <- function(test_data, fitted_model, snowload = TRUE,
                          snowdepth_col = snowdepth, snowload_col = snowload,
                          mean = 0, sd = 1, percentile,
                          nboot = 200) {
  lnorm_params_matrix <- boot_sample_test(
    test_data, nboot, mean, sd,
    label, snowload, snowdepth_col
  )



  quantile_value <- quantile(lnorm_params_matrix[, 2], probs = percentile, na.rm = TRUE)

  diffs <- abs(lnorm_params_matrix[, 2] - quantile_value)
  closest_row_position <- which.min(diffs)

  result <- lnorm_params_matrix[closest_row_position, ]


  return(result)
}
