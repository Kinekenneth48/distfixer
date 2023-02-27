
get_error_distribution <- function(train_data, fitted_model, method, label ="RATIO") {
  error_distr <- switch(method,
    "rf" = {
      res <- as.numeric(train_data[[label]]) - predict(fitted_model, train_data)
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    "svr" = {
      res <- as.numeric(train_data[[label]]) - predict(fitted_model, train_data)
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    "gbm" = {
      res <- as.numeric(train_data[[label]]) - predict(fitted_model, train_data)
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    stop(paste("Unknown method:", method))
  )
  
  return(error_dist = list(
    distribution = error_distr$distname,
    parameter_sd = error_distr$estimate[["mean"]],
    parameter_sd = error_distr$estimate[["sd"]]
  ))
}
