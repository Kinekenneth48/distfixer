fit_true <- function(train_data, label, snowload, snowload_col) {
  if (snowload) {
    fit <- fitdistrplus::fitdist(
      fit = train_data[[snowload_col]], distr = "lnorm",
      start = list(
        meanlog = mean(log(train_data[[snowload_col]])),
        sdlog = stats::sd(log(train_data[[snowload_col]]))
      ),
      method = "mle"
    )
  } else {
    fit <- fitdistrplus::fitdist(
      fit = train_data[[label]], distr = "lnorm",
      start = list(
        meanlog = mean(log(train_data[[label]])),
        sdlog = stats::sd(log(train_data[[label]]))
      ),
      method = "mle"
    )
  }

  return(fit)
}
