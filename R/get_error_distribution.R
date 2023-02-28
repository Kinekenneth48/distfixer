
#' @title Error distribution
#' @description This function fits the residuals of the training data to a
#'  normal distribution and returns the distribution parameters.
#' @param train_data Training data of class data.frame.
#' @param fitted_model A fitted model from the fit_model function. A fitted
#'  model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @param label Response/dependent variable name in the train_data.
#' @return Normal distribution parameters.
#' @seealso 
#'  \code{\link[fitdistrplus]{fitdist}}
#' @rdname get_error_distribution
#' @export 
#' @importFrom fitdistrplus fitdist
get_error_distribution <- function(train_data, fitted_model,  label ) {
  
  #get class of model
  model_type <- class(fitted_model)
  
  #estimate the distribution of the training error
  error_distr <- switch(model_type,
    "ranger" = {
      res <- as.numeric(train_data[[label]]) - 
        predict(fitted_model, train_data)[["predictions"]]
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    "ksvm" = {
      res <- as.numeric(train_data[[label]]) - predict(fitted_model, train_data)
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    "gbm" = {
      res <- as.numeric(train_data[[label]]) - predict(fitted_model, train_data)
      fit_norm <- fitdistrplus::fitdist(res, "norm")
    },
    stop(paste("Unknown method:", model_type))
  )
  
  return(error_dist = list(
    distribution = error_distr$distname,
    parameter_mean = error_distr$estimate[["mean"]],
    parameter_sd = error_distr$estimate[["sd"]]
  ))
}
