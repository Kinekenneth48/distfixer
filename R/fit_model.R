# =============================================================================#
# model_fit function(external) declaration
# =============================================================================#
#' @title Fit machine learning model
#' @description This function allows the user to fit machine learning models
#'  which include random forest, support vector regression, and gradient boosting
#'  machine.
#' @param formula Object of class character describing the model to fit.
#' @param data Training data of class data.frame.
#' @param method Machine learning model to fit. This is a character class type
#'  where "rf" - random forest, "svr" - support vector regression, and
#'  "gbm" - gradient boosting machine.
#' @param label Response/dependent variable, alternative interface to data with
#'  formula approach.
#' @param features Predictor variables, alternative interface to data with
#'  formula approach.
#' @param cv_folds A k-fold cross validation on the training data, Default: 5.
#'  This is an integer type parameter.
#' @return A fitted model of class "ranger" when random forest if fitted, "ksvm"
#'  when support vector regression is fitted, and "gbm.object" when gradient
#'  boosting machine is fitted.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#'   model <- fit_model(
#'     data = data, method = "gbm", label = "y",
#'     features = c("x1", "x2")
#'   )
#' }
#' }
#' @seealso
#'  \code{\link[ranger]{ranger}}
#'  \code{\link[kernlab]{ksvm}}
#'  \code{\link[gbm]{gbm}}
#' @rdname fit_model
#' @export
#' @importFrom ranger ranger
#' @importFrom kernlab ksvm
#' @importFrom parallel detectCores
#' @importFrom gbm gbm
fit_model <- function(formula, data, method, label, features, cv_folds = 5) {
  # Extract the label and feature names from the formula or arguments
  if (missing(formula)) {
    if (missing(label) || missing(features)) {
      stop("Both label and features must be provided when formula is not provided.")
    }
    formula <- stats::as.formula(paste(label, "~", paste(features, collapse = "+")))
  } else {
    if (missing(label) || missing(features)) {
      f <- formula(data)
      label <- as.character(f[[2]])
      features <- names(f)[-2]
    }
  }

  
  
  # fit model based on model specification
  model_fit <- switch(method,
    "rf" = {
      ranger::ranger(
        formula = formula,
        data = data,
        num.trees = 200,
        mtry = 3,
        min.node.size = 5,
        importance = "impurity",
        oob.error = TRUE,
        num.threads = parallel::detectCores() - 2
      )
    },
    "svr" = {
      kernlab::ksvm(
        formula,
        data = data,
        kernel = "rbfdot",
        C = 1,
        kpar = list(sigma = 0.1),
        type = "eps-svr",
        cross = cv_folds,
        epsilon = 0.1,
        numThreads = parallel::detectCores() - 2
      )
    },
    "gbm" = {
      gbm::gbm(
        formula = formula,
        data = data,
        distribution = "gaussian",
        n.trees = 200,
        interaction.depth = 6,
        shrinkage = 0.01,
        bag.fraction = 0.7,
        cv.folds = cv_folds,
        train.fraction = 1,
        n.cores = parallel::detectCores() - 2
      )
    },
    stop(paste("Unknown method:", method))
  )



  # Return the trained model
  return(model_fit)
}
