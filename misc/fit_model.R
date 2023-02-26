


fit_model <- function(formula, data, method, label, features, cv_folds =5) {
  # Extract the label and feature names from the formula or arguments
  if (missing(formula)) {
    if (missing(label) || missing(features)) {
      stop("Both label and features must be provided when formula is not provided.")
    }
    formula <- as.formula(paste(label, "~", paste(features, collapse = "+")))
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
        num.trees = 500,
        mtry = 3,
        min.node.size = 5,
        importance = "impurity",
        oob.error = TRUE
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
        n.trees = 500,
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
