library(mlr3)
library(mlr3learners)
library(ranger)

# Define a function to tune hyperparameters for regression-based machine learning methods
tune_regression_model <- function(data, target_col, method) {
  # Create a task for regression
  task <- TaskRegr$new(id = "regression_task",
                       backend = data[, c(features, target_col)], target = target_col)
  
  # Define search space for the specified machine learning method
  if (method == "rf") {
    rf_space <- ps(
      mtry = p_int(lower = 2, upper = ncol(data) - 1),
      min.node.size = p_int(lower = 1, upper = 20),
      ntrees = p_int(lower = 10, upper = 500)
    )
    learner <- lrn("regr.ranger", predict_type = "response", tune_ps = rf_space)
  } else if (method == "svm") {
    svm_space <- ps(
      cost = p_log(lower = 0.001, upper = 10),
      epsilon = p_log(lower = 0.001, upper = 10),
      sigma = p_log(lower = 0.001, upper = 1)
    )
    learner <- lrn("regr.svm", predict_type = "response", tune_ps = svm_space)
  } else if (method == "gbm") {
    gbm_space <- ps(
      n.trees = p_int(lower = 10, upper = 500),
      interaction.depth = p_int(lower = 1, upper = 5),
      shrinkage = p_dbl(lower = 0.01, upper = 0.1),
      bag.fraction = p_dbl(lower = 0.5, upper = 1),
      sample.fraction = p_dbl(lower = 0.5, upper = 1)
    )
    learner <- lrn("regr.gbm", predict_type = "response", tune_ps = gbm_space)
  } else {
    stop(paste("Invalid method:", method))
  }
  
  # Define the resampling method
  resampling <- mlr3::rsmp("cv", folds = 5)
  
  # Define the tuner
  tuner <- mlr3::tnr("random_search", maxit = 100, resolution = 20)
  
  # Define the benchmark
  benchmark <- BenchmarkRegr$new(
    tasks = task,
    learners = learner,
    resamplings = resampling,
    tuners = tuner
  )
  
  # Run the benchmark
  results <- benchmark$benchmark()
  
  # Return the best hyperparameters
  best_params <- as.data.frame(tune_result(results$learner)$x)
  if (method == "rf") {
    colnames(best_params) <- c("Mtry", "Min.node.size", "N.trees")
  } else if (method == "svm") {
    colnames(best_params) <- c("Cost", "Epsilon", "Sigma")
  } else if (method == "gbm") {
    colnames(best_params) <- c("N.trees", "Interaction.depth", "Shrinkage", 
                               "Bag.fraction", "Sample.fraction")
  }
  
  return(best_params)
}

# Example usage of the function
data(iris)
best_params_rf <- tune_regression_model(data = iris[, -5], 
                                        target_col = "Petal.Width", method = "rf")
print(best_params_rf)

best_params_svm <- tune_regression_model(data = iris[,
                                                     