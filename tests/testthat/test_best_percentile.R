testthat::test_that("Test: best_percentile function", {
  set.seed(1232)
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rlnorm(100))
  
  # Fit a gradient boosting machine model using fit_model
  model <- fit_model(data = data, method = "gbm", label = "y", 
                     features = c("x1", "x2"))
  
  
  expected <- best_percentile(train_data = data,  label = "y",
                              fitted_model = model, 
                              snowload = FALSE,method = "gbm" )$percentile
  actual <- 0.085
  expect_identical(expected, actual)
})