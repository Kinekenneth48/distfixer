testthat::test_that("Test 1: get_error_distribution function: mean", {
  set.seed(1232)
  expected <- get_error_distribution(
    train_data = data, label = "y", distr = "norm",
    fitted_model = model
  )
  actual <- as.vector(round(0.0086413837, 4))
  expected <- as.vector(round(expected$estimate[1], 4))
  expect_equal(expected, actual)
})


testthat::test_that("Test 1: get_error_distribution function: sd", {
  set.seed(1232)
  expected <- get_error_distribution(
    train_data = data, label = "y", distr = "norm",
    fitted_model = model
  )
  actual <- as.vector(round(2.243325, 2))
  expected <- as.vector(round(expected$estimate[2], 2))
  expect_equal(expected, actual)
})
