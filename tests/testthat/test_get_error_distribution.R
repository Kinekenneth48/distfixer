testthat::test_that("Test 1: get_error_distribution function", {
  set.seed(1232)
  expected <- get_error_distribution(train_data = data,  label = "y",
                              fitted_model = model
                              )
  actual <- round(0.0086413837, 4)
  expected = round(expected$parameter_mean, 4)
  expect_equal(expected, actual)
})


testthat::test_that("Test 1: get_error_distribution function", {
  set.seed(1232)
  expected <- get_error_distribution(train_data = data,  label = "y",
                                     fitted_model = model
  )
  actual <- round( 2.243325, 2)
  expected = round(expected$parameter_sd, 2)
  expect_equal(expected, actual)
})