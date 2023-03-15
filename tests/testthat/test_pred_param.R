testthat::test_that("Test I: pred_param function", {
  set.seed(1232)
  expected <- predict_param(
    test_data = data, label_convert = FALSE,
    fitted_model = model, param_adjust = "sdlog", bounds = c(1, Inf)
  )
  actual <- round(0.6, 1)
  expected <- round(as.numeric(expected[1]), 1)
  expect_identical(expected, actual)
})


testthat::test_that("Test II: pred_param function", {
  set.seed(1232)
  expected <- predict_param(
    test_data = data, label_convert = FALSE,
    fitted_model = model, param_adjust = "sdlog", bounds = c(1, Inf)
  )
  actual <- round(0.4, 1)
  expected <- round(as.numeric(expected[2]), 1)
  expect_equal(expected, actual)
})
