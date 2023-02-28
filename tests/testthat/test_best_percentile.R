testthat::test_that("Test: best_percentile function", {
  set.seed(1232)
  expected <- best_percentile(
    train_data = data, label = "y",
    fitted_model = model,
    snowload = FALSE
  )
  actual <- 0.985

  expect_identical(expected, actual)
})
