testthat::test_that("Test: best_percentile function", {
  set.seed(1232)
  expected <- best_percentile(
    train_data = data, direct_label = "y",
    fitted_model = model,
    label_convert = FALSE, method = "mme", bounds = c(0.1, Inf)
  )

  actual <- as.vector(round(0.97, 1))
  expected <- as.vector(round(expected, 1))
  expect_identical(expected, actual)
})
