testthat::test_that("Test: best_percentile function", {
  set.seed(1232)
  expected <- best_percentile(
    train_data = data, direct_label = "y",
    fitted_model = model,
    label_convert = FALSE , method = "mme"
  )
  actual <- 0.985

  expect_identical(expected, actual)
})
