testthat::test_that("Test I: pred_param function", {
  set.seed(1232)
  expected <- predict_param(test_data = data,  snowload = FALSE,
                                     fitted_model = model
  )
  actual <- round( 0.33  , 1)
  expected = round(as.numeric(expected[1]), 1)
  expect_identical(expected, actual)
})


testthat::test_that("Test II: pred_param function", {
  set.seed(1232)
  expected <- predict_param(test_data = data,  snowload = FALSE,
                            fitted_model = model
  )
  actual <- round(0.8632859 , 1)
  expected = round(as.numeric(expected[2]), 1)
  expect_equal(expected, actual)
})
