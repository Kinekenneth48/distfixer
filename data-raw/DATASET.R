## code to prepare `DATASET` dataset goes here


load("/tests/testdata/model.RData")
load("/tests/testdata/data.RData")
usethis::use_data(data, model, overwrite = TRUE, internal = TRUE)
