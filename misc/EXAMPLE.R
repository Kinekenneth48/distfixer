
data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rlnorm(100))

# Fit a gradient boosting machine model using fit_model
model <- fit_model(data = data, method = "gbm", label = "y", features = c("x1", "x2"))
