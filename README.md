
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distfixer

<!-- badges: start -->
<!-- badges: end -->

The "distfixer" package allows the user to correct the biases
in a distribution scale parameter when missing values are imputed using
machine learning models. The package includes the "fit_model" function, 
to fit a machine learning model. The "get_error_distribution" is used to get the 
parameters of the error distribution. The " best_percentile" to get the best
percentile that corrects the scale parameters' biases.
The "pred_param" is used to compute the unbiased distribution parameters.

## Installation

You can install the development version of distfixer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Kinekenneth48/distfixer")
```

