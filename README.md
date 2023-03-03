
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distfixer

<!-- badges: start -->
<!-- badges: end -->

The "distfixer" package allow the user to correct the biasness
in lognormal distribution fitting when missing values are imputed using
machine learning models. The package includes the "fit_model" function, 
to fit a machine learning model. The "get_error_distribution" to get the 
parameters of the error distribution. The " best_percentile" to get the best
percentile that corrects the biasness in the location and scale parameters.
The "pred_param" to compute the unbiased lognormal distribution parameters.

## Installation

You can install the development version of distfixer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Kinekenneth48/distfixer")
```

