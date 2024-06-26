% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot_sample_train.R
\name{boot_sample_train}
\alias{boot_sample_train}
\title{Bootstrap fitted model error for train data}
\usage{
boot_sample_train(
  train_data,
  fitted_model,
  bounds,
  distr,
  mean,
  sd,
  nboot,
  label_convert,
  multiplier,
  fit_true_object,
  ...
)
}
\arguments{
\item{train_data}{Training data of class data.frame.}

\item{fitted_model}{A fitted model from the fit_model function. A fitted
model of class "ranger" when random forest if fitted, "ksvm"
when support vector regression is fitted, and "gbm.object" when gradient
boosting machine is fitted.}

\item{bounds}{Specify the bounds of the label of interest to ensure
a cap during the bootstrap process.}

\item{distr}{A character string that represent the distribution to fit for
the label/response. Default is "norm" for normal distribution.
See fitdistrplus::fitdist for string names for other distributions.}

\item{mean}{Mean of model error. Distribution is normal.}

\item{sd}{Standard deviation of model error. Distribution is normal.}

\item{nboot}{Number of times to bootstrap the error distribution. This is an
integer type parameter.}

\item{label_convert}{Logical variable indicating that the final
response/label variable for fitting the distribution should be changed
from direct_label to the indirect_label. Default is FALSE, where the
direct_label is considered. If TRUE, the predicted values from the
fitted model must be multiplied by the "multiplier" to get the estimated
indirect_label.}

\item{multiplier}{Specify the multiplier column needed to compute the
indirect_label quantity. Default: "snowdepth".}

\item{fit_true_object}{An object of class "fitdist".}

\item{...}{Other arguments to send to the distribution function
fitdistrplus::fitdist}
}
\value{
A matrix of location and scale parameters based on nboot.
}
\description{
This is an internal function that bootstraps the error from
the fitted model and add them to the predicted response values. The new
values are then fitted to a log normal distribution.
}
