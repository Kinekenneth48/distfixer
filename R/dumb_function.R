
#' @title Dumb function
#' @param test_data Test data of class data.frame.
#' @param test_data_missing Test data with missing values for label.
#' @param test_data_not_missing Test data without missing values for label.
#' @param predictions Prediction values made for the missing label values. 
#' @param label_convert Logical variable indicating that the final
#' response/label variable for fitting the distribution should be changed
#' from direct_label to the indirect_label. Default is FALSE, where the
#' direct_label is considered. If TRUE, the predicted values from the
#' fitted model must be multiplied by the "multiplier" to get the estimated
#' indirect_label.
#' @param multiplier Specify the multiplier column needed to compute the
#'  indirect_label quantity. Default: "snowdepth".
#' @param mean Mean of model error. Distribution is normal.
#' @param sd Standard deviation of model error. Distribution is normal.
#' @param indirect_label Specify the actual indirect_label column name needed
#' for computing the true parameter values. Default: "snowload".
#' @param direct_label Response/dependent/label variable name that is
#' predicted by the fitted model.
#' @param distr A character string that represent the distribution to fit for
#'  the label/response. Default is "lnorm" for normal distribution.
#' @param ... Other arguments to send to the distribution function
#'  fitdistrplus::fitdist
#' @return fitted distr. object
#' @rdname dumb_function

dumb_function = function(test_data_missing,test_data_not_missing, 
                                 predictions, test_data, 
                                 label_convert, multiplier, mean, sd,
                                 indirect_label, direct_label,
                                 distr,...){
  
  
    bootstrap_samples <- rnorm(n = nrow(test_data_missing), mean = mean,
                               sd = sd
    )
    
    # Add the bootstrapped residuals to the predictions
    y_bootstrap <- ifelse((as.vector(predictions) + bootstrap_samples) <= 0,
                          as.vector(predictions),
                          (as.vector(predictions) + bootstrap_samples)
    )
    
    # compute swe if snowload is TRUE
    if (label_convert) {
      
      # combine the missing and non-missing data after imputation and bootstrap
      full_data <- c(y_bootstrap, 
                     as.numeric(test_data_not_missing[[indirect_label]]))
      
      # compute estimated indirect_label if label_convert is TRUE
      new_y_bootstrap <- as.vector(test_data[[multiplier]]) * full_data
    } else {
      
      # combine the missing and non-missing data after imputation and bootstrap
      new_y_bootstrap <- c(y_bootstrap, 
                           as.numeric(test_data_not_missing[[direct_label]]))
      
    }
    
    # Fit specified distribution to the boot samples
    distr_model <- fitdist(
      data = as.numeric(new_y_bootstrap), distr = distr
    )
  
  return(distr_model)
}