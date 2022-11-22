
#function REGRESSIONS significant values
one_by_one_significant_predictors_lm  <- function(predictors, one_response, thresh=0.05)
{
        models  <- map(predictors, ~  lm(one_response ~ as.factor(.x) ))
coeffs  <- map(models, ~coef(summary(.x))[-1, c( "Pr(>|t|)")]  )
sig  <- map(coeffs, ~ .x[.x <= thresh ]  )
selector <-  map_lgl(sig, ~length(.x) > 0)
models  <- models[selector]
trim <-  sig[selector]
return(trim)
}


send_responses_to_predictors_lm <- function(responses_dataset, predictors_dataset, threshold_significance = 0.05, factor_flag = FALSE)
{
results_predictors_response_one_by_one <- lapply(responses_dataset, one_by_one_significant_predictors_lm, threshold_significance = 0.05, factor_flag = TRUE)
names(results_predictors_response_one_by_one) <- names(responses_dataset)
selector <- sapply(responses_dataset, function(x) x > 0)
results_predictors_response_one_by_one <- results_predictors_response_one_by_one[selector]
return(results_predictors_response_one_by_one)
}

