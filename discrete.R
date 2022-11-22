library(broom)
library(purrr)
library(tidyr)
library(dplyr)

#function REGRESSIONS significant values
one_by_one_significant_predictors_lm  <- function(one_response, predictors, threshold_significance = 0.05, factor_flag = FALSE)
{
	models  <- map(predictors, ~  if(factor_flag) lm(one_response ~ as.factor(.x) ) else lm(one_response ~ .x))
	coeffs  <- map(models, ~coef(summary(.x))[-1, c( "Pr(>|t|)")]  )
	sig  <- map(coeffs, ~ .x[.x <= threshold_significance ]  )
	selector <-  map_lgl(sig, ~length(.x) > 0)
	models  <- models[selector]
	trim <-  sig[selector]
	return(trim)
}


send_responses_to_predictors_lm <- function(responses_dataset, predictors_dataset, threshold_significance = 0.05, factor_flag = FALSE)
{
results_predictors_response_one_by_one <- lapply(responses_dataset, one_by_one_significant_predictors_lm, predictors_dataset, threshold_significance = 0.05, factor_flag = TRUE)
#names(results_predictors_response_one_by_one) <- names(responses_dataset)
#selector <- sapply(responses_dataset, function(x) x > 0)
#results_predictors_response_one_by_one <- results_predictors_response_one_by_one[selector]
return(results_predictors_response_one_by_one)
}

data(iris)
respon <- data.frame(iris[, 1:3])
pred <- data.frame(iris[, 4:5])

rrrr <- send_responses_to_predictors_lm(respon, pred, 0.10, FALSE)

#rrrr <- one_by_one_significant_predictors_lm(iris$Sepal.Length, pred)

