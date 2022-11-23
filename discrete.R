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
return(results_predictors_response_one_by_one)
}

source('https://github.com/umontano/CBQ_comandos_SPSS_lab_ChyC/raw/main/CBQ_comandosSPSS_lab_CHyC.R')


library('tidytext')
library('widyr')
library('ggplot2')
library('dplyr')
library('tidyr')
library('tibble')
library('psych')
merged_categorical_and_torrance_totals <- function(columns_dataset, sign=0.05)
{

merged_dataset <- factors %>%
	add_id_column_numero %>%
	merge(add_id_column_numero(scales), by='numero') %>%
	merge(add_id_column_numero(raven), by='numero') %>%
	merge(add_id_column_numero(columns_dataset), by='numero') %>%
	mutate(numero=as.numeric(numero)) %>%
	select(!starts_with('X')) %>%
	select(where(is.numeric))

tor <- merged_dataset %>%
    select(c(names(columns_dataset), -numero))
torrest <- merged_dataset %>%
    select(-numero) %>%
    select(!names(columns_dataset))


#categorical_names <- c('escuela', 'grupo', 'sexo', 'edad', 'percentil', 'rango', 'dx')
categorical_names <- c('escuela', 'grupo', 'sexo', 'edad', 'percentil', 'rango')
categorical_variables <- merged_dataset %>%
select(categorical_names) %>%
    select(-numero) %>%
    select(!names(columns_dataset))

return(list(tor, categorical_variables))
}

tor_categorical <- merged_categorical_and_torrance_totals(torrance_totals)
results <- 
 send_responses_to_predictors_lm(tor_categorical[[1]], tor_categorical[[2]], 0.10, FALSE)


data(iris)
respon <- data.frame(iris[, 1:3])
pred <- data.frame(iris[, 4:5])

#rrrr <- send_responses_to_predictors_lm(respon, pred, 0.10, FALSE)

