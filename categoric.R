
#function REGRESSIONS significant values
one_by_one_significant_predictors_lm  <- function(one_response, predictors, threshold_significance = 0.05, categorical_flag = FALSE)
{
	models  <- map(predictors, ~ if(categorical_flag) lm(one_response ~ as.factor(.x) ) else lm(one_response ~ .x))
	coeffs  <- map(models, ~ coef(summary(.x))[-1, c( "Pr(>|t|)")]  )
	sig  <- map(coeffs, ~ .x[ .x <= threshold_significance & !is.na(.x) ]  )
	selector <-  map_lgl(sig, ~length(.x) > 0)
	models  <- models[selector]
	trim <-  sig[selector]
	return(trim)
}


send_responses_to_predictors_lm <- function(responses_dataset, predictors_dataset, threshold_significance = 0.05, categorical_flag = FALSE)
{
	results_predictors_response_one_by_one <- lapply(responses_dataset, one_by_one_significant_predictors_lm, predictors_dataset, threshold_significance = threshold_significance, categorical_flag = categorical_flag)
	selector <- sapply(results_predictors_response_one_by_one, function(x) length(x)>0)
	return(results_predictors_response_one_by_one[selector])
}

merged_categorical_and_torrance_totals <- function(columns_dataset, categorical_names = c('perfil', 'escuela', 'grupo', 'sexo', 'edad', 'percentil', 'rango', 'dx'), sign=0.05)
{
    merged_dataset <- factors %>%
        add_id_column_numero %>%
        merge(add_id_column_numero(scales), by='numero') %>%
        merge(add_id_column_numero(raven), by='numero') %>%
        merge(add_id_column_numero(columns_dataset), by='numero') %>%
        mutate(numero=as.numeric(numero)) %>%
        select(!starts_with('X'))

    tor <- merged_dataset %>%
        select(c(names(columns_dataset), -numero))
    torrest <- merged_dataset %>%
        select(!names(columns_dataset))


    #categorical_names <- c('perfil', 'escuela', 'grupo', 'sexo', 'edad', 'percentil', 'rango', 'dx')
    categorical_dataset <- merged_dataset %>%
        select(all_of(categorical_names)) %>%
        mutate(across(where(is.numeric), as.factor))

    return(list(tor, categorical_dataset))
}


library('tidytext')
library('widyr')
library('ggplot2')
library('dplyr')
library('tidyr')
library('tibble')
library('psych')
library(broom)
library(purrr)
library(tidyr)
library(dplyr)
