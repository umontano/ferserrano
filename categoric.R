
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


merged_lm_responses_to_predictors <- function(lmee_responses, lmee_categoricals, responses_names = names(lmee_responses), categoricals_names = names(lmee_categoricals), threshold_significance = 0.05, categorical_flag = FALSE)
{
tttt <- merge(add_id_column_numero(lmee_responses), add_id_column_numero(lmee_categoricals), by = 'numero')
cats <- tttt[, names(lmee_categoricals)]
#if(is.null(responses_names)) responses_names <- names(lmee_responses)
resp <- tttt[, responses_names]
results_list <- send_responses_to_predictors_lm(resp, cats, threshold_significance, categorical_flag)
if(length(results_list) > 0)
{
print(results_list)

	pairs_list <- find_list_significant_differences_in_multi_lm(results_list)
	anova_graphs_list <- anova_graphs_from_lm_pairs_list(tttt, pairs_list, threshold_significance = threshold_significance)
			#Show a single graph from the list of anova graphs
			#NOTE: marrangeGrobs does not work with these anova plots, find the replacement in stackoverflow.
			library('gridExtra')
			if(length(anova_graphs_list) > 1)
			{
			print(anova_graphs_list)
			#print(gridExtra::marrangeGrob(grobs = anova_graphs_list, ncol = 3, nrow = 2))
			}
			else {print(anova_graphs_list)}

return(results_list)
}
else
{
print('=== NO SIGNIFICANT RESEULTS ===')
return(NULL)
}

}


#================================================================
#FUNCTION TO EXTACT THE STRING_PAIRS FROM THE MULTI LM RETURNED LIST
#================================================================
find_list_significant_differences_in_multi_lm <- function(pairee_list)
{
	lm_pairs_list <<- NULL
	responses_names <- names(pairee_list)
	llll <- lapply(responses_names, 
		function(x) lapply(names(pairee_list[[x]]),
			function(y) { print(paste(x,y)); lm_pairs_list[[ length(lm_pairs_list) + 1 ]] <<- c(x,y) }
		) )
	return(lm_pairs_list)
}




anova_graphs_from_lm_pairs_list <- function(complete_dataset, lm_pairs_list, threshold_significance = 0.05)
{
	anova_graphs_list <- lapply(lm_pairs_list, 
		function(x)
			one_way_anova_graph(complete_dataset, x[1], x[2], threshold_significance = threshold_significance)
			)
	return(anova_graphs_list)
}

#threshold_significance <- 0.05
#complete_dataset <- cbind(torrance_percentil, torrance_csv_original)
#lm_pairs_list <- find_list_significant_differences_in_multi_lm(rrrr)
			#Send list of names to generate scatterplots
			#scatters_list <- lapply(pairs_list, scatterp_with_regression_lines, rows_dataset, columns_dataset)


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
library(rmarkdown)
library(hugodown)


categorical_names <- c('perfil', 'escuela', 'grupo', 'sexo', 'edad', 'percentil', 'rango', 'dx')
