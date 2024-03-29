
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
			library('cowplot')
			if(length(anova_graphs_list) > 1)
			{
#printing in the anova_graphs funtion itself
			#print(anova_graphs_list)
			#print(cowplot::plot_grid(plotlist = anova_graphs_list, ncol = 3, nrow = 2))
			#print(gridExtra::marrangeGrob(grobs = anova_graphs_list, ncol = 3, nrow = 2))
			}
			else {
				#print(anova_graphs_list)
				}

return(results_list)
}
else
{
print('=== NO SIGNIFICANT RESULTS ===')
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




#================================================================
#ANOVA from pairs list
#================================================================
anova_graphs_from_lm_pairs_list <- function(complete_dataset, lm_pairs_list, threshold_significance = 0.05)
{
	anova_graphs_list <- lapply(lm_pairs_list, 
		function(x)
			one_way_anova_graph(complete_dataset, x[1], x[2], threshold_significance = threshold_significance)
			)
	return(anova_graphs_list)
}


#================================================================
two_way_anova_graph <- function(complete_dataset, response_column, grouping_column1, grouping_column2, threshold_significance = 0.05)
{

#Make a data frame with the group labels
#Now we need to make an additional data frame so we can add these groupwise differences to our graph.
#First, summarize the original data using grouping_column1 type and planting grouping_column2 as grouping variables.
complete_dataset[,grouping_column1] <- as.factor(complete_dataset[, grouping_column1])
complete_dataset[,grouping_column2] <- as.factor(complete_dataset[, grouping_column2])

summarized_stats <- complete_dataset %>%
  group_by(!! as.symbol(grouping_column1), !! as.symbol(grouping_column2)) %>%
  summarise(mean = mean(!! as.symbol(response_column)))
print('=== GROUPS DESCRIPTIVE STATISTICS ===')
print(summarized_stats)


#Next, add the group labels as a new variable in the data frame.
#summarized_stats$group <- c("a","b","c","d")

summarized_stats


library(ggplot2)
library(ggbeeswarm)

gganova <- ggplot(complete_dataset, aes(get(grouping_column2), get(response_column), col = get(grouping_column2), group=get(grouping_column1))) +
  #geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.05, h = 0))
  geom_beeswarm() +
#Add the means and standard errors to the graph
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
geom_point(data=summarized_stats, aes(x=get(grouping_column2), y=mean)) +
#Split up the data
#  geom_text(data=summarized_stats, label=summarized_stats$group, vjust = -8, size = 5) +
  facet_wrap(~ get(grouping_column1)) +
#Make the graph ready for publication
#  theme_classic() +
  labs(title = paste(grouping_column2, ' Diferrences in ', response_column, '\n Faceted by ', grouping_column1),
      x = grouping_column2,
      y = response_column)

return(gganova)

}



  

#================================================================
one_way_anova_graph <- function(complete_dataset, response_column, grouping_column1, threshold_significance = 0.05, printing_flag = TRUE)
{
complete_dataset[,grouping_column1] <- as.factor(complete_dataset[, grouping_column1])
#Summarize the original data using grouping_column1 type and planting grouping_column2 as grouping variables.

library(dplyr)
#complete_dataset <- iris
#grouping_column1 <- 'Species'
#response_column <- 'Sepal.Length'
summarized_stats <- complete_dataset %>%
  group_by(!! as.symbol(grouping_column1)) %>%
  summarise(mean = mean(!! as.symbol(response_column)), sd = sd(get(response_column)), num = n())
#summarized_stats



library(ggplot2)
library(ggbeeswarm)
#Make graph
gganova <- ggplot(complete_dataset, aes(get(grouping_column1), get(response_column), col = get(grouping_column1))) +
  #geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.05, h = 0)) +
  geom_beeswarm(cex = 1.5, pch = 1.0) +
#Add the means and standard errors to the graph
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
geom_point(data=summarized_stats, aes(x=get(grouping_column1), y=mean)) +
#Make the graph ready for publication
#  theme_classic() +
  labs(title = paste(grouping_column1, '-groups Diferrences in ', response_column),
      x = grouping_column1,
      y = response_column)

	if(printing_flag)
		{
		print('=== GROUP DIFFERENCES GRAPH ===')
		print(gganova)
		print(paste0('=== DESCRIPTIVE STATS OF ', response_column, ' IN ', grouping_column1, ' GROUPS ==='))
		print(summarized_stats)
		}

return(gganova)
}


descriptives_skim_boxplot_histo_polygon <- function(descriptee_dataset)
{
#baseline_dataset <- data.frame(lapply(baseline_dataset, as.numeric))
library(skimr)
library('ggplot2')
library('dplyr')
library('tidyr')

#Compute bw as the optimal binwidth for histogram
x <- descriptee_dataset[, 3]
bw <- 2 * IQR(x) / length(x)^(1/3)

skimmed_dataset <- descriptee_dataset %>%
	skim %>%
	#select(-skim_type, -n_missing, -complete_rate, -factor.ordered, -factor.n_unique, -factor.top_counts)
	select(-n_missing, -complete_rate) %>%
	rename(var = skim_variable, mean = numeric.mean, sd = numeric.sd, p0 = numeric.p0, p25 = numeric.p25, p50 = numeric.p50, p75 = numeric.p75, p100 = numeric.p100, hist = numeric.hist)


#lambda definition pipeline

long <- . %>% 
	pivot_longer( cols=everything(),
		names_to='variable',
		values_to='value')

#Creates longtidy format
long_dataset     <- descriptee_dataset %>% long

ggboxplot <- ggplot(long_dataset, aes(variable, value, fill = variable)) +
geom_boxplot()


ggbar <- ggplot(long_dataset, aes(value, fill=variable)) +
#geom_bar(alpha=0.5, width=0.99) +
geom_histogram(binwidth = bw, col = 'black') +
facet_wrap(~ variable, scales='free')

ggpolygon <- ggplot(long_dataset, aes(value, fill=variable, col=variable)) +
#geom_freqpoly(alpha=0.5, binwidth=4) +
geom_freqpoly(binwidth = bw * 1) +
facet_wrap(~ variable, scales='free')

print(skimmed_dataset)
print(ggboxplot)
print(ggbar)
print(ggpolygon)
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
library(rmarkdown)
library(hugodown)
