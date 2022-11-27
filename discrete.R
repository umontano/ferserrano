#Attach the original dx variable to cleaned raven
#source('https://github.com/umontano/CBQ_comandos_SPSS_lab_ChyC/raw/main/CBQ_comandosSPSS_lab_CHyC.R')
#raven_csv_original <- read.csv(raven_url)
#raven$dx <- as.factor(raven_csv_original$dx)

#source('/a/ferserrano/categoric.R')
source('https://github.com/umontano/ferserrano/raw/main/categoric.R')


#using torrance partial variabels generate lm list
partor_categorical <- merged_categorical_and_torrance_totals(torrance)

#partial torrance varisables sent to lm_mapped
partialtor_results <- send_responses_to_predictors_lm(partor_categorical[[1]], partor_categorical[[2]], 0.05, TRUE)

print(partialtor_results)






library(dplyr)
data(mtcars)
complete_dataset <-  mtcars
complete_dataset <- cbind(torrance_percentil, torrance_csv_original)


response_column <- 'perc_creatividad'
grouping_column1 <- 'grupo'
grouping_column2 <- 'edad'

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
summarized_stats


#Next, add the group labels as a new variable in the data frame.
#summarized_stats$group <- c("a","b","c","d")

summarized_stats


library(ggplot2)
library(ggbeeswarm)

two.way.plot <- ggplot(complete_dataset, aes(get(grouping_column2), get(response_column), col = get(grouping_column2), group=get(grouping_column1))) +
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

return(two.way.plot)

}



  

one_way_anova_graph <- function(complete_dataset, response_column, grouping_column1, threshold_significance = 0.05)
{

complete_dataset[,grouping_column1] <- as.factor(complete_dataset[, grouping_column1])
#First, summarize the original data using grouping_column1 type and planting grouping_column2 as grouping variables.

summarized_stats <- complete_dataset %>%
  group_by(!! as.symbol(grouping_column1)) %>%
  summarise(mean = mean(!! as.symbol(response_column)))
summarized_stats


#Next, add the group labels as a new variable in the data frame.
#summarized_stats$group <- c("a","b","c","d")
summarized_stats


library(ggplot2)
library(ggbeeswarm)

two.way.plot <- ggplot(complete_dataset, aes(get(grouping_column1), get(response_column), col = get(grouping_column1))) +
  #geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.05, h = 0))
  geom_beeswarm() +
#Add the means and standard errors to the graph
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
geom_point(data=summarized_stats, aes(x=get(grouping_column1), y=mean)) +
#Make the graph ready for publication
#  theme_classic() +
  labs(title = paste(grouping_column1, '-groups Diferrences in ', response_column),
      x = grouping_column1,
      y = response_column)

return(two.way.plot)

}

