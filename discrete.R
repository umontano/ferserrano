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

response_column <- 'mpg'
grouping_column1 <- 'vs'
grouping_column2 <- 'am'
two_way_anova_graph <- function(complete_dataset, response_column, grouping_column1, grouping_column2, threshold_significance = 0.05)
{
#Make a data frame with the group labels
#Now we need to make an additional data frame so we can add these groupwise differences to our graph.
#First, summarize the original data using grouping_column1 type and planting grouping_column2 as grouping variables.

ssss <- mtcars %>%
group_by(am, vs) %>%
summarize(prom = mean(mpg))
ssss


summarized_stats <- complete_dataset %>%
  group_by(!! as.symbol(grouping_column1), !! as.symbol(grouping_column2)) %>%
  summarise(mean = mean(!! as.symbol(response_column)))
summarized_stats


#Next, add the group labels as a new variable in the data frame.
summarized_stats$group <- c("a","b","c","d")
summarized_stats


library(ggplot2)

two.way.plot <- ggplot(complete_dataset, aes(x = !!as.symbol(grouping_column2), y = !!as.symbol(response_column), group=!!as.symbol(grouping_column1))) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

#Add the means and standard errors to the graph
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
geom_point(data=summarized_stats, aes(x=get(grouping_column2), y=mean))
two.way.plot

#Split up the data
#To show which groups are different from one another, use facet_wrap() to split the data up over the three types of grouping_column1. To add labels, use geom_text(), and add the group letters from the summarized_stats dataframe you made earlier.

two.way.plot <- two.way.plot +
  geom_text(data=summarized_stats, label=summarized_stats$group, vjust = -8, size = 5) +
  facet_wrap(~ get(grouping_column1))
two.way.plot


#Make the graph ready for publication
#In this step we will remove the grey background and add axis labels.

two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "response to grouping_column1 grouping_column2",
      x = "grouping_column2 (1=low grouping_column2, 2=high grouping_column2)",
      y = "response"

two.way.plot
}


