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





mean.yield.data <- crop.data %>%
  group_by(fertilizer, density) %>%
  summarise(
      yield = mean(yield)
  )

#Next, add the group labels as a new variable in the data frame.

mean.yield.data



two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

#Add the means and standard errors to the graph
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))

Split up the data

To show which groups are different from one another, use facet_wrap() to split the data up over the three types of fertilizer. To add labels, use geom_text(), and add the group letters from the mean.yield.data dataframe you made earlier.

two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer)

two.way.plot



