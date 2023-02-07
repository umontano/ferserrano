#==========================================
#Aggregated/Total variables Torrance
#==========================================
totals_torrance <- function()
{
torrance_csv_original <- read.csv('https://github.com/Laboratorio-CHyC/Temperament/raw/main/torrance1_2022.csv')

torrance_totals <- torrance_csv_original %>%
remove_rownames %>% 
tibble::column_to_rownames(var='identificador') %>%
mutate_if(is.numeric, ~replace_na(.,0)) %>%
mutate(originalidad=orig1 + orig2_1 + orig2_2 + orig2_3 + orig2_4 + orig2_5 + orig2_6 + orig2_7 + orig2_8 + orig2_9 + orig2_10 + orig3) %>%
mutate(fluidez=flui2 + flui3) %>%
mutate(elaboracion=elab1 + elab2 + elab3) %>%
mutate(flexibilidad=flex2 + flex3) %>%
mutate(creatividad=originalidad + fluidez + elaboracion + flexibilidad) %>%
select(originalidad, fluidez, elaboracion, flexibilidad, creatividad) %>%
data.frame

library(dplyr)
library(rmarkdown)
pc_crea_median <- median(torrance_percentil$perc_creatividad)
torrance_groups_perc <- torrance_percentil %>%
mutate(median_grp = case_when(
perc_creatividad > pc_crea_median ~ 'g2above75',
TRUE ~ 'g1below75'
)) %>%
mutate(three_groups = case_when(
perc_creatividad <= 65 ~ 'g1bbibelow65',
perc_creatividad > 86 ~ 'g3above86',
TRUE 
~ 'g2mid66_85'
)) %>%
select(median_grp, three_groups)
#torrance_groups_perc$three_groups
torrance_groups_perc$median_grp


return(torrance_totals)
}

#==========================================
#LOAD DATASETS
#BLOCK TOrrance
#==========================================
load_datasets_cbq_raven_torrance <- function()
{
torrance <- read.csv('https://github.com/Laboratorio-CHyC/Temperament/raw/main/torrance1_2022.csv')
rownames(torrance) <- torrance$identificador
torrance$numero <- gsub('.*(\\d{4})\\s*$', '\\1', torrance$identificador, perl=TRUE) 
torrance[, grep('^X|_|dibujo|titulo|observaciones|experimentadora|escuela|grupo|edad|sexo|identificador|Parte', names(torrance))] <- list(NULL)
#Removed problematic
torrance[, c('orig2_6')] <- list(NULL)
torrance <- data.frame(lapply(torrance, as.numeric))
torrance_raw <- torrance
torrance <- identify_and_make_na_outlaiers(torrance)
torrance <- data.frame(impute_any_dataset_mice(torrance))

#==========================================
#Load and impute scales datasets
#==========================================
#scales <- read.csv('~/p/tmfs/imp30/xCBQ_15DIMENSIONES.csv')[, -1]
scales <- read.csv('https://raw.githubusercontent.com/umontano/kar/master/mfs/mfs22cbq15dimensiones_imputado.csv')
row.names(scales) <- scales[, 1]
scales <- scales[, -1]

#==========================================
#Load and impute factors dataset
#==========================================
#factors <- read.csv('~/p/tmfs/imp30/xCBQ_3FACTORES.csv')[, -1]
factors <- read.csv('https://raw.githubusercontent.com/umontano/kar/master/mfs/mfs22cbq3factores_imputado.csv')
row.names(factors) <- factors[, 1]
factors <- factors[, -1]


#==========================================
#RAVEN BLOCK
#==========================================
var_raven  <- c('columna_a', 'columna_ab', 'columna_b', 'puntaje', 'dx')
#xxxxbbbbxxxx
#LOAD DATA
raven <- read.csv('https://raw.githubusercontent.com/Laboratorio-CHyC/Temperament/main/ferserrano2022_raven.csv', header=TRUE)
#Set indentificador as column names
rownames(raven) <- raven$identificador

#==========================================
#CLEAN OUTLAIERS AND IMPUTE RAVEN DATA
#==========================================
rav_to_impute  <- raven[, var_raven]
rav_to_impute  <- data.frame(lapply(rav_to_impute, as.numeric))
raven_raw <- rav_to_impute

rav_to_impute <- identify_and_make_na_outlaiers(rav_to_impute)
rav_to_impute <- impute_any_dataset_mice(rav_to_impute)

#==========================================
#CONTINUE PROCESSIN NOT PRINTING
#==========================================
raven[, var_raven] <- list(NULL)
raven  <- cbind(raven, rav_to_impute)


#==========================================
#Attach the original dx variable to cleaned raven
#==========================================
source('https://github.com/umontano/CBQ_comandos_SPSS_lab_ChyC/raw/main/CBQ_comandosSPSS_lab_CHyC.R')
raven_csv_original <- read.csv(raven_url)
raven$dx <- as.factor(raven_csv_original$dx)


#==========================================
#CLEAN OUTLAIERS IN CBQ39PARTICIPANTS AND TORRANCE TOTALS
#==========================================
iiii <- identify_and_make_na_outlaiers(scales)
i1 <- impute_any_dataset_mice(iiii)
scales <- i1
rownames(scales) <- cbqcsv$identificador
 attach(scales)
 factors$CE <- rowMeans(data.frame(attcon, lip,inh, per, attfoc, attshi) , na.rm=TRUE )
 factors$AN <- rowMeans(data.frame(sad, dis, fru, fea, sth) , na.rm=TRUE )
 factors$SU <- rowMeans(data.frame(shy, app, imp, hip, smi, act) , na.rm=TRUE )
 detach(scales)
 attach(factors)
 cem  <- median(CE)
 anm  <- median(AN)
 factors$perfil[CE >= cem & AN <  anm] <- 'easy'
 factors$perfil[CE >= cem & AN >= anm] <- 'intense'
 factors$perfil[CE <  cem & AN <  anm] <- 'disengaged'
 factors$perfil[CE <  cem & AN >= anm] <- 'risky'
 detach(factors)
 factors$perfil  <- as.factor(factors$perfil)

}



#Outlaiers cleanup
#==========================================
#CBQ CMOMMANDOS SPSS
#==========================================
source('https://raw.githubusercontent.com/umontano/CBQ_comandos_SPSS_lab_ChyC/main/CBQ_comandosSPSS_lab_CHyC.R')
#Clean outlaiers and impute questionnaire data
#con datos DE M F SERRANO (MFS)
#cbq(mfs)
#outlaiers_before_impute(mfs, number_of_imputations = 1, maximum_iterations = 1)
#imputed_cbq(mfs, number_of_imputations = 1, maximum_iterations = 1)

library(rmarkdown)
library(hugodown)
library(dplyr)

id_numbers_cbq <- gsub('.*(\\d{4})$', '\\1', row.names(items), perl = TRUE)
id_numbers_raven <- gsub('.*(\\d{4})$', '\\1', row.names(raven), perl = TRUE)
id_numbers_torrance <- gsub('.*(\\d{4})$', '\\1', torrance$identificador, perl = TRUE)

#NINNIOS QUE FALTAN
setdiff(id_numbers_cbq, id_numbers_torrance)
setdiff(id_numbers_torrance, id_numbers_cbq)
