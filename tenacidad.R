
create_indexed_label <- function(indices, label)
{
  indexed_label <- paste0(label, indices)
  return(indexed_label)
}

create_subset_dataframe <- function(indices, label, input_df)
{
  # Create indexed labels using lapply
  indexed_labels <- lapply(indices, function(index) paste0(label, index))
  # Filter variables in input dataframe that match indexed labels
  matched_vars <- grep(paste(indexed_labels, collapse = "|"), names(input_df), value = TRUE)
  # Create output dataframe containing matched variables
  output_dataframe <- input_df[, matched_vars, drop = FALSE]
  return(output_dataframe)
}


######################################################################
######################################################################
## LOAD SCRIPT FOR REGRESSION ANALYZING GROUPS AND CATEGORICAL VARIABLES. INCULDING MAKING DESCRIPTIOVE BOXPLOTS AND HISTOGRAMS
#source('https://github.com/umontano/ferserrano/raw/main/categoric.R')
## LOAD DATASET for PERSEVERANCIA PERFECCIONISMO ALTAS CAPACIDADES
#https://github.com/Mafer-sg/Altas-Capacidades.git


tenacidad <- read.csv('https://github.com/Mafer-sg/Altas-Capacidades/raw/main/cpt.csv')

tenacidad_categorical_names <- c('escuela', 'grado', 'sexo', 'edad')

## SÓLO SE ANALIZARÁN ESTAS DOS PRUEBAS QUE YA ESTÁN VALIDADAS PARA LOS INFORMES: grit (va de grit1-grit8). 
## RANGOS: BAJO (1 - 4.5), MEDIO (4.6 - 6.1) Y ALTO (6.2 – 8.0) 
## EMPF (EMPF1-EMPF35). RANGOS: BAJO (5 – 17), MEDIO (18 – 20), ALTO (21 – 22) Y SUPERIOR (23 – 25). # Call the function to create a subset dataframe

## SELECTING THE EMPF VARIABLES
indices <- c(1:35)
label <- "empf"
empf1_35 <- create_subset_dataframe(indices, label, tenacidad)
names(empf1_35)
# EMPF, CALCULATE ROW MEANS USING ROWMEANS WITH NA.RM = TRUE
empf_means <- rowMeans(empf1_35, na.rm = TRUE)
names(empf_means)
head(empf_means)

## SELECTING THE GRIT VARIABLES
indices <- c(1:8)
label <- "grit"
grit1_35 <- create_subset_dataframe(indices, label, tenacidad)
names(grit1_35)
# EMPF, CALCULATE ROW MEANS USING ROWMEANS WITH NA.RM = TRUE
grit_means <- rowMeans(grit1_35, na.rm = TRUE)

names(grit_means)
head(grit_means)

# CREATE NUMERICAL  AND CATEGORICAL DATAFAMES
categorical_data <- NULL
numeric_data <- NULL
categorical_data <- tenacidad[, tenacidad_categorical_names]
numeric_data <- data.frame(empf_means, grit_means)

print('==EMPF MEAN==:')
print(mean(empf_means))
print('==EMPF SD==:')
print(sd(empf_means))

print('==GRIT MEAN==:')
print(mean(grit_means))
print('==GRIT SD==:')
print(sd(grit_means))
