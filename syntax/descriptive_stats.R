library(psych)
library(dplyr)

# load dataset
load("data/export/data_analysis.RData")


# categorical variables
data1 <- data.analysis[, c("non.white", "male", "police.stopped_14", "carrying_knife_14",
                          "offending_violent_14", "offending_violent_17", 
                          "offending_theft_14", "offending_theft_17", 
                          "street_gang_14", 
                          "cannabis_use_14",  "drinking_ever_14",
                           "likely_uni_17.quartile", 
                           "depression_current", "depression_14older" )]
                        

# generate stats table for categorical variables
stats.table1 <- data.frame(
  Count = colSums(data1, na.rm = TRUE), # Count of TRUE values
  Row_Percentage = paste0(round((colSums(data1, na.rm = TRUE) / nrow(data1)) * 100, 2),"%") # Row percentage
)

stats.table1


# continious/interval-level variables
data2 <- data.analysis[, c("area_safety_14","cannabis_use_17",  "victimisation_14", "edu_irt_14",
                           "self_esteem_change","psychological_distress", "mental_wellbeing",
                           "internalising_behaviour", "hyperactivity", "conduct_problems")]
                          

# generate stats table for continious/interval-level variables
stats.table2 <- data.frame(
  Mean = round(sapply(data2, mean, na.rm = TRUE), 2),
  Std = round(sapply(data2, sd, na.rm = TRUE), 2),
  Min = round(sapply(data2, min, na.rm = TRUE),2),
  Max = round(sapply(data2, max, na.rm = TRUE),2),
  N = sapply(data2, function(x) sum(!is.na(x)))
)


stats.table2


















