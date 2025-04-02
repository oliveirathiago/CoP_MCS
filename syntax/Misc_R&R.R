# load required packages
library(tidyverse)        # tidyverse package fot data managing (e.g., dplyr, ggplot2, ...)
library(haven)            # haven package to read foreign data sources
library(lavaan)           # lavaan package for latent variable modelling
library(mfx)              # mfx package for marginal effects in glm models
library(texreg)           # texreg package for regression displays

# load masterdataset
load("data/export/data_analysis.RData")


###################################################
#### ETHNICITY ####
### recording following comments from reviewers ###
###################################################

data.analysis <- data.analysis %>%
  mutate(
    ethnicity_grp1 = case_when(
      ethnicity %in% c(1, 2, 3, 4) ~ "White",
      ethnicity %in% c(14, 15, 16, 5, 6) ~ "Black",       # Black and mixed Black
      ethnicity %in% c(9, 10, 11, 12, 13, 7) ~ "Asian",       # Asian and mixed Asian
      ethnicity %in% c(17, 18, 8) ~ "Other",                # Arab, Other ethnic group, multiple Mixed categories
      TRUE ~ NA_character_                               # Everything else set to NA
    )
  )

# Create dummy variables for ethnic group
data.analysis <- data.analysis %>%
  mutate(
    ethnicity_asian = if_else(ethnicity_grp1 == "Asian", 1, 0),
    ethnicity_black = if_else(ethnicity_grp1 == "Black", 1, 0),
    ethnicity_other = if_else(ethnicity_grp1 == "Other", 1, 0)
  )

# data.analysis <- data.analysis %>%
#   mutate(
#     ethnicity_grp1 = case_when(
#       ethnicity %in% c(1, 2, 3, 4) ~ "White",
#       ethnicity %in% c(14, 15, 16) ~ "Black",       # Black 
#       ethnicity %in% c(9, 10, 11, 12,13) ~ "Asian",       # Asian 
#       ethnicity %in% c(5,6,7,8) ~ "Mixed",       #  Mixed
#       ethnicity %in% c(17, 18) ~ "Other",                # Arab, Other ethnic group
#       TRUE ~ NA_character_                               # Everything else set to NA
#     )
#   )
# 
# table(data.analysis$ethnicity_grp1, data.analysis$police.stopped_14=="TRUE")
# 
# # Create dummy variables for ethnic group
# data.analysis <- data.analysis %>%
#   mutate(
#     ethnicity_asian = if_else(ethnicity_grp1 == "Asian", 1, 0),
#     ethnicity_black = if_else(ethnicity_grp1 == "Black", 1, 0),
#     ethnicity_mixed = if_else(ethnicity_grp1 == "Mixed", 1, 0),
#     ethnicity_other = if_else(ethnicity_grp1 == "Other", 1, 0)
#   )

# Create the contingency table
eth_stop_table <- table(data.analysis$ethnicity_grp1, data.analysis$police.stopped_14 == "TRUE")

# Rename columns for clarity (optional)
colnames(eth_stop_table) <- c("Not_Stopped", "Stopped")

# View the table
eth_stop_table

# Run chi-squared test
chisq.test(eth_stop_table)

# Get chi-squared test
test <- chisq.test(eth_stop_table)

# Look at standardized residuals
round(test$stdres, 2)

