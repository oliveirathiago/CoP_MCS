# Load required packages
library(tidyverse)
library(haven)

# read data: age 14 sweep 6 | select relevant variables
data14 <- read_stata("data/import/Age 14 sweep 6/UKDA-8156-stata/stata/stata13/mcs6_cm_interview.dta") %>%
  mutate(MCSID_updated = paste(MCSID, "_", FCNUM00, sep = "")) %>%
  dplyr::select(MCSID, MCSID_updated, 
                
                # offending behaviour
                carrying_knife_14 = FCKNIF00, physical_aggression_hit_14 = FCHITT00, physical_aggression_weapon_14 = FCWEPN00,
                theft_taken_14 = FCSTOL00, theft_stolen_14 = FCSTLN00, graffiti_14 = FCSPRY00, damage_14 = FCDAMG00, 
                hacking_14 = FCHACK00, hacking_virus_14 = FCVIRS00, burglary_14 = FCROBH00, street_gang_14 = FCGANG00,
                
                # victimised
                victim_threatened_14 = FCVICG00, victim_violent_14 = FCVICA00, victim_weapon_14 = FCVICC00, 
                victim_stolen_14 = FCVICE00, victim_sexualy_14 = FCVICF0A,
                
                # educational performance
                self.concept_english_14 = FCENGL00, self.concept_maths_14 = FCMTHS00, self.concept_science_14 = FCSCIE00,
                educ.motivation_best_14 = FCSCBE00, educ.motivation_interesting_14 = FCSINT00, educ.motivation_unhappy_14 = FCSUNH00,
                educ.motivation_tired_14 = FCSTIR00, educ.motivation_waste_14 = FCSCWA00,
                
                # self-steem
                FCSATI00:FCGDSF00,
                
                # mood & feelings
                FCMDSA00:FCMDSM00,
                
                # well being
                FCSCWK00:FCLIFE00,
                
                # health & risk-taking behaviours
                cannabis_use_14 = FCCANB00, illegal.drugs_14 = FCOTDR00,
                smoking_14 = FCSMOK00, smoking_age.started_14 = FCAGSM00, smoking_vaping_14 = FCECIG00, smoking_friends_14 = FCSMFR00,
                drinking_ever_14 = FCALCD00, drinking_age.started_14 = FCALAG00, drinking_12mo_14 = FCALCN00, drinking_4we_14 = FCALNF00,
                
                # police contact
                police.stopped_14 = FCPOLS00, police.warning_14 = FCCAUT00, police.arrested_14 = FCARES00,
                
                # demographics
                sex = FCCSEX00, area_safety_14 = FCSAFD00, ethnicity_england = FCETHE00_R20, ethnicity_wales = FCETHW00_R5
  ) %>%
  mutate(not.EW = ethnicity_england == -1 & ethnicity_wales == -1) %>%
  filter(not.EW == F) %>%
  mutate(ethnicity = if_else(
    ethnicity_england != -1, ethnicity_england, ethnicity_wales
  ))

# read data: age 14 sweep 6 | derived scales | select relevant variables
data14_derived <- read_stata("data/import/Age 14 sweep 6/UKDA-8156-stata/stata/stata13/mcs6_cm_derived.dta") %>%
  mutate(MCSID_updated = paste(MCSID, "_", FCNUM00, sep = "")) %>%
dplyr::select(MCSID_updated, MCSID, 
              
              # cognitive development
              FEMOTION, FCONDUCT, FHYPER, FPEER, FPROSOC, FEBDTOT
              
)

# read data: age 14 sweep 6 | parent income brackets | select relevant variables
data14_parents <- read_stata("data/import/Age 14 sweep 6/UKDA-8156-stata/stata/stata13/mcs6_parent_income_brackets.dta")
  

# read data: age 17 sweep 7 | select relevant variables
data17 <- read_stata("data/import/Age 17 sweep 7/UKDA-8682-stata/stata/stata13/mcs7_cm_interview.dta") %>%
  mutate(MCSID_updated = paste(MCSID, "_", GCNUM00, sep = "")) %>%
  dplyr::select(MCSID_updated, MCSID, 
                
                # offending behaviour
                carrying_knife_17 = GCKNIF00, physical_aggression_hit_17 = GCHITT00, physical_aggression_weapon_17 = GCWEPN00,
                theft_taken_17 = GCSTOL00, theft_stolen_17 = GCSTLN00, graffiti_17 = GCSPRY00, damage_17 = GCDAMG00, 
                hacking_17 = GCHACK00, hacking_virus_17 = GCVIRS00, burglary_17 = GCROBH00, street_gang_17 = GCGANG00,
                
                # victimised
                victim_threatened_17 = GCVICG00, victim_violent_17 = GCVICA00, victim_weapon_17 = GCVICC00, 
                victim_stolen_17 = GCVICE00,
                
                # educational performance
                at.university_17 = GCUNIQ00, likely_university_17 = GCSTYR00,
                
                # self-steem
                GCSATI00:GCGDSF00,
                
                # depression
                depression_doctor = GCDEAN00, depression_age = GCDAGE00, depression_current = GCTRDE00, depression_ever = GCTRDV00,
                
                # health & risk-taking behaviours
                cannabis_use_17 = GCDRUA00,
                smoking_17 = GCSMOK00, smoking_age.started_17 = GCAGSM00, smoking_vaping_17 = GCVAPE00,
                drinking_ever_17 = GCALCD00, drinking_age.started_17 = GCALAG00, drinking_12mo_17 = GCALCN00, drinking_4we_17 = GCALNF00,
                
                # police contact
                police.stopped_17 = GCPOLS00, police.warning_17 = GCCAUT00, police.arrested_17 = GCARES00,
                
                # Demographics
                gender = GCGNID00, sexual_minority = GCSXID00,
  )

# read data: age 17 sweep 7 | select relevant variables
data17_derived <- read_stata("data/import/Age 17 sweep 7/UKDA-8682-stata/stata/stata13/mcs7_cm_derived.dta") %>%
  mutate(MCSID_updated = paste(MCSID, "_", GCNUM00, sep = "")) %>%
  dplyr::select(MCSID_updated, MCSID,
                
                # psychological distress
                psychological_distress = GDCKESSL,
                
                # mental wellbeing
                mental_wellbeing = GDWEMWBS,
                
                # internalising behaviour
                internalising_behaviour = GEMOTION_C,
                
                # externalising behaviour
                hyperactivity = GHYPER_C, conduct_problems = GCONDUCT_C
                
  )

# merge all three datasets
masterdataset <-
  data14 %>%
  left_join(data14_derived, by = "MCSID_updated") %>%
  left_join(data17, by = "MCSID_updated") %>%
  left_join(data17_derived, by = "MCSID_updated")

# save merged data files in RData and SPSS formats
save(masterdataset, file = "data/export/masterdataset.RData")
write_sav(masterdataset, "data/export/masterdataset.sav")

# data wrangling: making variables ready for analysis
data.analysis <-
  masterdataset %>%
  mutate(police.stopped_14 = police.stopped_14 == 1,
         police.arrested_14 = police.arrested_14 == 1,
         police.warning_14 = police.warning_14 == 1,
         male = gender == 1,
         white = case_when(
           ethnicity == 1 ~ TRUE,
           ethnicity == 2 ~ TRUE,
           ethnicity == 3 ~ TRUE,
           ethnicity == 4 ~ TRUE,
           TRUE ~ FALSE
         ),
         white_GB = ethnicity == 1,
         area_safety_14 = na_if(area_safety_14, -9),
         area_safety_14 = na_if(area_safety_14, -8),
         carrying_knife_14 = carrying_knife_14 == 1,
         physical_aggression_hit_14 = physical_aggression_hit_14 == 1,
         physical_aggression_weapon_14 = physical_aggression_weapon_14 == 1,
         theft_stolen_14 = theft_stolen_14 == 1,
         theft_taken_14 = theft_taken_14 == 1, # addition
         street_gang_14 = street_gang_14 == 1,
         cannabis_use_14 = cannabis_use_14 == 1,
         drinking_ever_14 = drinking_ever_14 == 1, # addition
         victim_threatened_14 = victim_threatened_14 == 1,
         victim_violent_14 = victim_violent_14 == 1,
         victim_weapon_14 = victim_weapon_14 == 1,
         victim_stolen_14 = victim_stolen_14 == 1,
         physical_aggression_hit_17 = physical_aggression_hit_17 == 1,
         physical_aggression_weapon_17 = physical_aggression_weapon_17 == 1,
         theft_stolen_17 = theft_stolen_17 == 1,
         theft_taken_17 = theft_taken_17 == 1,
         victim_threatened_17 = victim_threatened_14 == 1,
         victim_violent_17 = victim_violent_17 == 1,
         victim_weapon_17 = victim_weapon_17 == 1,
         victim_stolen_17 = victim_stolen_17 == 1
  )

# save data.analysis file in RData format
save(data.analysis, file = "data/export/data_analysis.RData")