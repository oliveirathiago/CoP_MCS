library(tidyverse)
library(haven)
library(texreg)
library(lavaan)


# load dataset
load("data/export/data_analysis.RData")

##############

# assigning new variables to the dataset
data.analysis <-
  data.analysis %>%
  mutate(
    # SELF-ESTEEM
    mutate(across(c(FCSATI00:FCGDSF00, GCSATI00:GCGDSF00), ~case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ NA_real_
    ))),
    # offending by age 14 | summative scale:
    offending_sum_14 = physical_aggression_hit_14 + physical_aggression_weapon_14 + theft_taken_14 + theft_stolen_14,
    # violent behaviour by 14:
    offending_violent_14 = case_when(physical_aggression_hit_14 == 1 | physical_aggression_weapon_14 == 1 ~ TRUE, TRUE ~ FALSE),
    # non-violent behaviour by 14:
    offending_theft_14 = case_when(theft_taken_14 == 1 | theft_stolen_14 == 1 ~ TRUE, TRUE ~ FALSE),
    # offending by age 17 | summative scale:
    offending_sum_17 = physical_aggression_hit_17 + physical_aggression_weapon_17 + theft_taken_17 + theft_stolen_17,
    # violent behaviour by 17:
    offending_violent_17 = case_when(physical_aggression_hit_17 == 1 | physical_aggression_weapon_17 == 1 ~ TRUE, TRUE ~ FALSE),
    # non-violent behaviour by 17:
    offending_theft_17 = case_when(theft_taken_17 == 1 | theft_stolen_17 == 1 ~ TRUE, TRUE ~ FALSE),
    # victimisation by age 14 | summative scale:
    victimisation_14 = victim_threatened_14 + victim_violent_14 + victim_weapon_14 + victim_stolen_14,
    # victimisation by age 17 | summative scale:
    victimisation_17 = victim_threatened_17 + victim_violent_17 + victim_weapon_17 + victim_stolen_17,
    # changing reference of the ethnicity-binary variable
    non.white = white_GB == F,
    # manually computing interaction groups
    stopped_white = case_when(police.stopped_14 == TRUE & white_GB == TRUE ~ TRUE, TRUE ~ FALSE),
    stopped_non.white = case_when(police.stopped_14 == TRUE & white_GB == FALSE ~ TRUE, TRUE ~ FALSE),
    not.stopped_non.white = case_when(police.stopped_14 == FALSE & white_GB == FALSE ~ TRUE, TRUE ~ FALSE),
    not.stopped_white = case_when(police.stopped_14 == FALSE & white_GB == TRUE ~ TRUE, TRUE ~ FALSE)
  ) 

#########################################
### Predicting changes in self-esteem ###
#########################################

auto_sem <-
  '
  self.esteem_14 =~ a * FCSATI00 + b * FCGDQL00 + c * FCDOWL00 + d * FCVALU00 + e * FCGDSF00
  self.esteem_17 =~ a * GCSATI00 + b * GCGDQL00 + c * GCDOWL00 + d * GCVALU00 + e * GCGDSF00
  
  FCSATI00 ~~ GCSATI00
  FCGDQL00 ~~ GCGDQL00
  FCDOWL00 ~~ GCDOWL00
  FCVALU00 ~~ GCVALU00
  FCGDSF00 ~~ GCGDSF00
  
  self.esteem_17 ~ self.esteem_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + drinking_ever_14 + victimisation_14 + police.stopped_14 + offending_violent_14 + offending_theft_14
  ' %>%
  sem(data = data.analysis, estimator = "MLR", missing = "ML", std.lv = T)

auto_sem_int <-
  '
  self.esteem_14 =~ a * FCSATI00 + b * FCGDQL00 + c * FCDOWL00 + d * FCVALU00 + e * FCGDSF00
  self.esteem_17 =~ a * GCSATI00 + b * GCGDQL00 + c * GCDOWL00 + d * GCVALU00 + e * GCGDSF00
  
  FCSATI00 ~~ GCSATI00
  FCGDQL00 ~~ GCGDQL00
  FCDOWL00 ~~ GCDOWL00
  FCVALU00 ~~ GCVALU00
  FCGDSF00 ~~ GCGDSF00
  
  self.esteem_17 ~ stopped_white + stopped_non.white + not.stopped_non.white + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14
  ' %>%
  sem(data = data.analysis, estimator = "MLR", missing = "ML", std.lv = T)


data.analysis_CFA <-
  data.analysis %>%
  dplyr::select(MCSID_updated,
                se11 = FCSATI00, se21 = FCGDQL00, se31 = FCDOWL00, se41 = FCVALU00, se51 = FCGDSF00,
                se12 = GCSATI00, se22 = GCGDQL00, se32 = GCDOWL00, se42 = GCVALU00, se52 = GCGDSF00) %>%
  pivot_longer(cols = c(se11:se52)) %>%
  mutate(wave = substr(name, 4, 4),
         var = substr(name, 1, 3)) %>%
  pivot_wider(id_cols = c(MCSID_updated, wave),
              names_from = var,
              values_from = value) %>%
  filter(!rowSums(is.na(dplyr::select(., c('se1', 'se2', 'se3', 'se4', 'se5')))) == length(c('se1', 'se2', 'se3', 'se4', 'se5')))

cfa_se <- 'se =~ se1 + se2 + se3 + se4 + se5' %>% cfa(data = data.analysis_CFA, estimator = "MLR", missing = "ML", std.lv = T)

data.analysis_CFA <-
  data.analysis_CFA %>%
  mutate(self_esteem = lavPredict(cfa_se)) %>%
  dplyr::select(MCSID_updated, wave, self_esteem) %>%
  pivot_wider(id_cols = MCSID_updated,
              names_from = wave,
              values_from = self_esteem) %>%
  mutate(self_esteem_change = `2` - `1`)

data.analysis <-
  data.analysis %>%
  left_join(data.analysis_CFA %>% dplyr::select(MCSID_updated, self_esteem_change))

m.noint <- lm(self_esteem_change ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                        drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14, data.analysis)
m.intwhite <- lm(self_esteem_change ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14, data.analysis)
m.intnonwhite <- lm(self_esteem_change ~ police.stopped_14 * non.white + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14, data.analysis)

list(
  "No interaction" = m.noint,
  "With an interaction term" = m.intwhite
) %>% wordreg(ci.force = T, file = "tables/mentalheatl.docx")

data.plot <-
  tibble(
    coef = c(coef(m.noint)[2], coef(m.intwhite)[2], coef(m.intnonwhite)[2]),
    ci.low = c(confint(m.noint)[2,1], confint(m.intwhite)[2,1], confint(m.intnonwhite)[2,1]),
    ci.upp = c(confint(m.noint)[2,2], confint(m.intwhite)[2,2], confint(m.intnonwhite)[2,2]),
    var = c("Changes in self-esteem") %>% rep(3),
    main = c("all", "non-white", "white")
  )

# produce plot: interaction
plot.results_interaction <- ggplot(data.plot, aes(y = coef, x = var, group = main, colour = main)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp), width = .25, position = position_dodge(), size = .8, lwd = .9, show.legend = T) +
  geom_point(position = position_dodge(width = .25)) + 
  ylim(-.5,.5) +
  geom_hline(yintercept = 0, size = .75, color = 'darkgray') +
  #coord_flip() +
  ylab("") + xlab("") +
  ggtitle("Effects of being stopped by the police by age 14 \n on changes in self-esteem* by age 17 \n") +
  labs(caption = 'Linear change score model estimated
                  Coefficients and 95% confidence intervals reported. \n
                  * Scores of self-esteem derived from a pooled CFA model.
                  Change scores between ages 17 and 14 computed.\n
                  Models control for gender, perception of safety, gang membership by age 14,
                  cannabis use by age 14, drinking by age 14, and victimisation by age 14. \n

                  n = 7524 for both models') +
  guides(colour = guide_legend(title = "")) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 8),
        axis.text.x = element_text(vjust = 125, colour = "#3C3C3C", size = 10.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 12),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Set1",
                     limits = c('white', 'non-white', 'all'),
                     labels = c('White', "Non-White", 'All respondents'))
  
pdf('plots/mental health plots.pdf')
plot.results_interaction
dev.off()