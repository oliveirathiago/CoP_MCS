library(tidyverse)
library(haven)
library(texreg)
library(lavaan)
library(mfx)

# load dataset
load("data/export/data_analysis.RData")

##############

# assigning new variables to the dataset
data.analysis <-
  data.analysis %>%
  mutate(
    # SELF-ESTEEM
    across(c(FCSATI00:FCGDSF00, GCSATI00:GCGDSF00), ~case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ NA_real_
    )),
    #
    across(c(depression_age, psychological_distress, mental_wellbeing, internalising_behaviour, hyperactivity, conduct_problems), ~case_when(. < 0 ~ NA_real_, TRUE ~ .)),
    across(c(depression_doctor, depression_current, depression_ever), ~ case_when(. == 1 ~ TRUE, TRUE ~ FALSE)),
    depression_14older = case_when(depression_age > 13 ~ TRUE, TRUE ~ FALSE),
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
    # stopped_white = case_when(police.stopped_14 == TRUE & white_GB == TRUE ~ TRUE, TRUE ~ FALSE),
    # stopped_non.white = case_when(police.stopped_14 == TRUE & white_GB == FALSE ~ TRUE, TRUE ~ FALSE),
    # not.stopped_non.white = case_when(police.stopped_14 == FALSE & white_GB == FALSE ~ TRUE, TRUE ~ FALSE),
    # not.stopped_white = case_when(police.stopped_14 == FALSE & white_GB == TRUE ~ TRUE, TRUE ~ FALSE)
    stopped_white = police.stopped_14 == TRUE & white_GB == TRUE,
    stopped_black = police.stopped_14 == TRUE & ethnicity_black == 1,
    not.stopped_black = police.stopped_14 == FALSE & ethnicity_black == 1,
    not.stopped_white = police.stopped_14 == FALSE & white_GB == TRUE
    )
    


#############################
### Measuring self-esteem ###
#############################


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
  left_join(data.analysis_CFA %>% dplyr::select(MCSID_updated, self_esteem_change, self_esteem_14 = `1`, self_esteem_17 = `2`))

## Models for self-esteem
m.noint <- lm(self_esteem_change ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                        drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14, data.analysis)
m.intwhite <- lm(self_esteem_change ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14, data.analysis)
m.intnonwhite <- lm(self_esteem_change ~ police.stopped_14 * ethnicity_black +  ethnicity_asian + ethnicity_black +
                      ethnicity_mixed + ethnicity_other + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14, data.analysis)

## Models for depression
m.depression_current.noint <- glm(depression_current ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                    drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                  data.analysis, family = binomial(link = 'logit'))
m.depression_current.int <- glm(depression_current ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                    drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                  data.analysis, family = binomial(link = 'logit'))
m.depression_current.int_non.white <- glm(depression_current ~ police.stopped_14 * ethnicity_black + ethnicity_asian + ethnicity_black +
                                            ethnicity_mixed + ethnicity_other + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                  drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                data.analysis, family = binomial(link = 'logit'))

m.depression_14older.noint <- glm(depression_14older ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                    drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                  data.analysis, family = binomial(link = 'logit'))
m.depression_14older.int <- glm(depression_14older ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                  drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                data.analysis, family = binomial(link = 'logit'))
m.depression_14older.int_non.white <- glm(depression_14older ~ police.stopped_14 *ethnicity_black + ethnicity_asian + ethnicity_black +
                                            ethnicity_mixed + ethnicity_other + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                  drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                data.analysis, family = binomial(link = 'logit'))

## Models for psychological_distress
m.psychological_distress.noint <- lm(psychological_distress ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                    drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                  data.analysis)
m.psychological_distress.int <- lm(psychological_distress ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                  drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                data.analysis)
m.psychological_distress.int_non.white <- lm(psychological_distress ~ police.stopped_14 * ethnicity_black + ethnicity_asian + ethnicity_black +
                                               ethnicity_mixed + ethnicity_other + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                     drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                   data.analysis)

## Models for mental_wellbeing
m.mental_wellbeing.noint <- lm(mental_wellbeing ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                       drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                     data.analysis)
m.mental_wellbeing.int <- lm(mental_wellbeing ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                     drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                   data.analysis)
m.mental_wellbeing.int_non.white <- lm(mental_wellbeing ~ police.stopped_14 * ethnicity_black + ethnicity_asian + ethnicity_black +
                                         ethnicity_mixed + ethnicity_other + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                               drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                             data.analysis)

## Models for internalising_behaviour
m.internalising_behaviour.noint <- lm(internalising_behaviour ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                 drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                               data.analysis)
m.internalising_behaviour.int <- lm(internalising_behaviour ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                               drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                             data.analysis)
m.internalising_behaviour.int_non.white <- lm(internalising_behaviour ~ police.stopped_14 * ethnicity_black + ethnicity_asian + ethnicity_black +
                                                ethnicity_mixed + ethnicity_other + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                      drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                                    data.analysis)

## Models for hyperactivity
m.hyperactivity.noint <- lm(hyperactivity ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                 drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                               data.analysis)
m.hyperactivity.int <- lm(hyperactivity ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                               drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                             data.analysis)
m.hyperactivity.int_non.white <- lm(hyperactivity ~ police.stopped_14 * ethnicity_black + ethnicity_asian + ethnicity_black +
                                      ethnicity_mixed + ethnicity_other + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                            drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                          data.analysis)

## Models for conduct_problems
m.conduct_problems.noint <- lm(conduct_problems ~ police.stopped_14 + white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                                 drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                               data.analysis)
m.conduct_problems.int <- lm(conduct_problems ~ police.stopped_14 * white_GB + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                               drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                             data.analysis)
m.conduct_problems.int_non.white <- lm(conduct_problems ~ police.stopped_14 * ethnicity_black + ethnicity_asian + ethnicity_black +
                                         ethnicity_mixed + ethnicity_other + male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + 
                               drinking_ever_14 + victimisation_14 + offending_violent_14 + offending_theft_14 + self_esteem_14,
                             data.analysis)

list(
  "Self esteem" = m.noint,
  "Self esteem" = m.intwhite,
  "Depression" = m.depression_14older.noint,
  "Depression" = m.depression_14older.int,
  "Psychological distress" = m.psychological_distress.noint,
  "Psychological distress" = m.psychological_distress.int,
  "Mental wellbeing" = m.mental_wellbeing.noint,
  "Mental wellbeing" = m.mental_wellbeing.int,
  "Internalising behaviour" = m.internalising_behaviour.noint,
  "Internalising behaviour" = m.internalising_behaviour.int,
  "Hyperactivity" = m.hyperactivity.noint,
  "Hyperactivity" = m.hyperactivity.int,
  "Conduct problems" = m.conduct_problems.noint,
  "Conduct problems" = m.conduct_problems.int
) %>% wordreg(#ci.force = T, 
              file = "tables/4_mentalhealth_coefficients.docx",
              custom.gof.rows = list("Interaction term" = c("No", "Yes") %>% rep(7)))

m.depression_14older.noint_prob <- logitmfx(m.depression_14older.noint, data = data.analysis, atmean = T)
m.depression_14older.int_prob <- logitmfx(m.depression_14older.int, data = data.analysis, atmean = T)
m.depression_14older.int_non.white_prob <- logitmfx(m.depression_14older.int_non.white, data = data.analysis, atmean = T)

data.plot <-
  tibble(
    var = c('Self Esteem', 'Depression', 'Psychological Distress', 'Mental wellbeing', 'Internalised Behaviour',
            'Hyperactivity', 'Conduct Problems') %>% rep(each = 3),
    coef = c(
      # self esteem
      coef(m.noint)[2], coef(m.intwhite)[2], coef(m.intnonwhite)[2],
      # depression
      m.depression_14older.noint_prob$mfxest[1,1], m.depression_14older.int_prob$mfxest[1,1], m.depression_14older.int_non.white_prob$mfxest[1,1],
      # psychological distress
      coef(m.psychological_distress.noint)[2], coef(m.psychological_distress.int)[2], coef(m.psychological_distress.int_non.white)[2],
      # mental wellbeing
      coef(m.mental_wellbeing.noint)[2], coef(m.mental_wellbeing.int)[2], coef(m.mental_wellbeing.int_non.white)[2],
      # internalising behaviour
      coef(m.internalising_behaviour.noint)[2], coef(m.internalising_behaviour.int)[2], coef(m.internalising_behaviour.int_non.white)[2],
      # hyperactivity
      coef(m.hyperactivity.noint)[2], coef(m.hyperactivity.int)[2], coef(m.hyperactivity.int_non.white)[2],
      # conduct problems
      coef(m.conduct_problems.noint)[2], coef(m.conduct_problems.int)[2], coef(m.conduct_problems.int_non.white)[2]
      ),
    ci.low = c(
      # self esteem
      confint(m.noint)[2,1], confint(m.intwhite)[2,1], confint(m.intnonwhite)[2,1],
      # depression
          m.depression_14older.noint_prob$mfxest[1,1] - 1.96 * m.depression_14older.noint_prob$mfxest[1,2], 
          m.depression_14older.int_prob$mfxest[1,1] - 1.96 * m.depression_14older.int_prob$mfxest[1,2], 
          m.depression_14older.int_non.white_prob$mfxest[1,1] - 1.96 * m.depression_14older.int_non.white_prob$mfxest[1,2],
      # psychological distress
      confint(m.psychological_distress.noint)[2,1], confint(m.psychological_distress.int)[2,1], confint(m.psychological_distress.int_non.white)[2,1],
      # mental wellbeing
      confint(m.mental_wellbeing.noint)[2,1], confint(m.mental_wellbeing.int)[2,1], confint(m.mental_wellbeing.int_non.white)[2,1],
      # internalising behaviour
      confint(m.internalising_behaviour.noint)[2,1], confint(m.internalising_behaviour.int)[2,1], confint(m.internalising_behaviour.int_non.white)[2,1],
      # hyperactivity
      confint(m.hyperactivity.noint)[2,1], confint(m.hyperactivity.int)[2,1], confint(m.hyperactivity.int_non.white)[2,1],
      # conduct problems
      confint(m.conduct_problems.noint)[2,1], confint(m.conduct_problems.int)[2,1], confint(m.conduct_problems.int_non.white)[2,1]
      ),
    ci.upp = c(
      # self esteem
      confint(m.noint)[2,2], confint(m.intwhite)[2,2], confint(m.intnonwhite)[2,2],
      # depression
          m.depression_14older.noint_prob$mfxest[1,1] + 1.96 * m.depression_14older.noint_prob$mfxest[1,2], 
          m.depression_14older.int_prob$mfxest[1,1] + 1.96 * m.depression_14older.int_prob$mfxest[1,2], 
          m.depression_14older.int_non.white_prob$mfxest[1,1] + 1.96 * m.depression_14older.int_non.white_prob$mfxest[1,2],
      # psychological distress
      confint(m.psychological_distress.noint)[2,2], confint(m.psychological_distress.int)[2,2], confint(m.psychological_distress.int_non.white)[2,2],
      # mental wellbeing
      confint(m.mental_wellbeing.noint)[2,2], confint(m.mental_wellbeing.int)[2,2], confint(m.mental_wellbeing.int_non.white)[2,2],
      # internalising behaviour
      confint(m.internalising_behaviour.noint)[2,2], confint(m.internalising_behaviour.int)[2,2], confint(m.internalising_behaviour.int_non.white)[2,2],
      # hyperactivity
      confint(m.hyperactivity.noint)[2,2], confint(m.hyperactivity.int)[2,2], confint(m.hyperactivity.int_non.white)[2,2],
      # conduct problems
      confint(m.conduct_problems.noint)[2,2], confint(m.conduct_problems.int)[2,2], confint(m.conduct_problems.int_non.white)[2,2]
    ),
    main = c("all", "black", "white") %>% rep(7)
  ) %>% 
  mutate(var = factor(var, levels = c('Self Esteem', 'Depression', 'Psychological Distress', 'Mental wellbeing', 'Internalised Behaviour',
                                      'Hyperactivity', 'Conduct Problems') %>% rev))

# produce plot: interaction
plot.results_interaction <- ggplot(data.plot, aes(y = coef, x = var, group = main, colour = main)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp), width = .5, position = position_dodge(), size = .75, lwd = .5, show.legend = T) +
  geom_point(position = position_dodge(width = .5), lwd = .5, size = .75) + 
  ylim(-1.5,1.75) +
  geom_hline(yintercept = 0, size = .2, color = 'darkgray') +
  coord_flip() +
  ylab("") + xlab("") +
  ggtitle("Effects of being stopped by the police by age 14 \n on various mental health indicators by age 17 \n") +
  labs(caption = 'Each row in the y-axis indicates a different dependent variable for
                  which two models (with and without an interaction term) were estimated.
                  In total, results from 14 models are displayed in this Figure.\n
                  Models for the dependent variable "Self Esteem" are linear change score 
                  models, with self-esteem scores derived from a pooled CFA model.
                  Change scores between ages 17 and 14 computed.\n
                  Models for the dependent variable "Depression" are binomial logistic
                  regression models. Estimates displayed are marginal effects at the
                  mean (in probability scale).\n
                  Models for the other five dependent variables (psychological distress,
                  mental wellbeing, internalised behaviour, hyperactivity, and conduct
                  problems) are OLS models using psychometric derived scales.\n
                  95% confidence intervals reported. Models control for self-esteem by
                  age 14 (except for the self-esteem models, as those are linear change
                  score models). All models control for gender, perception of safety, 
                  gang membership by age 14, cannabis use by age 14, drinking by age 14, 
                  and victimisation by age 14. \n

                  n = {7179; 7410} across all 14 models') + 
  guides(colour = guide_legend(title = "")) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 11),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8, margin = margin(0, 0, 0, 0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 12),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) +
  theme(aspect.ratio = 1.5) +
  scale_color_brewer(palette = "Set1",
                     limits = c('white', 'black', 'all'),
                     labels = c('White', "Black", "All"))


pdf('plots/4_mental_health_interactions.pdf')
plot.results_interaction
dev.off()