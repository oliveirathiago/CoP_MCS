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
          # recoding educational motivation variables so that 
          # higher scores indicate more motivation
          across(c(educ.motivation_best_14, educ.motivation_interesting_14), ~case_when(
            . == 1 ~ 4,
            . == 2 ~ 3,
            . == 3 ~ 2,
            . == 4 ~ 1,
            TRUE ~ NA_real_)),
          across(c(educ.motivation_unhappy_14, educ.motivation_tired_14, educ.motivation_waste_14, likely_university_17), ~ case_when(. < 0 ~ NA_real_, TRUE ~ .)),
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
          non.white = white_GB == F
          ) %>%
  mutate(likely_uni_17.quartile = likely_university_17 >= 95,
         likely_uni_17.median = likely_university_17 >= 75
  )


###########################################
#### Measuring educational motivation ####
###########################################

# measuring educiational motivation 
measurement_edu <-
  'edu.motivation_14 =~ educ.motivation_best_14 + educ.motivation_interesting_14 + educ.motivation_unhappy_14 + educ.motivation_tired_14+educ.motivation_waste_14
  '%>%
  cfa(data.analysis %>%
        mutate(across(c(educ.motivation_best_14, educ.motivation_interesting_14 , educ.motivation_unhappy_14 , educ.motivation_tired_14, educ.motivation_waste_14), as.numeric)),
      estimator = "MLR", missing = "ML", std.lv = T
  )

# assigning new variables to the dataset
data.analysis <-
  data.analysis %>%
  mutate(
    # educational motivation age 14 | irt scale:
    edu_irt_14 = lavPredict(measurement_edu)[, "edu.motivation_14"])

#########################################
### Predicting university aspirations ###
#########################################

# list of covariates
covs_17 <- 'male + area_safety_14 + carrying_knife_14 + street_gang_14 + cannabis_use_14 + drinking_ever_14 + victimisation_14 + police.stopped_14 + offending_violent_14 + offending_theft_14'

model.controls  <-glm(as.formula(paste('likely_uni_17.quartile', '~', paste(covs_17, ' + white_GB'))), data.analysis, family = binomial(link = 'logit'))
model.all  <-glm(as.formula(paste('likely_uni_17.quartile', '~', paste(covs_17, ' + edu_irt_14 + white_GB'))), data.analysis, family = binomial(link = 'logit'))
model.white  <-glm(as.formula(paste('likely_uni_17.quartile', '~', paste(covs_17, ' + edu_irt_14 + white_GB * police.stopped_14'))), data.analysis, family = binomial(link = 'logit'))
model.non.white <-glm(as.formula(paste('likely_uni_17.quartile', '~', paste(covs_17, ' + edu_irt_14 + non.white * police.stopped_14'))), data.analysis, family = binomial(link = 'logit'))


list(model.controls, model.all, model.white, model.non.white) %>% screenreg(ci.force = T)

model.controls.prob <- logitmfx(model.controls, data = data.analysis, atmean = T)
model.all.prob <- logitmfx(model.all, data = data.analysis, atmean = T)
model.white.prob <- logitmfx(model.white, data = data.analysis, atmean = T)
model.non.white.prob <- logitmfx(model.non.white, data = data.analysis, atmean = T)

## produce tables
# reg coefficients
list(
  "Controls" = model.controls, 
  "No interaction" = model.all,
  "With an interaction" = model.white
) %>% wordreg(ci.force = T, 
              file = "tables/3_education_coefficients.docx")

# reg probability
list(
  "Controls" = model.controls.prob,
  "No interaction" = model.all.prob,
  "With an interaction" = model.white.prob
) %>% wordreg(ci.force = T, 
              file = "tables/3_education_marginaleffects.docx")



list_models <- list(model.all.prob$mfxest, model.white.prob$mfxest, model.non.white.prob$mfxest)

data.plot_interaction <-
  tibble(
    prob = c(
      list_models[[1]]["police.stopped_14TRUE", "dF/dx"], list_models[[2]]["police.stopped_14TRUE", "dF/dx"], list_models[[3]]["police.stopped_14TRUE", "dF/dx"]
    ),
    std.err = c(
      list_models[[1]]["police.stopped_14TRUE", 2], list_models[[2]]["police.stopped_14TRUE", 2], list_models[[3]]["police.stopped_14TRUE", 2]
    ),
    var = rep('Likely to attend\nuniversity', 3),
    main = c('all', 'non-white', 'white')
  ) %>%
  mutate(ci.low = prob - 1.96 * std.err,
         ci.upp = prob + 1.96 * std.err)


# produce plot: interaction
plot.results_interaction <- ggplot(data.plot_interaction, aes(y = prob, x = var, group = main, colour = main)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp), width = .25, position = position_dodge(), size = .8, lwd = .9, show.legend = T) +
  geom_point(position = position_dodge(width = .25)) + 
  ylim(-.5,.5) +
  geom_hline(yintercept = 0, size = .75, color = 'darkgray') +
  #coord_flip() +
  ylab("") + xlab("") +
  ggtitle("Effects of being stopped by the police by age 14 \n on the probability of attending university* \n") +
  labs(caption = 'Binomial logistic regression models estimated using maximum likelihood.
                  Marginal effects at the mean (i.e., in probability scale)
                  and 95% confidence intervals reported. \n
                  * Dependent variable: self-reported likelihood to attend university,
                  dichotomised on the 75th percentile (likely vs. not likely).\n
                  Models include previous educational motivation by age 14. 
                  Models also control for gender, perception of safety, gang membership by age 14,
                  cannabis use by age 14, drinking by age 14, and victimisation by age 14. \n

                  n = 5050') +
  guides(colour = guide_legend(title = "")) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 8),
        axis.text.x = element_text(vjust = 120, colour = "#3C3C3C", size = 10.5)) +
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
                     labels = c('White', "Black and other\nethnic minoritoes", "All"))

pdf('plots/3_education_interactions.pdf')
plot.results_interaction
dev.off()
