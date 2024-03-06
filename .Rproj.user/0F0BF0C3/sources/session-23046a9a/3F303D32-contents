# load required packages
library(tidyverse)        # tidyverse package fot data managing (e.g., dplyr, ggplot2, ...)
library(haven)            # haven package to read foreign data sources
library(lavaan)           # lavaan package for latent variable modelling
library(mfx)              # mfx package for marginal effects in glm models
library(texreg)           # texreg package for regression displays

# load masterdataset
load("data/export/data_analysis.RData")

###########################################
#### Measuring offending behaviour and ####
### victimisation using an IRT approach ###
###########################################

# measuring offending behaviour
measurement_offending <-
  'offending_14 =~ physical_aggression_hit_14 + physical_aggression_weapon_14 + theft_taken_14 + theft_stolen_14
   offending_17 =~ physical_aggression_hit_17 + physical_aggression_weapon_17 + theft_taken_17 + theft_stolen_17
   
   offending_14 ~~ offending_17
   
   physical_aggression_hit_14 ~~ physical_aggression_hit_17
   physical_aggression_weapon_14 ~~ physical_aggression_weapon_17
   theft_taken_14 ~~ theft_taken_17
   theft_stolen_14 ~~ theft_stolen_17

  ' %>%
  cfa(data.analysis %>%
        mutate(across(c(physical_aggression_hit_14:theft_stolen_14, physical_aggression_hit_17:theft_stolen_17), as.numeric)),
      estimator = "MLR", missing = "ML", std.lv = T
      )

# assigning new variables to the dataset
data.analysis <-
  data.analysis %>%
  mutate(
    # offending by age 14 | irt scale:
    offending_irt_14 = lavPredict(measurement_offending)[, "offending_14"],
    # offending by age 14 | summative scale:
    offending_sum_14 = physical_aggression_hit_14 + physical_aggression_weapon_14 + theft_taken_14 + theft_stolen_14,
    # offending by age 17 | irt scale:
    offending_irt_17 = lavPredict(measurement_offending)[, "offending_17"],
    # offending by age 17 | summative scale:
    offending_sum_17 = physical_aggression_hit_17 + physical_aggression_weapon_17 + theft_taken_17 + theft_stolen_17,
    ###
    # victimisation by age 14 | summative scale:
    victimisation_14 = victim_threatened_14 + victim_violent_14 + victim_weapon_14 + victim_stolen_14,
    # victimisation by age 17 | summative scale:
    victimisation_17 = victim_threatened_17 + victim_violent_17 + victim_weapon_17 + victim_stolen_17,
    # changing reference of the ethnicity-binary variable
    non.white = white_GB == F
  )

#########################################
### Predicting police stops by age 14 ###
#########################################

# list of covariates
covs_14 <- 'male + area_safety_14 + carrying_knife_14 + offending_sum_14 + 
            street_gang_14 + cannabis_use_14 + drinking_ever_14 + victimisation_14'


# estimate regression models
m.stop <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), '+ white_GB')), data.analysis, family = binomial(link = 'logit'))

# estimate regression models with interaction terms (ref: nonWhite)
m.stopXknife_white <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + white_GB + white_GB * carrying_knife_14')), data.analysis, family = binomial(link = 'logit'))
m.stopXoffending_white <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + white_GB + white_GB * offending_sum_14')), data.analysis, family = binomial(link = 'logit'))
m.stopXgang_white <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + white_GB + white_GB * street_gang_14')), data.analysis, family = binomial(link = 'logit'))
m.stopXcannabis_white <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + white_GB + white_GB * cannabis_use_14')), data.analysis, family = binomial(link = 'logit'))
m.stopXdrinking_white <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + white_GB + white_GB * drinking_ever_14')), data.analysis, family = binomial(link = 'logit'))

# estimate regression models with interaction terms (ref: White) (saving those models for plot purposes)
m.stopXknife_nonwhite <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + non.white + non.white * carrying_knife_14')), data.analysis, family = binomial(link = 'logit'))
m.stopXoffending_nonwhite <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + non.white + non.white * offending_sum_14')), data.analysis, family = binomial(link = 'logit'))
m.stopXgang_nonwhite <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + non.white + non.white * street_gang_14')), data.analysis, family = binomial(link = 'logit'))
m.stopXcannabis_nonwhite <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + non.white + non.white * cannabis_use_14')), data.analysis, family = binomial(link = 'logit'))
m.stopXdrinking_nonwhite <- glm(as.formula(paste('police.stopped_14', '~', paste(covs_14), ' + non.white + non.white * drinking_ever_14')), data.analysis, family = binomial(link = 'logit'))

# see results
list(m.stop, m.stopXknife, m.stopXoffending, m.stopXgang, m.stopXcannabis, m.stopXdrinking) %>% screenreg(ci.force = T)

# save results in probability scale
m.stop_prob <- logitmfx(m.stop, data = data.analysis, atmean = T)
m.stopXknife_white_prob <- logitmfx(m.stopXknife_white, data = data.analysis, atmean = T)
m.stopXoffending_white_prob <- logitmfx(m.stopXoffending_white, data = data.analysis, atmean = T)
m.stopXgang_white_prob <- logitmfx(m.stopXgang_white, data = data.analysis, atmean = T)
m.stopXcannabis_white_prob <- logitmfx(m.stopXcannabis_white, data = data.analysis, atmean = T)
m.stopXdrinking_white_prob <- logitmfx(m.stopXdrinking_white, data = data.analysis, atmean = T)
m.stopXknife_nonwhite_prob <- logitmfx(m.stopXknife_nonwhite, data = data.analysis, atmean = T)
m.stopXoffending_nonwhite_prob <- logitmfx(m.stopXoffending_nonwhite, data = data.analysis, atmean = T)
m.stopXgang_nonwhite_prob <- logitmfx(m.stopXgang_nonwhite, data = data.analysis, atmean = T)
m.stopXcannabis_nonwhite_prob <- logitmfx(m.stopXcannabis_nonwhite, data = data.analysis, atmean = T)
m.stopXdrinking_nonwhite_prob <- logitmfx(m.stopXdrinking_nonwhite, data = data.analysis, atmean = T)

#############
### plots ###
#############

# data for plot: no interaction
data.plot_simple <-
  m.stop_prob$mfxest %>%
  as_tibble() %>%
  mutate(ci.low = `dF/dx` - 1.96 *`Std. Err.`,
         ci.upp = `dF/dx` + 1.96 *`Std. Err.`,
         vars = c('male', 'safety', 'knife', 'offending', 'gang', 'cannabis', 'drinking', 'victimisation', 'white')
  )

# produce plot: no interaction
plot.results_simple <- ggplot(data.plot_simple %>% filter(vars != c('male', 'safety', 'victimisation'))
                         , aes(y = `dF/dx`, x = vars)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp), width = .25, position = position_dodge(), size = .75, lwd = .75, show.legend = T) +
  ylim(-.1,.4) +
  geom_hline(yintercept = 0, size = .75, color = 'darkgray') +
  coord_flip() +
  ylab("") + xlab("") +
  ggtitle("Probability of police contact by age 14") +
  labs(caption = 'Binomial logistic regression models estimated using maximum likelihood.
                  Marginal effects at the mean (probability) 95% confidence intervals reported. 
                  Model also controls for gender, previous victimisation, and perception neighbourhood safety. \n

                  n = 7524') +
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
  scale_x_discrete(limits = c('white', 'offending', 'knife', 'gang', 'cannabis', 'drinking') %>% rev(),
                   breaks = c('white', 'offending', 'knife', 'gang', 'cannabis', 'drinking') %>% rev(),
                   labels = c('White', 'Previous offending behaviour', 'Carrying knife', 'Street gang', 'Using cannabis', 'Drinking') %>% rev())

# data for plot: interaction
list_models <- list(
  m.stopXknife_white_prob$mfxest, m.stopXoffending_white_prob$mfxest, m.stopXgang_white_prob$mfxest,
  m.stopXcannabis_white_prob$mfxest, m.stopXdrinking_white_prob$mfxest, m.stopXknife_nonwhite_prob$mfxest, 
  m.stopXoffending_nonwhite_prob$mfxest, m.stopXgang_nonwhite_prob$mfxest, m.stopXcannabis_nonwhite_prob$mfxest,
  m.stopXdrinking_nonwhite_prob$mfxest
)

data.plot_interaction <-
  tibble(
    prob = c(
      list_models[[1]][3, "dF/dx"], list_models[[2]][4, "dF/dx"], list_models[[3]][5, "dF/dx"], list_models[[4]][6, "dF/dx"], list_models[[5]][7, "dF/dx"],
      list_models[[6]][3, "dF/dx"], list_models[[7]][4, "dF/dx"], list_models[[8]][5, "dF/dx"], list_models[[9]][6, "dF/dx"], list_models[[10]][7, "dF/dx"]
    ),
    std.err = c(
      list_models[[1]][3, 2], list_models[[2]][4, 2], list_models[[3]][5, 2], list_models[[4]][6, 2], list_models[[5]][7, 2],
      list_models[[6]][3, 2], list_models[[7]][4, 2], list_models[[8]][5, 2], list_models[[9]][6, 2], list_models[[10]][7, 2]
    ),
    var = c('knife', 'offending', 'gang', 'cannabis', 'drinking') %>% rep(2),
    main = c('white', 'non-white') %>% rep(each = 5)
  ) %>%
  mutate(ci.low = prob - 1.96 * std.err,
         ci.upp = prob + 1.96 * std.err)

# produce plot: interaction
plot.results_interaction <- ggplot(data.plot_interaction, aes(y = prob, x = var, group = main, colour = main)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp), width = .25, position = position_dodge(), size = .75, lwd = .75, show.legend = T) +
  ylim(-.1,.4) +
  geom_hline(yintercept = 0, size = .75, color = 'darkgray') +
  coord_flip() +
  ylab("") + xlab("") +
  ggtitle("Probability of police contact by age 14") +
  labs(caption = 'Binomial logistic regression models estimated using maximum likelihood.
                  Marginal effects at the mean (probability) 95% confidence intervals reported. 
                  Models also control for gender, previous victimisation, and perception neighbourhood safety. \n

                  n = 7524 for all five models') +
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
  scale_x_discrete(limits = c('offending', 'knife', 'gang', 'cannabis', 'drinking') %>% rev(),
                   breaks = c('offending', 'knife', 'gang', 'cannabis', 'drinking') %>% rev(),
                   labels = c('Previous offending behaviour', 'Carrying knife', 'Street gang', 'Using cannabis', 'Drinking') %>% rev()) + 
  scale_color_brewer(palette = "Set1",
                     limits = c('white', 'non-white'),
                     labels = c('White', "Non-White"))


# save results in pdf format in a subfolder named 'plots'
pdf('plots/policecontact14_simple.pdf')
plot.results_simple
dev.off()

pdf('plots/policecontact14_interactions.pdf')
plot.results_interaction
dev.off()