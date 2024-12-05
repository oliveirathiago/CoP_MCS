# load required packages
library(tidyverse)
library(haven)
library(dplyr)
library(mfx)
library(texreg)
library(ggplot2)
library(sjPlot)
library(lavaan)
library(MASS)


# load dataset
load("data/export/data_analysis.RData")

##############

# assigning new variables to the dataset
data.analysis <-
  data.analysis %>%
  mutate(
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
    ###
    # victimisation by age 14 | summative scale:
    victimisation_14 = victim_threatened_14 + victim_violent_14 + victim_weapon_14 + victim_stolen_14,
    # victimisation by age 17 | summative scale:
    victimisation_17 = victim_threatened_17 + victim_violent_17 + victim_weapon_17 + victim_stolen_17,
    # changing reference of the ethnicity-binary variable
    non.white = white_GB == F
  )

#########################################
### Predicting offending behaviour at age 17 ###
#########################################

# list of covariates
covs_17 <- 'white_GB + male + area_safety_14 +  street_gang_14 + cannabis_use_14 + drinking_ever_14 + victimisation_14 + police.stopped_14'

# estimate regression models for offending behaviour_17 (single measure)
m.offending_violence_nocontrol <- glm(as.formula(paste('offending_violent_17', '~', paste(covs_17))), data.analysis, family = binomial(link = 'logit'))
m.offending_violence_autoreg <- glm(as.formula(paste('offending_violent_17', '~', paste(covs_17), '+ offending_violent_14 + offending_theft_14')), data.analysis, family = binomial(link = 'logit') )
m.offending_violence_int <- glm(as.formula(paste('offending_violent_17', '~', paste(covs_17), '+ offending_violent_14 + offending_theft_14 + police.stopped_14*white_GB')), data.analysis, family = binomial(link = 'logit'))

m.offending_theft_nocontrol <- glm(as.formula(paste('offending_theft_17', '~', paste(covs_17))), data.analysis, family = binomial(link = 'logit'))
m.offending_theft_autoreg <- glm(as.formula(paste('offending_theft_17', '~', paste(covs_17), '+ offending_violent_14 + offending_theft_14')), data.analysis, family = binomial(link = 'logit') )
m.offending_theft_int <- glm(as.formula(paste('offending_theft_17', '~', paste(covs_17), '+ offending_theft_14 + offending_theft_14 + police.stopped_14*white_GB')), data.analysis, family = binomial(link = 'logit'))

# changing reference group for plots

covs_17_nonwhite <- 'non.white + male + area_safety_14 +  street_gang_14 + cannabis_use_14 + drinking_ever_14 + victimisation_14 + police.stopped_14'

m.offending_violence_int.nonwhite <- glm(as.formula(paste('offending_violent_17', '~', paste(covs_17_nonwhite), '+ offending_violent_14 + offending_theft_14 + police.stopped_14*non.white')), data.analysis, family = binomial(link = 'logit'))
m.offending_theft_int.nonwhite <- glm(as.formula(paste('offending_theft_17', '~', paste(covs_17_nonwhite), '+ offending_theft_14 + offending_theft_14 + police.stopped_14*non.white')), data.analysis, family = binomial(link = 'logit'))

# probability scale
m.violence.all_prob <- logitmfx(m.offending_violence_nocontrol, data = data.analysis, atmean = T)
m.theft.all_prob <- logitmfx(m.offending_theft_nocontrol, data = data.analysis, atmean = T)
m.violence_prob <- logitmfx(m.offending_violence_autoreg, data = data.analysis, atmean = T)
m.theft_prob <- logitmfx(m.offending_theft_autoreg, data = data.analysis, atmean = T)
m.violence_white_prob <- logitmfx(m.offending_violence_int, data = data.analysis, atmean = T)
m.violence_non.white_prob <- logitmfx(m.offending_violence_int.nonwhite, data = data.analysis, atmean = T)
m.theft_white_prob <- logitmfx(m.offending_theft_int, data = data.analysis, atmean = T)
m.theft_non.white_prob <- logitmfx(m.offending_theft_int.nonwhite, data = data.analysis, atmean = T)

## produce tables
# reg coefficients
list(
  "Violent offending" = m.offending_violence_nocontrol,
  "Violent offending" = m.offending_violence_autoreg,
  "Violent offending" = m.offending_violence_int,
  "Non-Violent offending" = m.offending_theft_nocontrol,
  "Non-Violent offending" = m.offending_theft_autoreg,
  "Non-Violent offending" = m.offending_theft_int
) %>% wordreg(ci.force = T, 
              custom.gof.rows = list("Autoregressive" = c("No", "Yes", "Yes") %>% rep(2),
                                     "Interaction term" = c("No", "No", "Yes") %>% rep(2)),
              file = "tables/2_offending_coefficients.docx")

# reg coefficients
list(
  "Violent offending" = m.violence.all_prob,
  "Violent offending" = m.violence_prob,
  "Violent offending" = m.violence_white_prob,
  "Non-Violent offending" = m.theft.all_prob,
  "Non-Violent offending" = m.theft_prob,
  "Non-Violent offending" = m.theft_white_prob
) %>% wordreg(ci.force = T, 
              custom.gof.rows = list("Autoregressive" = c("No", "Yes", "Yes") %>% rep(2),
                                     "Interaction term" = c("No", "No", "Yes") %>% rep(2)),
              file = "tables/2_offending_marginaleffects.docx")


# data for plot: interaction
list_models <- list(
  m.violence_white_prob$mfxest,
  m.violence_non.white_prob$mfxest,
  m.theft_white_prob$mfxest,
  m.theft_non.white_prob$mfxest,
  m.violence_prob$mfxest,
  m.theft_prob$mfxest
)

data.plot_interaction <-
  tibble(
    prob = c(
      list_models[[1]]["police.stopped_14TRUE", "dF/dx"], list_models[[2]]["police.stopped_14TRUE", "dF/dx"], 
      list_models[[3]]["police.stopped_14TRUE", "dF/dx"], list_models[[4]]["police.stopped_14TRUE", "dF/dx"],
      list_models[[5]]["police.stopped_14TRUE", "dF/dx"], list_models[[6]]["police.stopped_14TRUE", "dF/dx"]
    ),
    std.err = c(
      list_models[[1]]["police.stopped_14TRUE", 2], list_models[[2]]["police.stopped_14TRUE", 2], 
      list_models[[3]]["police.stopped_14TRUE", 2], list_models[[4]]["police.stopped_14TRUE", 2],
      list_models[[5]]["police.stopped_14TRUE", 2], list_models[[6]]["police.stopped_14TRUE", 2]
    ),
    var = c(c("violent", "non-violent") %>% rep(each = 2), "violent", "non-violent"),
    main = c(c('non-white', 'white') %>% rep(2), "all", "all")
  ) %>%
  mutate(ci.low = prob - 1.96 * std.err,
         ci.upp = prob + 1.96 * std.err)


# produce plot: interaction
plot.results_interaction <- ggplot(data.plot_interaction, aes(y = prob, x = var, group = main, colour = main)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp), width = .3, position = position_dodge(), size = .8, lwd = .75, show.legend = T) +
  geom_point(position = position_dodge(width = 0.3)) +
  ylim(-.1,.15) +
  geom_hline(yintercept = 0, size = .75, color = 'darkgray') +
  #coord_flip() +
  ylab("") + xlab("") +
  ggtitle("Effects of being stopped by the police by age 14 \n on the probability of offending behavior by age 17 \n") +
  labs(caption = 'Binomial logistic regression models estimated using maximum likelihood.
                  Marginal effects at the mean (i.e., in probability scale)
                  and 95% confidence intervals reported. \n
                  Models include autoregressive parameters (previous offending behavior by age 14). 
                  Models also control for gender, perception of safety, gang membership by age 14,
                  cannabis use by age 14, drinking by age 14, and victimisation by age 14. \n

                  n = 7524 for all four models') +
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
  scale_x_discrete(expand = expansion(mult = .5),
                   limits = c("violent", "non-violent") %>% rev,
                   breaks = c("violent", "non-violent") %>% rev,
                   labels = c('Violent\noffending behaviour', 'Non-violent\noffending behaviour') %>% rev) + 
  scale_color_brewer(palette = "Set1",
                     limits = c('white', 'non-white', 'all'),
                     labels = c('White', "Black and other\nethnic minorities", 'All respondents'))

pdf('plots/2_offending_interactions.pdf')
plot.results_interaction
dev.off()