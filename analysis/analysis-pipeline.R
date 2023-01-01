## title: "Analysis pipeline"
## author: "PSA008" v. R&R

## This document describes the analysis pipeline for PSA 008 Minimal
## Groups. It does not include the power analysis, which is in a
## separate document. The purpose is to troubleshoot --- some
## (e.g. convergence) errors might be an artifact of the artificial
## data.

##################################################
## R package information
 
## load packages
## list of packages required
packages <- c(
  "tidyverse", # data wrangling
  "lme4", # random effects models
  "lmerTest", # random effects models
  "metafor", # for meta-analysis
  "patchwork" # plots
)
## create list of packages that have not been installed
new_packages <-
    packages[!(packages %in% installed.packages()[,"Package"])]
## install packages that have not been installed
if (length(new_packages)) 
    install.packages(new_packages, dependencies = TRUE)
## load all packages
sapply(packages, library, character.only = TRUE)

source("./custom-functions.R")

set.seed(1970) # to reproduce analysis

sessionInfo() # session info

##################################################
## Input artificial data

## We created an artificial dataset that contains the relevant
## variables in a separate script, which we can call here and in other
## files. Note the dataset here will include the variables used in the
## main analyses, not all collected variables. Furthermore, the
## dataset resembles the processed data, not the raw data from
## Qualtrics.
 
## source simulated daa
## loads the simulated data, and wrangled dataframes for each RQ
source("./data-simulation.R")

## peek at data
head(fake_data)

## The (minimal group) dependent measures are based on the three
## dictator games (in-group--self, out-group--self,
## in-group--out-group) and the average attitude towards in-group and
## towards out-group. The resultant three measures are: scores in
## in-group and out-group attitudes (att_bias), amount in
## in-group–self and out-group–self decisions in the dictator game
## (dg_first_bias), and the decision in the in-group–out-group
## dictator game (dg_third_bias).

## First run some summary statistics (of att_bias, the same could be
## done for the other two outcomes)

## peek at dataframe
head(df_rq1)

## distribution of one outcome by country
df_rq1_att %>% 
  ggplot(aes(amount, colour = group)) + 
  geom_density() +
  facet_wrap(. ~ country)

## descriptives of one outcome
df_rq1_att %>% 
  group_by(country, group) %>% 
  summarise(mean = mean(amount, na.rm = TRUE), 
            sd = sd(amount, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
    print(n = n_countries)

##################################################
## Research question 1

##################################################
## meta-analytical approach

##################################################
## Attitudes

## calculate d and sv
## first calculate various measures (e.g. sd, r)
df_rq1_ma <-

    filter(df_rq1, measure == "att") %>%
    pivot_wider(names_from = group, values_from = amount) %>%
    group_by(country) %>%
    summarise(n_sample = n(),
              ## means per condition
              mean_min_in_self = mean(`in`),
              mean_min_out_self = mean(out),
              ## correlation between conditions
              r_means = cor(`in`, out),
              ## standard deviation per condition
              sd_min_in_self = sd(`in`),
              sd_min_out_self = sd(out),
              sd_z = sqrt(sd_min_in_self^2 + sd_min_out_self^2 + 
                          2*r_means*sd_min_in_self*sd_min_in_self),
              sd_rm = sd_z / (sqrt(2*(1 - r_means))),
              ## effect size
              d = (mean_min_in_self - mean_min_out_self) / sd_rm,
              g = d * (1 - (3/((4*n_sample) - 9))), # NB not needed
              ## sampling variance
              sv = (((1/n_sample) + (d^2/(2*n_sample))) * (2*(1 - r_means))),
              ## 95% confidence intervals
              ci_low = g - qnorm(0.025, lower.tail = FALSE) * sqrt(sv),
              ci_up = g - qnorm(0.975, lower.tail = FALSE) * sqrt(sv)
              )

## We show a cleveland plot of the raw data
filter(df_rq1, measure == "att") %>%
    pivot_wider(names_from = group, values_from = amount) %>%
    pivot_longer(cols = c(`in`, out), names_to = "group", values_to = "amount") %>%
    group_by(country, group) %>%
    summarise(mean = mean(amount)) %>%
    ungroup %>%
    ggplot() +
    aes(mean, country) +
    geom_line(aes(group = country)) +
    geom_point(aes(colour = group), size = 3) +
    xlim(1, 7) +
    theme_minimal()

## We then run the meta-analysis with country as random effect:

## effect size moderated by country
es_country <-

    rma.mv(g, 
           sv, 
           random = list(~ 1 | country),
           data = df_rq1_ma)

## coefficients
summary(es_country)

## As shown in the summary above, the overall effect size is `r
## round(coef(summary(es_country))$estimate, 2)` (`r
## round(coef(summary(es_country))$ci.lb, 2)`, `r
## round(coef(summary(es_country))$ci.ub, 2)`) .

## custom forest plot
df_rq1_ma %>% 
    arrange(desc(g)) %>%
    mutate(min_effect = if_else(ci_low < 0 & ci_up > 0, "no", "yes")) %>%
    ggplot() +
    aes(x = g, y = reorder(country, g), xmin = ci_low, xmax = ci_up) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_errorbarh(colour = "grey30", height = .1) + 
    geom_point(aes(colour = min_effect), size = 3) +
    geom_text(aes(x = 1,
                  label = paste0(
                      round(g, 2), " [", round(ci_low, 2), ", ", round(ci_up, 2), "]")),
              hjust = 0) +
    xlim(-0.5, 1.5) +
    xlab("effect size (d)") +
    ylab("country") +
    guides(colour = "none") +
    theme_minimal()

## To determine the amount of variability in countries, we report
## report heterogeneity measures:

## Q
## reported in summary()

## heterogeneity (I2)
## from: https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
W <- diag(1/es_country$vi)
X <- model.matrix(es_country)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
I_squared <- 100 * sum(es_country$sigma2) / (sum(es_country$sigma2) + (es_country$k - es_country$p)/sum(diag(P)))

## tau
## reported in summary

##################################################
## First party allocation

## for dg_first

## calculate d and sv
## first calculate various measures (e.g. sd, r)
df_rq1_ma <-

    filter(df_rq1, measure == "dg_first") %>%
    pivot_wider(names_from = group, values_from = amount) %>%
    group_by(country) %>%
    summarise(n_sample = n(),
              ## means per condition
              mean_min_in_self = mean(`in`),
              mean_min_out_self = mean(out),
              ## correlation between conditions
              r_means = cor(`in`, out),
              ## standard deviation per condition
              sd_min_in_self = sd(`in`),
              sd_min_out_self = sd(out),
              sd_z = sqrt(sd_min_in_self^2 + sd_min_out_self^2 + 
                          2*r_means*sd_min_in_self*sd_min_in_self),
              sd_rm = sd_z / (sqrt(2*(1 - r_means))),
              ## effect size
              d = (mean_min_in_self - mean_min_out_self) / sd_rm,
              g = d * (1 - (3/((4*n_sample) - 9))), # NB not needed
              ## sampling variance
              sv = (((1/n_sample) + (d^2/(2*n_sample))) * (2*(1 - r_means))),
              ## 95% confidence intervals
              ci_low = g - qnorm(0.025, lower.tail = FALSE) * sqrt(sv),
              ci_up = g - qnorm(0.975, lower.tail = FALSE) * sqrt(sv)
              )

## We show a cleveland plot of the raw data
filter(df2, measure == "dg_first") %>%
    pivot_wider(names_from = group, values_from = amount) %>%
    pivot_longer(cols = c(`in`, out), names_to = "group", values_to = "amount") %>%
    group_by(country, group) %>%
    summarise(mean = mean(amount)) %>%
    ungroup %>%
    ggplot() +
    aes(mean, country) +
    geom_line(aes(group = country)) +
    geom_point(aes(colour = group), size = 3) +
    xlim(0, 20) +
    theme_minimal()

## We then run the meta-analysis with country as random effect:

## effect size moderated by country
es_country <-

    rma.mv(g, 
           sv, 
           random = list(~ 1 | country),
           data = df_rq1_ma)

## coefficients
summary(es_country)

## As shown in the summary above, the overall effect size is `r
## round(coef(summary(es_country))$estimate, 2)` (`r
## round(coef(summary(es_country))$ci.lb, 2)`, `r
## round(coef(summary(es_country))$ci.ub, 2)`).

## custom forest plot
df_rq1_ma %>% 
    arrange(desc(g)) %>%
    mutate(min_effect = if_else(ci_low < 0 & ci_up > 0, "no", "yes")) %>%
    ggplot() +
    aes(x = g, y = reorder(country, g), xmin = ci_low, xmax = ci_up) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_errorbarh(colour = "grey30", height = .1) + 
    geom_point(aes(colour = min_effect), size = 3) +
    geom_text(aes(x = 0.75,
                  label = paste0(
                      round(g, 2), " [", round(ci_low, 2), ", ", round(ci_up, 2), "]")),
              hjust = 0) +
    xlim(-0.5, 1.0) +
    xlab("effect size (d)") +
    ylab("country") +
    guides(colour = "none") +
    theme_minimal()

## To determine the amount of variability in countries, we report
## report heterogeneity measures:

## Q
## reported in summary()

## heterogeneity (I2)
## from: https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
W <- diag(1/es_country$vi)
X <- model.matrix(es_country)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
I_squared <- 100 * sum(es_country$sigma2) / (sum(es_country$sigma2) + (es_country$k - es_country$p)/sum(diag(P)))

## tau
## reported in summary

##################################################
### Third party allocations

## for dg_third

## Unlike the other two effect sizes, we use the standardized mean
## difference effect (Cohen's $d_z$). We first show the mean
## difference scores.

filter(df_rq1, measure == "dg_third") %>%
    pivot_wider(names_from = group, values_from = amount) %>%
    mutate(diff = `in` - out) %>%
    group_by(country) %>%
    mutate(mean_diff = mean(diff)) %>%
    group_by(country) %>%
    summarise(mean = mean(mean_diff))

## calculate d and sv
## first calculate various measures (e.g. sd, r)
## see text for equations
df_rq1_ma <-

    filter(df_rq1, measure == "dg_third") %>%
    pivot_wider(names_from = group, values_from = amount) %>%
    ## mutate(`in` = 20 - `in`,
    ##        out = 20 - out) %>%    
    ## mutate(`in` = both,
    ##        out = 20 - `in`) %>%    
    ## mutate(`in` = `in`- 10,
    ##        out = out - 10) %>%
    mutate(diff = `in` - out) %>%
    group_by(country) %>%
    mutate(mean_diff = mean(diff)) %>%
    unnest(cols = c()) %>%
    mutate(diff_mean_diff = diff - mean_diff,
           diff_mean_diff_sq = (diff_mean_diff)^2
           ) %>%
    summarise(n_sample = n(),
              mean_diff = mean(mean_diff),
              sum_diff = sum(diff_mean_diff_sq),
              sum_diff_n = sum_diff / (n_sample - 1),
              sqrt_sum_diff_n = sqrt(sum_diff_n),
              d_z = mean_diff / sqrt_sum_diff_n,
              sv = sum_diff_n
              )

## We then run the meta-analysis with country as random effect:

## effect size moderated by country
es_country <-

    rma.mv(d_z, 
           sqrt_sum_diff_n, 
           random = list(~ 1 | country),
           data = df_rq1_ma)

## coefficients
summary(es_country)

## As shown in the summary above, the overall effect size is `r
## round(coef(summary(es_country))$estimate, 2)` (`r
## round(coef(summary(es_country))$ci.lb, 2)`, `r
## round(coef(summary(es_country))$ci.ub, 2)`).

## To determine the amount of variability in countries, we report
## report heterogeneity measures:

## Q
## reported in summary()

## heterogeneity (I2)
## from: https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
W <- diag(1/es_country$vi)
X <- model.matrix(es_country)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
I_squared <- 100 * sum(es_country$sigma2) / (sum(es_country$sigma2) + (es_country$k - es_country$p)/sum(diag(P)))

## tau
## reported in summary

##################################################
## Research Question 2

##################################################
## individual level

#####
## Below is an individual level analysis of a single moderator, as an example.

## individualism--collectivism
model_rq2_permeability <-
    
    lmerTest::lmer(
                  amount ~ 1 + permeability + (permeability | country)
                , data = df_rq2_att)

## summarise
summary(model_rq2_permeability)

#####
## below are specifications for all models
## NB not all of these will run as simulated data does not have all moderators

## NB to prevent errors in running entire scripts, the models with the
## moderators not in the simulated data have been commented out

## 3 models for permeability (hypothesis H2.1)

## ## family ties
## model_rq2_family_ties <-
    
##     lmerTest::lmer(
##                   amount ~ 1 + family_ties + (family_ties | country)
##                 , data = df_rq2_att)

## individualism--collectivism
model_rq2_permeability <-
    
    lmerTest::lmer(
                  amount ~ 1 + permeability + (permeability | country)
                , data = df_rq2_att)

## ## model relational mobility
## model_rq2_relational_mobility <-
    
##     lmerTest::lmer(
##                   amount ~ 1 + relational_mobility + (relational_mobility | country)
##                 , data = df_rq2_att)

## 2 models for trust (hypothesis H2.2)

## ## model with trust (stranger)
## model_rq2_trust_strangers <-
    
##     lmerTest::lmer(
##                   amount ~ 1 + trust_strangers + (trust_strangers | country)
##                 , data = df_rq2_att)

## model with trust (institutional)
model_rq2_trust_institution <-
    
    lmerTest::lmer(
                  amount ~ 1 + trust_institution + (trust_institution | country)
                , data = df_rq2_att)

## 1 model for self-esteem (hypothesis H2.3)

## model with esteem
model_rq2_esteem <-
    
    lmerTest::lmer(
                  amount ~ 1 + self_esteem + (self_esteem | country)
                , data = df_rq2_att)

## 2 models for belief and status (hypothesis H2.4)

## ## model with belief
## model_rq2_belief <-
    
##     lmerTest::lmer(
##                   amount ~ 1 + belief + (belief | country)
##                 , data = df_rq2_att)

## ## model with status
## model_rq2_status <-
    
##     lmerTest::lmer(
##                   amount ~ 1 + status + (status | country)
##                 , data = df_rq2_att)

##################################################
### Country level 
 
## We see if country-level predictors (Hofstede's individualism, the
## strength of family ties, in-group--out-group trust, and the Kinship
## Intensity Indicator) are significantly correlated with the
## countries' MGE means measures. Here we demonstrate with with
## Hofstede's individualism, the Kinship Intensity Indicator, and
## trust from country-level indicators shared by JS.

## read in country level indicators
country_level_indicators <-
  
  read_csv("./country-level-indicators.csv")

## calculate means
country_level_means <-

    df_rq2_att %>%
    select(id, country, group, amount) %>%
    pivot_wider(names_from = group, values_from = amount) %>%
    mutate(min_bias = `in` - out) %>%
    group_by(country) %>%
    summarise(mean_min_bias = mean(min_bias)) %>%
    select(country, mean_min_bias)

## print random effects and best line
## shown is just dg_third_dummy
joined <- 
  
    left_join(country_level_means,
              country_level_indicators,
              by = "country") %>%
    filter(!is.na(country_actual)) # for pipeline, remove NA countries

## correlation
cor.test(joined$KII, joined$mean_min_bias) # KII
cor.test(joined$Hofstede, joined$mean_min_bias) # Hofstede
cor.test(joined$trust, joined$mean_min_bias) # trust

## plot (all three measures)
joined %>%
  pivot_longer(cols = c("KII", "Hofstede", "trust"),
               names_to = "country_measure", 
               values_to = "value") %>%
  ggplot(aes(x = value, y = mean_min_bias, label = country)) + 
  geom_point() + 
  geom_smooth(se = F, method = lm) +
  geom_label(nudge_y = 0.0001, alpha = 0.5) +
  facet_wrap(. ~ country_measure) +
  theme_bw()

## example of ols
model_rq2_ols <- lm(mean_min_bias ~ KII, data = joined)
## biogeographic covariates to be included

##################################################
## Research Question 3

## For research question 3, we assess whether real-world bias (towards
## the nation and/or family) is predicted by minimal group bias.

## peek at dataframe
head(df_rq3)

## To avoid three-way interactions, we model each measure
## separately. We demonstrate the analysis with the attitude measures.

## real-world
model_real_world <- 

    lmerTest::lmer(att_real_bias ~ att_min_bias * group_type + 
                       (1 | id)  + 
                       (att_min_bias + group_type | country)
                 , contrasts = list(group_type = "contr.sum")
                 , data = df_rq3)

## summary
summary(model_real_world)

## plot rq3 att
model_coefs <- 
  
  coef(model_real_world)$country %>% 
  rename(intercept = `(Intercept)`, slope = att_min_bias) %>% 
  rownames_to_column("country")

## model_coefs
df_rq3_rand <- left_join(df_rq3_att, model_coefs, by = "country")

## create plot
ggplot(data = df_rq3_rand, 
       mapping = aes(x = att_min_bias, 
                     y = att_real_bias, 
                     colour = lab)
       ) +
  geom_point(na.rm = T, alpha = 0.5) +
  geom_abline(aes(intercept = intercept, 
                  slope = slope,
                  colour = lab
                  ),
              size = 1.5
              ) +
  facet_wrap(. ~ group_type) +
  guides(colour = "none") +
  theme_bw()

## The above will be repeated for minimal bias in both the first- and
## third-party dictator games.

##### demographics
## include demographic variables
## as robustness checks
## NB this will be with all models, included model here for demonstration purposes

## model with country
model_real_world_demographics <- 
    
    lmerTest::lmer(
                  att_real_bias ~ att_min_bias * group_type +
                      age + gender + income + political +
                      (1 | id)  + 
                      (att_min_bias + group_type | country)
                , contrasts = list(group_type = "contr.sum",
                                   gender = "contr.sum")
                , data = df_rq3
              )

## summary
summary(model_real_world_demographics)

## run anova to check significance of demographic vars
anova(model_real_world_demographics, ddf = "Kenward-Roger")
