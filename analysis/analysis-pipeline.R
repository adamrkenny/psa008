#' ---
#' title: "Analysis pipeline"
#' author: "PSA 008"
#' ---
#' 
#' This document describes the analysis pipeline for PSA 008 Minimal
#' Groups. It does not include the power analysis, which is in a
#' separate document. The purpose is to troubleshoot --- some errors
#' might be an artifact of the artificial data.
#' 

#' 
#' ## R package information
#' 
## ----load-packages, message = FALSE, warning = FALSE--------------------------
## list of packages required
packages <- c(
  "tidyverse", # data wrangling
  "lme4", # random effects models
  "lmerTest" # random effects models
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

#' 
#' ## Input artificial data
#' 
#' We created an artificial dataset that contains the relevant
#' variables in a separate script, which we can call here and in other
#' files. Note the dataset here will include the variables used in the
#' main analyses, not all collected variables. Furthermore, the
#' dataset resembles the processed data, not the raw data from
#' Qualtrics.
#' 
## ---- source-simulated-data---------------------------------------------------
## loads the simulated data, and wrangled dataframes for each RQ
source("./data-simulation.R")

## peek at data
head(fake_data)

#' 
#' The (minimal group) dependent measures are based on the
#' three dictator games (in-group--self, out-group--self,
#' in-group--out-group) and the average attitude towards in-group and
#' towards out-group. The resultant three (minimal group) measures are: difference
#' between in-group and out-group attitudes (att_bias), difference between
#' in-group–self and out-group–self decisions in the dictator game (dg_first_bias), and
#' the decision in the in-group–out-group dictator game (dg_third_bias).
#' 
#' ## Research question 1
#' 
#' Let's run some summary statistics (of att_bias).
#' 
## ---- rq1-manipulation--------------------------------------------------------
## peek at dataframe
head(df_rq1)

## distribution of one outcome by country
fake_data %>% 
  ggplot(aes(att_min_bias)) + 
  geom_density() +
  facet_wrap(. ~ country)

## descriptives of one outcome
fake_data %>% 
  group_by(country) %>% 
  summarise(mean = mean(att_min_bias, na.rm = TRUE), 
            sd = sd(att_min_bias, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
    print(n = n_countries)

## order factors
df_rq1 <-

    df_rq1 %>%
    mutate(measure = factor(measure,
                            levels = c("att", "dg_first", "dg_third")
                            ))

#' 
#' We then use outcome minimal bias (min_bias), which includes all three (standardised) measures of bias with the minimal groups. Measure can then be include as a random effect.
#' 
#' We can run the model either with measure combined or as dummy; I find it easier to extract country-level random effects if we use dummy.
#' 
## ---- rq1-combined-dv-group---------------------------------------------------
## model with country
model_rq1_country_att <- 

    lmerTest::lmer(min_bias ~ 1 +
                       ## measure + (1 | id)  + 
                       (1 | country),
                   data = df_rq1_att 
                   ## , contrasts = list(measure = "contr.sum")
                   )

## summary
summary(model_rq1_country_att) 

## icc
## country variance / (residual + country variance)
## TODO insert values

## dotplot
gg_caterpillar(ranef(model_rq1_country_att, condVar = TRUE), QQ = FALSE, 
               likeDotplot = FALSE)

## built plot manually
## from ranef help file
## TODO plot each by themselves
str(model_rq1_country_att_ranef <- ranef(model_rq1_country_att))
str(model_rq1_country_att_ranef_df <- as.data.frame(model_rq1_country_att_ranef))
model_rq1_country_att_ranef_df %>%
    ggplot() +
    aes(y = grp, x = condval) +
    geom_point() +
    ## facet_wrap(~term, scales = "free_x") +
    geom_errorbarh(aes(xmin = condval - 2*condsd,
                       xmax = condval + 2*condsd),
                   height = 0) +
    xlab("random effect") +
    ylab("country") +
    theme_bw()

## predict the scores based on the model
df_rq1_att %>% 
  mutate(mdl = predict(model_rq1_country_att)) %>%
  ggplot(aes(min_bias, y = mdl, colour = country, group = country)) + 
  geom_smooth(se = F, method = lm) +
  theme_bw() +
  guides(colour = "none") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

#' 
## ----df-rq1-intercept-variance, include = FALSE----------------------------------------

## model has to be re-run with REML = FALSE to compare
model_rq1_country_att_ML <- 

    lmerTest::lmer(min_bias ~ 1 + (1 | country),
                   data = df_rq1_att,
                   REML = FALSE
                   )

## model without random intercept
model_rq1_country_att_without_random_intercept <- 

    lm(min_bias ~ 1,
       data = df_rq1_att
       )

anova(model_rq1_country_att_ML, 
      model_rq1_country_att_without_random_intercept)

#' 
## ----df-rq1-additional, include = FALSE----------------------------------------

##### demographics
## include demographic variables
## as robustness check

## model with country
model_rq1_country_att_demographics <- 

    lmerTest::lmer(min_bias ~ 1 + age + income + political +
                       ## (1 | id)  + 
                       (1 | country),
                   data = df_rq1_att,
                   contrasts = list(age = "contr.sum",
                                    income = "contr.sum",
                                    political = "contr.sum"
                                    )
                   )

## summary
summary(model_rq1_country_att_demographics)

## run anova to check significance of demographic vars
anova(model_rq1_country_att_demographics, ddf = "Kenward-Roger")

#' 
#' ## Research question 2
#' 
#' ### Individual level 
#' 
#' For the individual level analysis of research question 2, we have three main
#' moderators of interest: permeability, (in-group and out-group) trust, and
#' self-esteem. We compare various models:
#' 
## ----df-rq2-------------------------------------------------------------------
## peek at dataframe
head(df_rq2)

#' 
## ----rq2-models---------------------------------------------------------------
## minimal model
model_rq2_min <-
    
    lmerTest::lmer(
        min_bias ~ 1 +  
          ## (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country),
        data = df_rq2_att)

## model with esteem
model_rq2_esteem <-
    
    lmerTest::lmer(
        min_bias ~ 1 + self_esteem +  
          ## (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2_att)

## model with trust (in/out)
model_rq2_trust_in_out <-
    
    lmerTest::lmer(
        min_bias ~ 1 + trust_in_out +  
          ## (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2_att)

## model with trust (institutional)
model_rq2_trust_institution <-
    
    lmerTest::lmer(
        min_bias ~ 1 + trust_institution +  
          ## (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2_att)

## model permeability
model_rq2_permeability <-
    
    lmerTest::lmer(
        min_bias ~ 1 + permeability +  
          ## (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2_att)

## model with esteem and trust
model_rq2_esteem_trust_in_out <-
    
    lmerTest::lmer(
        min_bias ~ 1 + (self_esteem + trust_in_out) +  
          ## (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2_att)

## model with esteem and trust (both in/out and institution)
model_rq2_esteem_trust_both <-
    
    lmerTest::lmer(
        min_bias ~ 1 + (self_esteem + trust_in_out + trust_institution) +  
          ## (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2_att)

## maximal model
model_rq2_max <-
    
    lmerTest::lmer(
        min_bias ~ 1 + (self_esteem + trust_in_out + trust_institution + permeability) +  
          ## (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2_att)

## compare models
anova(
    model_rq2_min, 
    model_rq2_esteem, 
    model_rq2_trust_in_out,
    model_rq2_trust_institution,
    model_rq2_permeability,
    model_rq2_esteem_trust_in_out,
    model_rq2_esteem_trust_both,
    model_rq2_max
)

## explicitly AIC comparison, although above considers it with ML
AIC(
    model_rq2_min, 
    model_rq2_esteem, 
    model_rq2_trust_in_out,
    model_rq2_trust_institution,
    model_rq2_permeability,
    model_rq2_esteem_trust_in_out,
    model_rq2_esteem_trust_both,
    model_rq2_max
)

## NB if the anova() and AIC() comparisons suggest different model order
## then give preferene to results of anova()

#' 
## ---- rq2-country-level-effects-----------------------------------------------
## select model and run the following

## graph with predicted country level min bias

## predict the scores based on the model
df_rq2_att %>%
  mutate(mdl = predict(model_rq2_permeability)) %>%
  ggplot(aes(permeability, y = mdl, colour = country, group = country)) + 
  geom_smooth(se = FALSE, method = lm) +
  theme_bw() +
  guides(colour = "none") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())

## graph of random effects with line of best fit

## save coefficients
coefs_model <- coef(model_rq2_permeability)

## print random effects and best line
## shown is permeability
coefs_model$country %>%
  mutate(country = rownames(coefs_model$country),
         intercept = `(Intercept)`) %>% 
  ggplot(aes(x = permeability, y = intercept, label = country)) + 
  geom_point() + 
  geom_smooth(se = F, method = lm) +
  geom_label(nudge_y = 0.001, alpha = 0.5) +
    theme_bw()

#' 
## ----df-rq2-additional, include = FALSE----------------------------------------

##### other indicators
## additional individual-level analyses
## that include other measures linked to permeability
## as robustness checks

## NB not included as embeddedness and family ties not generated in
## simulated data

## ## model embeddedness
## model_rq2_embeddedness <-
    
##     lmerTest::lmer(
##         min_bias ~ 1 + embeddedness +  
##           ## (1 | id)  + 
##           (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
##         data = df_rq2)

## ## model family ties
## model_rq2_family_ties <-
    
##     lmerTest::lmer(
##         min_bias ~ 1 + family_tie_pc1 +  
##           ## (1 | id)  + 
##           (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
##         data = df_rq2)

## ## AIC comparison
## AIC(
##     model_rq2_permeability,
##     model_rq2_embeddedness,
##     model_rq2_family_ties
## )

##### demographics
## include demographic variables
## as robustness checks

## NB this will be the model with lowest AIC, included here for
## demonstration purposes

## ## model with country
## model_rq2_min_demographics <- 

##     lmerTest::lmer(
##                   min_bias ~ 1 + age + income_when16 + political_orientation +
##                       ## (1 | id)  + 
##                       (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
##                   data = df_rq1
##               )

## ## summary
## summary(model_rq2_min_demographics)

## ## run anova to check significance of demographic vars
## anova(model_rq2_min_demographics, ddf = "Kenward-Roger")

#' 
#' ### Country level 
#' 
#' We see if country-level predictors (Hofstede's individualism, the
#' strength of family ties, in-group--out-group trust, and the Kinship
#' Intensity Indicator) are significantly correlated with the
#' countries' MGE means measures. Here we demonstrate with with
#' Hofstede's individualism, the Kinship Intensity Indicator, and
#' trust from country-level indicators shared by JS.
#' 
## ---- rq2-country-------------------------------------------------------------
## read in country level indicators
country_level_indicators <-
  
  read_csv("./country-level-indicators.csv")

## save coefficients
coefs_model <- coef(model_rq1_country_att)

## save intercepts
country_intercept <-
  
  coefs_model$country %>%
  mutate(country = rownames(coefs_model$country),
         intercept = `(Intercept)`)

## print random effects and best line
## shown is just dg_third_dummy
joined <- 
  
  left_join(country_intercept,
    country_level_indicators,
    by = "country")

## correlation
cor.test(joined$KII, joined$intercept) # KII
cor.test(joined$Hofstede, joined$intercept) # Hofstede
cor.test(joined$trust, joined$intercept) # trust

## plot (all three measures)
joined %>%
  pivot_longer(cols = c("KII", "Hofstede", "trust"),
               names_to = "country_measure", 
               values_to = "value") %>%
  ggplot(aes(x = value, y = intercept, label = country)) + 
  geom_point() + 
  geom_smooth(se = F, method = lm) +
  geom_label(nudge_y = 0.001, alpha = 0.5) +
  facet_wrap(. ~ country_measure) +
  theme_bw()

#' 
#' ## Research question 3
#' 
#' For research question 3, we assess whether real-world bias (towards
#' the nation and/or family) is predicted by minimal group bias. 
#' 
## ----measures-real-world-bias-------------------------------------------------
## peek at dataframe
head(df_rq3)

#' 
#' To avoid three-way interactions, we model each measure separately. We demonstrate the analysis with the attitude measures.
#' 
## ----rq3----------------------------------------------------------------------
## real-world
model_real_world <- 

    lmerTest::lmer(att_real_bias ~ att_min_bias * group_type + 
                       (1 | id)  + 
                       (att_min_bias + group_type | country/lab),
                   data = df_rq3)

## summary
summary(model_real_world)

## ## run anova
## anova(model_real_world, ddf = "Kenward-Roger")

#' 
## ----plot-rq3-----------------------------------------------------------------

model_coefs <- 
  
  coef(model_real_world)$country %>% 
  rename(intercept = `(Intercept)`, slope = att_min_bias) %>% 
  rownames_to_column("country")

model_coefs

df_rq3_rand <- left_join(df_rq3, model_coefs, by = "country")

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

#' 
#' The above will be repeated for minimal bias in both the first- and
#' third-party dictator games.


#' 
## ----df-rq3-additional, include = FALSE----------------------------------------

##### demographics
## include demographic variables
## as robustness checks
## NB this will be with all models, included model here for demonstration purposes

## ## model with country
## model_real_world_demographics <- 


##     lmerTest::lmer(
##                   att_real_bias ~ att_min_bias * group_type + age + income_when16 + political_orientation +
##                       (1 | id)  + 
##                        (att_min_bias + group_type | country/lab),
##                   data = df_rq3_att
##               )

## ## summary
## summary(model_real_world_demographics)

## ## run anova to check significance of demographic vars
## anova(model_rq2_min_demographics, ddf = "Kenward-Roger")

##################################################

## adjust all p.values afterwards using Benjamini & Yekutieli
## these should be reported alongside unadjusted values
## p.adjust(p, method = "BY")
