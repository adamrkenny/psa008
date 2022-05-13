#' ---
#' title: "Pilot study: summary and analysis"
#' author: "PSA 008"
#' ---
#' 
#' This document describes the analysis pipeline for PSA 008 Minimal
#' Groups, using the pilot study.
#' 

#' 
#' ## R package information
#' 
## ----load-packages, message = FALSE, warning = FALSE, echo = FALSE, include = FALSE----
## list of packages required
packages <- c(
  "tidyverse", # data wrangling
  "lme4", # random effects models
  "lmerTest", # random effects models
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

#' 
## ----sessioninfo, message = FALSE, warning = FALSE----------------------------
sessionInfo() # session info

#' 
#' ## Input pilot data
#' 
#' Pilot data imported and wrangled separately.
#' 
## ---- source-data, include = FALSE--------------------------------------------
## loads the wrangled data and analyses
df <- read_csv("../data/data-pilot-02-processed.csv")

## peek at data
head(df)

#' 
#' # Analysis
#' 
#' The (minimal group) dependent measures are based on the three
#' dictator games (in-group--self, out-group--self,
#' in-group--out-group) and the average attitude towards in-group and
#' towards out-group. The resultant three (minimal group) measures
#' are: difference between in-group and out-group attitudes
#' (att_bias), difference between in-group–self and out-group–self
#' decisions in the dictator game (dg_first_bias), and the decision in
#' the in-group–out-group dictator game (dg_third_bias).
#' 
#' ## Descriptives
#' 
## ----bias-descriptives--------------------------------------------------------

## descriptives of all bias measures
for (i in c("att_min_bias", "att_nat_bias", "att_fam_bias", 
            "dg_min_bias_first", "dg_nat_bias_first", "dg_fam_bias_first",
            "dg_min_bias_third", "dg_nat_bias_third", "dg_fam_bias_third")) {

    assign(paste0("df_", i),
           df %>%
           summary_stats(i)
           )
           
}

## check descriptives, as reported in main text
## NB these are unstandardized

## minimal group
df_att_min_bias
df_dg_min_bias_first
df_dg_min_bias_third

## family group
df_att_fam_bias
df_dg_fam_bias_first
df_dg_fam_bias_third

## national group
df_att_nat_bias
df_dg_nat_bias_first
df_dg_nat_bias_third

## t-tests for p values reported in main text

## minimal group
t.test(df$att_min_bias)
t.test(df$dg_min_bias_first)
t.test(df$dg_min_bias_third)

## family group
t.test(df$att_fam_bias)
t.test(df$dg_fam_bias_first)
t.test(df$dg_fam_bias_third)

## national group
t.test(df$att_nat_bias)
t.test(df$dg_nat_bias_first)
t.test(df$dg_nat_bias_third)

## ----bias-plot, eval = FALSE--------------------------------------------------
## create long dataframe
df2 <-

    df %>%
    ## ## NB to generate standardized, uncomment next line
    ## mutate(across(contains("_bias"), ~(scale(.) %>% as.vector))) %>%
    pivot_longer(cols = c(contains("_bias")),
                 names_to = "measure",
                 values_to = "value") %>%
    separate(measure, into = c("type", "group", "bias", "dgtype"), sep = "_") %>%
    mutate(type = case_when(type == "dg" & dgtype == "first" ~ "dg_first",
                            type == "dg" & dgtype == "third" ~ "dg_third",
                            type == "att" ~ "att",
                            TRUE ~ NA_character_)) %>%
    unite(col = "grouptype", c("type", "group"), remove = FALSE) %>%
    mutate(group = factor(group, levels = c("min", "fam", "nat"),
                          labels = c("minimal", "family", "national"))) %>%
    mutate(min_value = if_else(type == "att", -6, -20)) %>% 
    mutate(max_value = if_else(type == "att", 6, 20)) %>%
    mutate(interval_value = if_else(type == "att", 1, 4))
    ## ## NB to produce better plot with standardized data
    ## ## comment above and uncomment below next line
    ## mutate(min_value = if_else(type == "att", -7, -7)) %>% 
    ## mutate(max_value = if_else(type == "att", 7, 7)) %>%
    ## mutate(interval_value = if_else(type == "att", 2, 2))


## generate plot of each measure, faceted by country
for (measure in c("att", "dg_first", "dg_third")) {

    df2_measure <-

    df2 %>%
    filter(type == measure)            
    
    bias_summary_measure <-
        
        df2_measure %>%
        Rmisc::summarySE(measurevar = "value",
                         groupvars = c("group", "country"),
                         na.rm = TRUE)
    
    assign(paste0("plot_bias_", measure),

           df2_measure %>%
           mutate(intercept_no_bias = 0) %>%
           ggplot(aes(x = group,
                      y = value,
                      fill = group, 
                      colour = group
                      )) +
           geom_flat_violin(position = position_nudge(x = .25,
                                                      y = 0),
                            adjust = 2,
                            trim = TRUE,
                            na.rm = TRUE,
                            alpha = 0.5) +
           geom_point(position =
                          position_jitter(width = .05, height = .05),
                      size = 2,
                      alpha = 0.4,
                      na.rm = TRUE
                      ) +
           geom_errorbar(data = bias_summary_measure,
                         aes(ymin = value - ci,
                             ymax = value + ci),
                         position = position_nudge(y = 0.25),
                         colour = "grey40",
                         width = 0,
                         size = 1.5
                         ) +
           geom_point(data = bias_summary_measure,
                      position = position_nudge(y = 0.25),
                      size = 4,
                      shape = 19,
                      fill = "black",
                      colour = "grey40"
                      ) +
           geom_hline(aes(yintercept = intercept_no_bias),
                      linetype = "dashed",
                      colour = "grey40") +
           facet_grid(. ~ country,
                      scales = "free",
                      space = "free",
                      shrink = TRUE,
                      drop = TRUE
                      ) +
           scale_y_continuous(breaks = c(seq(unique(df2_measure$min_value),
                                             unique(df2_measure$max_value),
                                             unique(df2_measure$interval_value))),
                              limits = c(unique(df2_measure$min_value),
                                         unique(df2_measure$max_value))) +
           scale_fill_brewer(palette = "Set2") +
           scale_colour_brewer(palette = "Set2") +
           labs(x = "",
                y = "bias") +
           guides(fill = "none",
                  colour = "none"
                  ) +
           theme_classic() +
           theme(text = element_text(size = 20),
                 axis.text.x=element_text(angle = 45, hjust = 1),
                 strip.placement = "outside")
           )    
    
}

## combine plots
plot_bias <-

    (
        (plot_bias_att +
         ggtitle("attitudinal bias")) +
        (plot_bias_dg_first +
         ggtitle("first-party bias")) +
        (plot_bias_dg_third +
         ggtitle("third-party bias"))
    ) +
    plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
                    caption = "bias not standardized" # "standardized"
                    ) &
    theme(plot.tag = element_text(face = "bold"))

## plot included as figure 1 in RR 
plot_bias

#' 
## ----rq2-descriptives-selfesteem----------------------------------------------

## descriptives of moderators
for (i in c("self_esteem", "trust_in_out", "trust_institution",
            "permeability", "family_tie_sum", "embeddedness")) {

    assign(paste0("df_", i),
           df %>%
           summary_stats(i)
           )

}

## check descriptives, e.g. for main moderators
df_self_esteem
df_trust_in_out
df_permeability
## NB these are unstandardized

## plot of self-esteem
plot_self_esteem <-
    
    df %>%
    ggplot(aes(x = self_esteem)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) + #    geom_histogram(stat = "count") +
    labs(x = "self-esteem") +
    scale_x_continuous(breaks = unique(df$self_esteem)) + #, lim = c(1,7)) +
    scale_y_continuous("percentage (%)", labels = scales::percent) +
    facet_wrap(. ~ country) +
    theme_classic() +
    theme(text = element_text(size = 15))

## print plot
plot_self_esteem

## plot other measures (that are not binned)
for (moderator in c("trust_in_out", "trust_institution", "permeability",
                    "family_tie_sum", "embeddedness")) {
                      
    assign(paste0("plot_", moderator),

           df %>%
           select(moderator, country) %>%
           ggplot(aes(x = .[[1]])) + 
           geom_density() +
           facet_wrap(. ~ country) +
           theme_classic() +
           labs(x = moderator) +
           # scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
           theme(text = element_text(size = 15))

           )

}
           
## print plots, e.g.
plot_trust_in_out
plot_trust_institution
plot_permeability

#' 
#' ## Research question 1
#' 
#' We then use outcome minimal bias (min_bias), which includes all three (standardised) measures of bias with the minimal groups. Measure can then be include as a random effect.
#' 
## ---- rq1-manipulation, include = FALSE---------------------------------------
## RQ1

## long dataframe with only relevant vars
df_rq1 <-

    df %>%
    select(id, lab, country,
           att_min_bias, dg_min_bias_first, dg_min_bias_third,
           age, gender, political_orientation, income_when16) %>%
    ## standardize
    mutate(att_min_bias = as.vector(scale(att_min_bias)),
           dg_min_bias_first = as.vector(scale(dg_min_bias_first)),
           dg_min_bias_third = as.vector(scale(dg_min_bias_third))) %>%
    pivot_longer(cols = c(contains("min_bias")), 
                 names_to = "measure",
                 values_to = "min_bias") %>%
    mutate(measure = case_when(str_detect(measure, "att") ~ "att",
                                  str_detect(measure, "first") ~ "dg_first",
                                  str_detect(measure, "third") ~ "dg_third"
                                  )) %>%
    mutate(att_dummy = if_else(measure == "att", 1, 0),
           dg_first_dummy = if_else(measure == "dg_first", 1, 0),
           dg_third_dummy = if_else(measure == "dg_third", 1, 0)
           )

## peek at dataframe
head(df_rq1)

#' 
#' We can run the model either with measure combined or as dummy; I find it easier to extract country-level random effects if we use dummy.
#' 
## ---- rq1-combined-dv-group---------------------------------------------------
## model with country
model_country_all <- 

    lmerTest::lmer(min_bias ~ measure + 
                       (1 | id)  + 
                       (measure | country/lab),
                   data = df_rq1
                   ## , contrasts = list(measure = "contr.sum")
                   )

## summary
summary(model_country_all)

## icc
## country variance / (residual + country variance)
## TODO insert values

## dotplot
gg_caterpillar(ranef(model_country_all, condVar = TRUE), QQ = FALSE, 
              likeDotplot = FALSE)

## predict the scores based on the model
df_rq1 %>% 
  mutate(mdl = predict(model_country_all)) %>%
  ggplot(aes(min_bias, y = mdl, colour = country, group = country)) + 
  geom_smooth(se = F, method = lm) +
  theme_bw()

#' 
## ---- rq1-combined-dv-dummy---------------------------------------------------
## country
model_country_dummy <- 

    lmerTest::lmer(min_bias ~ att_dummy + dg_first_dummy + 
                       (1 | id)  + 
                       (att_dummy + dg_first_dummy + dg_third_dummy | country/lab),
                   data = df_rq1)

## summary
summary(model_country_dummy)

## icc
## country variance / (residual + country variance)
## TODO insert values

## dotplot
gg_caterpillar(ranef(model_country_dummy, condVar = TRUE), QQ = FALSE, 
              likeDotplot = TRUE)

## graph with predicted country level min bias

## predict the scores based on the model
df_rq1 %>%
  mutate(mdl = predict(model_country_dummy)) %>%
  ggplot(aes(min_bias, y = mdl, colour = country, group = country)) + 
  geom_smooth(se = FALSE, method = lm) +
  theme_bw()

## graph of random effects with line of best fit

## save coefficients
coefs_model <- coef(model_country_dummy)

## print random effects and best line
## shown is just dg_third_dummy
## NB this will be meaningless with two countries
coefs_model$country %>%
  mutate(country = rownames(coefs_model$country),
         intercept = `(Intercept)`) %>% 
  ggplot(aes(x = dg_third_dummy, y = intercept, label = country)) + 
  geom_point() + 
  geom_smooth(se = F, method = lm) +
  geom_label(nudge_y = 0.0025, alpha = 0.5) +
  theme_bw()

#' 
## ----df-rq1-additional, include = FALSE----------------------------------------

##### demographics
## include demographic variables as robustness check

## model with country
model_country_all_demographics <- 

    lmerTest::lmer(min_bias ~ measure + age + income_when16 + political_orientation +
                       (1 | id)  + 
                       (measure | country/lab),
                   data = df_rq1
                   ## , contrasts = list(measure = "contr.sum")
                   )

## summary
summary(model_country_all_demographics)

## run anova to check significance of demographic vars
anova(model_country_all_demographics, ddf = "Kenward-Roger")

##################################################
#' 
#' ## Research question 2
#' 
#' For the individual level analysis of research question 2, we have
#' three main moderators of interest: permeability, trust (both
#' interpersonal and institutional), and self-esteem. We compare
#' various models:
#'
#' NB trust_in_out is the same as interpersonal trust
#' 
## ----df-rq2, include = FALSE--------------------------------------------------

## PCA for family ties (following Alesina & Giuliano, 2010)
family_ties_pca <-

    prcomp(select(df, starts_with("family_tie_"), -family_tie_sum),
           center = TRUE,
           scale. = TRUE)

## extract first component
family_ties_pc1 <-

    predict(pca_family_ties, df) %>% 
    data.frame() %>%
    mutate(family_tie_pc1 = PC1) %>%
    select(family_tie_pc1)

## add pc1 to df 
df <-

    bind_cols(df, family_ties_pc1)

## long dataframe with only relevant vars
df_rq2 <-
    
    df %>%
    select(id, lab, country, 
          att_min_bias, dg_min_bias_first, dg_min_bias_third,
          self_esteem, trust_in_out, trust_institution, permeability,
          embeddedness, family_tie_pc1) %>%
    ## standardize outcomes
    mutate(att_min_bias = as.vector(scale(att_min_bias)),
           dg_min_bias_first = as.vector(scale(dg_min_bias_first)),
           dg_min_bias_third = as.vector(scale(dg_min_bias_third))) %>%
    ## standardize predictors
    mutate(self_esteem = as.vector(scale(self_esteem)),
           trust_in_out = as.vector(scale(trust_in_out)),
           trust_institution = as.vector(scale(trust_institution)),
           permeability = as.vector(scale(permeability)),
           embeddedness = as.vector(scale(embeddedness))         
           ) %>%
    pivot_longer(cols = c(contains("min_bias")), 
                 names_to = "measure",
                 values_to = "min_bias") %>%
    mutate(measure = case_when(str_detect(measure, "att") ~ "att",
                                  str_detect(measure, "first") ~ "dg_first",
                                  str_detect(measure, "third") ~ "dg_third"
                                  )) %>%
    mutate(att_dummy = if_else(measure == "att", 1, 0),
           dg_first_dummy = if_else(measure == "dg_first", 1, 0),
           dg_third_dummy = if_else(measure == "dg_third", 1, 0)
           )

## peek at dataframe
head(df_rq2)

#' 
## ----rq2-models---------------------------------------------------------------
## minimal model
model_rq2_min <-
    
    lmerTest::lmer(
        min_bias ~ measure +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## model with esteem
model_rq2_esteem <-
    
    lmerTest::lmer(
        min_bias ~ measure * self_esteem +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## model with trust (in/out)
model_rq2_trust_in_out <-
    
    lmerTest::lmer(
        min_bias ~ measure * trust_in_out +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## model with trust (institutional)
model_rq2_trust_institution <-
    
    lmerTest::lmer(
        min_bias ~ measure * trust_institution +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## model permeability
model_rq2_permeability <-
    
    lmerTest::lmer(
        min_bias ~ measure * permeability +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## model with esteem and trust
model_rq2_esteem_trust_in_out <-
    
    lmerTest::lmer(
        min_bias ~ measure * (self_esteem + trust_in_out) +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## model with esteem and trust (both in/out and institution)
model_rq2_esteem_trust_both <-
    
    lmerTest::lmer(
        min_bias ~ measure * (self_esteem + trust_in_out + trust_institution) +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## maximal model
model_rq2_max <-
    
    lmerTest::lmer(
        min_bias ~ measure * (self_esteem + trust_in_out + trust_institution + permeability) +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

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

#' 
## ----df-rq2-additional, include = FALSE----------------------------------------

##### other indicators
## additional individual-level analyses that include other measures
## linked to permeability as robustness checks

## model embeddedness
model_rq2_embeddedness <-
    
    lmerTest::lmer(
        min_bias ~ measure * embeddedness +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## model family ties
model_rq2_family_ties <-
    
    lmerTest::lmer(
        min_bias ~ measure * family_tie_pc1 +  
          (1 | id)  + 
          (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
        data = df_rq2)

## AIC comparison
AIC(
    model_rq2_permeability,
    model_rq2_embeddedness,
    model_rq2_family_ties
)

##### demographics
## include demographic variables as robustness checks

## NB this will be the model with lowest AIC, here using min for demonstration purposes

## model with country
model_rq2_min_demographics <- 

    lmerTest::lmer(
                  min_bias ~ measure + age + income_when16 + political_orientation +
                      (1 | id)  + 
                      (self_esteem + trust_in_out + trust_institution + permeability | country/lab),
                  data = df_rq1
              )

## summary
summary(model_rq2_min_demographics)

## run anova to check significance of demographic vars
anova(model_rq2_min_demographics, ddf = "Kenward-Roger")

#' 
#' 
#' ## Research question 3
#' 
#' For research question 3, we assess whether real-world bias (towards
#' the nation and/or family) is predicted by minimal group bias. 
#' 
## ----measures-real-world-bias-------------------------------------------------
## longish dataframe with relevant vars
## NB for only one measure
df_rq3_att <-
    
    df %>%
    select(id, lab, country,
           att_min_bias, att_nat_bias, att_fam_bias,
           dg_min_bias_first, dg_nat_bias_first, dg_fam_bias_first) %>%
    pivot_longer(cols = c(att_nat_bias, att_fam_bias),, 
                 names_to = "group_type",
                 values_to = "att_real_bias") %>%
    mutate(group_type = case_when(str_detect(group_type, "nat") ~ "nat",
                                  str_detect(group_type, "fam") ~ "fam"
                                  ))

df_rq3_dg_first <-
    
    df %>%
    select(id, lab, country,
           ## att_min_bias, att_nat_bias, att_fam_bias,
           dg_min_bias_first, dg_nat_bias_first, dg_fam_bias_first) %>%
    pivot_longer(cols = c(dg_nat_bias_first, dg_fam_bias_first),, 
                 names_to = "group_type",
                 values_to = "dg_real_bias_first") %>%
    mutate(group_type = case_when(str_detect(group_type, "nat") ~ "nat",
                                  str_detect(group_type, "fam") ~ "fam"
                                  ))

df_rq3_dg_third <-
    
    df %>%
    select(id, lab, country,
           ## att_min_bias, att_nat_bias, att_fam_bias,
           dg_min_bias_third, dg_nat_bias_third, dg_fam_bias_third) %>%
    pivot_longer(cols = c(dg_nat_bias_third, dg_fam_bias_third),, 
                 names_to = "group_type",
                 values_to = "dg_real_bias_third") %>%
    mutate(group_type = case_when(str_detect(group_type, "nat") ~ "nat",
                                  str_detect(group_type, "fam") ~ "fam"
                                  ))

## peek at dataframe
head(df_rq3_att)

#' 
#' To avoid three-way interactions, we model each measure separately. 
#' 
#' ### Attitude
#' 
#' We first demonstrate the analysis with the attitude measures.
#' 
## ----rq3-att------------------------------------------------------------------
## real-world
model_real_world <- 

    lmerTest::lmer(att_real_bias ~ att_min_bias * group_type + 
                       (1 | id)  + 
                       (att_min_bias + group_type | country/lab),
                   data = df_rq3_att)

## summary
summary(model_real_world)

#' 
## ----plot-rq3-att-------------------------------------------------------------

model_coefs <- 
  
  coef(model_real_world)$country %>% 
  rename(intercept = `(Intercept)`, slope = att_min_bias) %>% 
  rownames_to_column("country")

model_coefs

df_rq3_rand <- left_join(df_rq3_att, model_coefs, by = "country")

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
#' ### First-party DG
#' 
#' We then repeat with the first-party DG.
#' 
## ----rq3-dgfirst--------------------------------------------------------------
## real-world
model_real_world <- 

    lmerTest::lmer(dg_real_bias_first ~ dg_min_bias_first * group_type + 
                       (1 | id)  + 
                       (dg_min_bias_first + group_type | country/lab),
                   data = df_rq3_dg_first)

## summary
summary(model_real_world)

#' 
## ----plot-rq3-dgfirst---------------------------------------------------------

model_coefs <- 
  
  coef(model_real_world)$country %>% 
  rename(intercept = `(Intercept)`, slope = dg_min_bias_first) %>% 
  rownames_to_column("country")

model_coefs

df_rq3_rand <- left_join(df_rq3_dg_first, model_coefs, by = "country")

ggplot(data = df_rq3_rand, 
       mapping = aes(x = dg_min_bias_first, 
                     y = dg_real_bias_first, 
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
#' ### Third-party DG
#' 
#' We finally demonstrate the analysis with the third-party DG.
#' 
## ----rq3-dgthird--------------------------------------------------------------
## real-world
model_real_world <- 

    lmerTest::lmer(dg_real_bias_third ~ dg_min_bias_third * group_type + 
                       (1 | id)  + 
                       (dg_min_bias_third + group_type | country/lab),
                   data = df_rq3_dg_third)

## summary
summary(model_real_world)

#' 
## ----plot-rq3-dgthird---------------------------------------------------------

model_coefs <- 
  
  coef(model_real_world)$country %>% 
  rename(intercept = `(Intercept)`, slope = dg_min_bias_third) %>% 
  rownames_to_column("country")

model_coefs

df_rq3_rand <- left_join(df_rq3_dg_third, model_coefs, by = "country")

ggplot(data = df_rq3_rand, 
       mapping = aes(x = dg_min_bias_third, 
                     y = dg_real_bias_third, 
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
## ----df-rq3-additional, include = FALSE----------------------------------------

##### demographics
## include demographic variables as robustness checks

## NB this will be with all models, here using attitudes measures for
## demonstration purposes

## model with country
model_real_world_demographics <- 

    lmerTest::lmer(
                  att_real_bias ~ att_min_bias * group_type + age + income_when16 + political_orientation +
                      (1 | id)  + 
                       (att_min_bias + group_type | country/lab),
                  data = df_rq3_att
              )

## summary
summary(model_real_world_demographics)

## run anova to check significance of demographic vars
anova(model_rq2_min_demographics, ddf = "Kenward-Roger")

##### first cousins
## run the same as above excluding those with no first cousins
## i.e.
df %>% count(num_cousins) # would exclude 19 participants from pilot

#' 
## ----df-rq1-rq3-additional, include = FALSE-------------------------------------

## for RQ1--RQ3, the models are repeated (except for RQ2 where lowest
## AIC is), but excluding those who demonstrated familiarity
df %>%
    count(mgp_familiarity, mgp_followup) # would be 3 participants
                                         # from pilot, based on both questions
