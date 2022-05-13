#' ---
#' title: "Power analysis"
#' author: "PSA 008"
#' ---
#' 
#' This document describes the power analysis for PSA 008 Minimal
#' Groups.
 

#' 
#' ## R package information
#' 
## ----load-packages, message = FALSE, warning = FALSE--------------------------
## list of packages required
packages <- c(
  "tidyverse", # data wrangling
  "lme4", # random effects models
  "lmerTest", # random effects models
  "mixedpower", # estimating power in lme
  "simr", # simulating data
  "patchwork",# combine plots
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
#' We created an artificial dataset that contains the relevant variables in a separate script, which we can call here and in other files. Note the dataset here will include the variables used in the main analyses, not all collected variables. Furthermore, the dataset resembles the processed data, not the raw data from Qualtrics.
#' 
## ---- source-simulated-data---------------------------------------------------
## loads the simulated data, and wrangled dataframes for each RQ
source("./data-simulation.R")

## peek at dataframe
head(fake_data)

#' 
#' Our outcome measures of bias will be set to both a larger and
#' smaller value; we looked to the literature for an indication of
#' effect size.  The larger value is based on @Balliet-et-al_2014, a
#' meta-analysis of intergroup discrimination in behavioural economic
#' games. The overall effect size of games involving experimentally
#' created (i.e. minimal) groups was $d = 0.35 (0.27-0.42)$. The value
#' is similar to the overall effect size of $d = 0.37 (0.28-0.45; n =
#' 150)$ in games involving "artificial" (i.e. minimal) groups
#' reported in
#' @Lane_2016.
#' 
#' Effects sizes relevant to national groups in economic tasks are a
#' meta-analysis and a recent cross-cultural study. @Lane_2016 also
#' reports an overall effect size of ``national'' groups $d = 0.16
#' (0.04–0.29; n = 52)$, and
#' @Romano-et-al_2021 conducted a study across 42 nations finding $d =
#'     0.22 (0.19-0.25)$ (this compares ingroup with outgroup plus
#'     strangers; from supplementary figures, it appears that the
#'     difference between ingroup and outgroup is less than ingroup
#'     and stranger, so the overall effect size for ingroup outgroup
#'     is likely less than $0.22$). We are not aware of an overall
#'     effect size of family from a meta-analysis or large scale study
#'     to report an effect size. Therefore, we set the smaller effect
#'     size to 0.16, which is approximately half the larger value of
#'     0.35.
#' 
## ---- outcomes----------------------------------------------------------------
## set effect sizes
es_outcome_larger <- 0.35
es_outcome_smaller <- 0.16

#' 
#' We conduct a power analysis for each research question (RQ), using
#' one of the pre-specified mixed-effects models. For each fixed
#' effect we specify the beta coefficients (essentially, effect size),
#' for each random effect the variance, and finally the error variance
#' (\ie sigma). The error variance is set to 0.1 in all models.
#' 
## ---- basic-settings----------------------------------------------------------
## number of simulations
## n_sim <- 100 # to troubleshoot
n_sim <- 1000 # actual run

## sigma in models
residual_sd <- 1

## random effect for measure within slopes
rand_within_slopes <- 0.1

#' 
#' ## Research question 1
#' 
#' We have one model for RQ1:
#' "min_bias ~ measure + (1 | id) + (measure | country/lab)". For the
#' power analysis, we provide estimated intercepts and slopes. The
#' estimated intercept is 0.2 for the fixed effect "measure", while
#' for the random effects it is 0.1 ("id"), 0.5 ("country"), and 0.1
#' ("lab"). We expect variation at the country level to be greater
#' than the other two. All random slopes are set to 0.1.
#' 
## ---- rq1---------------------------------------------------------------------
## iterate over different effect sizes
for (min_bias_es in c(es_outcome_smaller, es_outcome_larger)) {
    
    ## fixed intercept 
    fixed <- c(
        min_bias_es, # bias
        rep(0.2, 2) # measure (with two levels)
    )
    
    ## random intercepts
    random <- list(
        0.1, # id 
        0.5, # country
        0.1, # lab
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1 # measure within slopes
    )
    
    ## construct model
    model_rq1 <- 
        
        makeLmer(
            min_bias ~ measure + (1 | id)  + (measure | country/lab),
            fixef = fixed, 
            VarCorr = random, 
            sigma = residual_sd, 
            data = df_rq1)
    
    ## summarise
    model_rq1
    
    ## vary number of countries
    ## example with n = 50
    model_rq1_country <- 
        
        extend(model_rq1, along = "country", n = 60)
    
                                        # ## check power of n = 60
                                        # sim_rq1_country <- 
                                        #   
                                        #   powerSim(model_rq1_country,
                                        #            nsim = n_sim)
    
                                        # ## summarise
                                        # sim_rq1_country
    
    ## p curve of different number of countries
    p_curve_rq1_countries <-
        
        powerCurve(model_rq1_country, 
                   along = "country",
                   nsim = n_sim,
                   breaks = c(seq(20, 60, by = 10))
                   )

    ## assingn powercurve to object for later use
    assign(
        ## name of object
        paste0("p_curve_rq1_countries_", min_bias_es),
        ## powercurve
        p_curve_rq1_countries
    )
    
    ## pcurve object for manipulation
    assign(paste0("p_curve_rq1_countries", min_bias_es),
           summary(p_curve_rq1_countries) %>%
           mutate(bias_es = min_bias_es))
    
}

## bind different effect sizes
power_curve_rq1 <-
    
    bind_rows(
        mutate(summary(p_curve_rq1_countries_0.16), bias_es = "0.16"), 
        mutate(summary(p_curve_rq1_countries_0.35), bias_es = "0.35")
    ) %>%
    ggplot(aes(nlevels, mean, 
               colour = as.factor(bias_es),
               group = as.factor(bias_es))) +
    geom_hline(yintercept = 0.95, linetype = 3) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
    scale_x_continuous(breaks = c(seq(20, 60, by = 10))) +
    scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.2))) +
    scale_colour_discrete("effect size") +
    labs(x = "no. countries",
         y = "power (%)") +
    theme_bw() +
    theme(legend.position="bottom")

power_curve_rq1

## save for later reuse
save.image(file = paste0("power-analysis-rq1-", n_sim, ".RData"))

#' 
#' ## Research question 2
#' 
#' ### Individual level 
#' 
#' For the individual level analysis of research question 2, we have
#' three main moderators of interest: permeability, (in-group and
#' out-group) trust, and self-esteem. Our confirmatory analyses
#' compares six models. For the power analysis, we use one of models
#' that includes one moderator:
#' "min_bias ~ measure * self_esteem + (1 | id) + (self_esteem | country/lab)". Intercepts
#' for measure, id, country, and lab are the same as for RQ1. Again,
#' all random slopes are set to 0.1, with only one random slope for tractability
#' 
#' We add intercepts for the three moderators as follows. The
#' self-esteem beta coefficient is set to 0.11, which corresponds to
#' half the overall effect size reported in
#' @Aberson-et-al_2000 of $d = $ 0.23. This is the difference in
#'     in-group bias (across different measures, not limited to
#'     minimal groups) between individuals with high \vs low
#'     self-esteem (across a variety of self-esteem
#'     measures). Although there is a wide range of effect sizes, the
#'     measures reported are not identical to our planned study, and
#'     the estimate is relatively old, it is the best estimate we are
#'     aware of.  We are aware of no effect sizes of trust and
#'     permeability in the literature to rely on for the power
#'     analysis, so if we were to include these we would set the beta
#'     coefficients to 0.1.
#' 
## ---- pwr-rq2-----------------------------------------------------------------
## iterate over different effect sizes
for (min_bias_es in c(es_outcome_smaller, es_outcome_larger)) {
    
    ## fixed intercept 
    fixed <- c(
        min_bias_es, # bias
        rep(0.2, 2), # measure (with two levels)
        0.11, # self_esteem
        rep(0.1, 2) # measure interaction with moderator (with two levels)
    )
    
    ## random intercepts
    random <- list(
        0.1, # id 
        0.5, # country
        0.1, # lab
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1 # measure within slopes
    )
    
    ## specify maximal model  
    model_rq2 <- 
        
        makeLmer(
            min_bias ~ measure * self_esteem + 
                (1 | id) + 
                (self_esteem | country/lab),
            fixef = fixed, 
            VarCorr = random, 
            sigma = residual_sd, 
            data = df_rq2)
    
    ## summarise
    model_rq2
    
                                        # ## estimate the power to detect the effect of each moderator in the model
                                        # ## against the other fixed effects
                                        # sim_rq2_self_esteem <- 
                                        #   
                                        #   powerSim(model_rq2,
                                        #            nsim = n_sim,
                                        #            test = fcompare(min_bias ~ measure * (trust + permeability))
                                        #           )
                                        # 
                                        # sim_rq2_self_esteem
    
    ## vary number of participants 
    ## to 140 per country (and lab)
    ## NB n has to be set per measure
    model_rq2_different_participants <- 
        
        extend(model_rq2,
               # test = fcompare(min_bias ~ measure * (trust + permeability)),
               within = "country+measure", 
               n = 140)
    
    ## p curve of different number of participants
    ## for one moderator against the other fixed effects
    p_curve_rq2_participants <-
        
        powerCurve(model_rq2_different_participants, 
                   test = fcompare(min_bias ~ measure * (trust + permeability)), 
                   within="country+measure",
                   nsim = n_sim,
                   breaks = c(seq(80, 140, by = 10))
                   )
    
    ## assign to object
    assign(
        ## name of object
        paste0("p_curve_rq2_participants_", min_bias_es),
        ## powercurve
        p_curve_rq2_participants
        
    )
    
    ## pcurve object
    assign(paste0("p_curve_rq2_participants_", min_bias_es),
           summary(p_curve_rq2_participants) %>%
           mutate(bias_es = min_bias_es))
    
}

## bind different effect sizes
power_curve_rq2 <-
    
    bind_rows(
        p_curve_rq2_participants_0.16,
        p_curve_rq2_participants_0.35
    ) %>%
    ggplot(aes(nlevels, mean, 
               colour = as.factor(bias_es), group = as.factor(bias_es))) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    scale_x_continuous(breaks = c(seq(80, 140, by = 10))) +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    labs(x = "no. participants",
         y = "power (%)") +
    guides(colour = "none") +
    theme_bw()

power_curve_rq2

## save for later reuse
save.image(file = paste0("power-analysis-rq2-", n_sim, ".RData"))

#' 
#' ## Research question 3
#' 
#' For research question 3, we assess whether real-world bias (towards
#' the nation and/or family) is predicted by minimal group bias. The
#' model is
#' "att_real_bias ~ att_min_bias * group_type + (1 | id) + (att_min_bias + group_type | country/lab)". We
#' keep the same intercepts for measure, id, country, and lab as for
#' RQ1 and RQ2. Again, all random slopes are set to 0.1. We set
#' minimal bias to 0.16, the smaller effect size used as an outcome in
#' RQ1 and RQ2.
#' 
## ---- pwr-rq3-----------------------------------------------------------------
## iterate over different effect sizes
for (min_bias_es in c(es_outcome_smaller, es_outcome_larger)) {

## set intercept and slopes
    fixed <-
        c(
            min_bias_es, # bias
            0.16, # min bias
            0.2, # effect of group type
            0.10 # effects of interaction with min bias and group type
        )
    
    ## random intercepts
    random <- list(
        0.1, # id 
        0.5, # country
        0.1, # lab
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1, # measure within slopes
        0.1 # measure within slopes
)
    
    ## specify maximal model  
    model_rq3 <- 
        
        makeLmer(
            att_real_bias ~ att_min_bias * group_type + 
                       (1 | id)  + 
                (att_min_bias + group_type | country/lab),
            fixef = fixed, 
            VarCorr = random, 
            sigma = residual_sd,
            data = df_rq3)

    ## summarise
    model_rq3
    
    ## vary number of participants 
    ## to 140 per country (and lab)
    ## NB n has to be set per measure
    model_rq3_different_participants <- 
        
        extend(model_rq3, within="country+group_type", n = 140)
    
    ## p curve of different number of participants
    ## for one moderator against the other fixed effects
    p_curve_rq3_participants <-
        
        powerCurve(model_rq3_different_participants, 
                   test = fcompare(att_real_bias ~ group_type),
                   within="country+group_type", 
                   nsim = n_sim,
                   breaks = c(seq(80, 140, by = 10))
                   )
    
    ## assign object for later reuse
    assign(
        ## name of object
        paste0("p_curve_rq3_participants_", min_bias_es),
        ## powercurve
        p_curve_rq3_participants
    )
    
    ## pcurve object
    assign(paste0("p_curve_rq3_participants_", min_bias_es),
           summary(p_curve_rq3_participants) %>%
           mutate(bias_es = min_bias_es))
    
}

## bind different effect sizes
power_curve_rq3 <-
    
    bind_rows(
        p_curve_rq3_participants_0.16,
        p_curve_rq3_participants_0.35
    ) %>%
    ggplot(aes(nlevels, mean, 
               colour = as.factor(bias_es), group = as.factor(bias_es))) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    scale_x_continuous(breaks = c(seq(80, 140, by = 10))) +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    labs(x = "no. participants",
         y = "power (%)") +
    guides(colour = "none") +
    theme_bw()

## save for later reuse
save.image(file = paste0("power-analysis-rq3-", n_sim, ".RData"))

#' 
#' ### Generate plot
#' 
## ---- plot-pcurve-------------------------------------------------------------

## combine plots
(power_curve_rq1 + power_curve_rq2 + power_curve_rq3) + 
  plot_annotation(tag_levels = "a")

#' 
#' ### Save all RData
#' 
## ---- save-objects------------------------------------------------------------
save.image(file = paste0("power-analysis", n_sim, ".RData"))
