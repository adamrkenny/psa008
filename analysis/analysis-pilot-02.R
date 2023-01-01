## title: "Pilot study: summary and analysis"
## author: "PSA 008" v. R&R

## This document describes the analysis pipeline for PSA 008 Minimal
## Groups, using the second pilot study.

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

## sessioninfo
sessionInfo() # session info

## Input pilot data
 
## Pilot data imported and wrangled separately.

## source-data
## loads the wrangled data and analyses
df <- read_csv("../data/data-pilot-02-processed.csv")

## peek at data
head(df)

## Analysis
## 
## The (minimal group) dependent measures are based on the three
## dictator games (in-group--self, out-group--self,
## in-group--out-group) and the average attitude towards in-group and
## towards out-group. The resultant three (minimal group) measures
## are: difference between in-group and out-group attitudes
## (att_bias), difference between in-group–self and out-group–self
## decisions in the dictator game (dg_first_bias), and the decision in
## the in-group–out-group dictator game (dg_third_bias).
 
## Descriptives

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

## bias plot (Figure 1 in manuscript)

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
                          labels = c("min", "fam", "nat"))) %>%
    mutate(min_value = case_when(type == "att" ~ -6,
                                 dgtype == "first" ~ -20,
                                 dgtype == "third" ~ -10
                                 )) %>% 
    mutate(max_value = case_when(type == "att" ~ 6,
                                 dgtype == "first" ~ 20,
                                 dgtype == "third" ~ 10
                                 )) %>%
    mutate(interval_value = case_when(type == "att" ~ 1,
                                 dgtype == "first" ~ 4,
                                 dgtype == "third" ~ 2
                                 )) %>%
    mutate(intercept_no_bias = case_when(type == "att" ~ 0,
                                 dgtype == "first" ~ 0,
                                 dgtype == "third" ~ 0
                                 ))

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
           ## mutate(intercept_no_bias = 0) %>%
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
           theme(text = element_text(size = 33),
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
    plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"
                   ## ,
                   ##  caption = "bias not standardized" # "standardized"
                    ) &
    theme(plot.tag = element_text(face = "bold"))

## plot included as figure 1 in RR 
plot_bias

## save plot for manuscript
## plot_bias %>%
##     ggsave(filename = paste(Sys.Date(), "bias-pilot-02.png", sep = "_"),  
##            bg = "transparent")

##################################################
## Research question 1

##################################################
## meta-analytical approach

## create long dataframe
df2 <-

    df %>% 
    ## ## NB to generate standardized, uncomment next line
    ## mutate(across(contains("_bias"), ~(scale(.) %>% as.vector))) %>%
    mutate(dg_min_in_self = 20 - dg_min_in_self,
           dg_min_out_self = 20 - dg_min_out_self,
           dg_min_in_out_out = 20 - dg_min_in_out,
           ) %>%
    pivot_longer(c("dg_min_in_self", "dg_min_out_self",
                   "dg_min_in_out", "dg_min_in_out_out",
                   "att_min_in", "att_min_out"),
                 names_to = "measure", values_to = "amount") %>%
    separate(col = measure, into = c("measure", "type"), extra = "merge") %>%
    mutate(measure = case_when(## measure == "dg" & type == "min_in_out" ~ "dg_third",
                               measure == "dg" & str_detect(type, "min_in_out") ~ "dg_third",
                               measure == "dg" ~ "dg_first",
                               measure == "att" ~ "att")) %>%
    mutate(group = case_when(## str_detect(type, "_in_out") ~ "both",
                             str_detect(type, "_in_out_out") ~ "out",
                             str_detect(type, "_in_out") ~ "in",
                             str_detect(type, "_in") ~ "in",
                             str_detect(type, "_out") ~ "out"
                             )) %>%
    select(id, lab, country, measure, group, amount)

##################################################
## Attitudes

## calculate d and sv
## first calculate various measures (e.g. sd, r)
df_rq1_ma <-

    filter(df2, measure == "att") %>%
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

## We show a cleveland plot of the raw data, showing the mean attitude
## towards the in-group is greater than that to the out-group in both
## countries:

filter(df2, measure == "att") %>%
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

## The distribution of effect sizes is show in the following forest
## plot. There are two positive estimated effects (GBR, USA):

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

## According to the Q test, there was no significant heterogeneity (`r
## format(round(es_country$QE, 2), nsmall = 2)`, p = `r
## format(round(es_country$QEp, 3), nsmall = 3)`). $I^2$ showed
## moderate heterogeneity ($I^2$ = `r format(round(I_squared, 2),
## nsmall = 2)`), assuming common thresholds (more than 50%). Tau
## showed zero heterogeneity ($\tau$ = `r
## format(round(es_country$tau2, 2), nsmall = 2)`).

##################################################
## First party allocation

## for dg_first

## calculate d and sv
## first calculate various measures (e.g. sd, r)
df_rq1_ma <-

    filter(df2, measure == "dg_first") %>%
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

## We show a cleveland plot of the raw data, showing the mean allocation
## to the in-group is greater than that to the out-group in both
## countries:

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

## The distribution of effect sizes is show in the following forest
## plot. There are two non-significant effects (GBR, USA):

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

## To determine the amount of variability in countries, we report report heterogeneity measures:

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

## According to the Q test, there was no significant heterogeneity (`r
## format(round(es_country$QE, 2), nsmall = 2)`, p = `r
## format(round(es_country$QEp, 3), nsmall = 3)`). $I^2$ showed low
## heterogeneity ($I^2$ = `r format(round(I_squared, 2), nsmall =
## 2)`), assuming common thersholds (less than 25% suggests it is not
## important). Tau showed zero heterogeneity ($\tau$ = `r
## es_country$tau2`).

##################################################
### Third party allocations

## for dg_third

## Unlike the other two effect sizes, we use the standardized mean
## difference effect (Cohen's $d_z$). We first show the mean
## difference scores.

filter(df2, measure == "dg_third") %>%
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

    filter(df2, measure == "dg_third") %>%
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

## As shown in the summary above, the overall effect size is `r round(coef(summary(es_country))$estimate, 2)` (`r round(coef(summary(es_country))$ci.lb, 2)`, `r round(coef(summary(es_country))$ci.ub, 2)`).

## To determine the amount of variability in countries, we report report
## heterogeneity measures:

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

## According to the Q test, there was no significant heterogeneity (`r
## format(round(es_country$QE, 2), nsmall = 2)`, p = `r
## format(round(es_country$QEp, 3), nsmall = 3)`). $I^2$ showed low
## heterogeneity ($I^2$ = `r format(round(I_squared, 2), nsmall =
## 2)`), assuming common thersholds (less than 25% suggests it is not
## important). Tau showed zero heterogeneity ($\tau$ = `r
## es_country$tau2`).

##################################################
## Research Question 2

## long dataframe with only relevant vars
df_rq2 <-
    
    df %>%
    select(id, lab, country, 
          att_min_bias, dg_min_bias_first, dg_min_bias_third,
          self_esteem, trust_in_out, trust_institution, permeability,
          embeddedness, family_tie_sum) %>%
    ## ## standardize outcomes
    ## mutate(att_min_bias = as.vector(scale(att_min_bias)),
    ##        dg_min_bias_first = as.vector(scale(dg_min_bias_first)),
    ##        dg_min_bias_third = as.vector(scale(dg_min_bias_third))) %>%
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

## extract dataframes with each measure
for (measure_type in c("att", "dg_first", "dg_third")) {

    ## filter by measure
    df_rq2_measure_type <-
        
        df_rq2 %>%
        filter(measure == measure_type)
    
    ## create object
    assign(paste0("df_rq2_", measure_type), 
           df_rq2_measure_type)
    
}

#####
## Below is an individual level analysis of a single moderator, as an example.

## individualism--collectivism
model_rq2_permeability <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + permeability + (permeability | country)
                , data = df_rq2_att)

## summarise
summary(model_rq2_permeability)

#####
## below are specifications for all models

## 3 models for permeability (hypothesis H2.1)

## family ties
model_rq2_family_ties <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + family_ties + (family_ties | country)
                , data = df_rq2_att)

## individualism--collectivism
model_rq2_permeability <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + permeability + (permeability | country)
                , data = df_rq2_att)

## model relational mobility
model_rq2_relational_mobility <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + relational_mobility + (relational_mobility | country)
                , data = df_rq2_att)

## 2 models for trust (hypothesis H2.2)

## model with trust (strangers)
model_rq2_trust_strangers <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + trust_strangers + (trust_strangers | country)
                , data = df_rq2_att)

## model with trust (institutional)
model_rq2_trust_institution <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + trust_institution + (trust_institution | country)
                , data = df_rq2_att)

## 1 model for self-esteem (hypothesis H2.3)

## model with esteem
model_rq2_esteem <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + self_esteem + (self_esteem | country)
                , data = df_rq2_att)

## 2 models for belief and status (hypothesis H2.4)

## model with belief
model_rq2_belief <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + belief + (belief | country)
                , data = df_rq2_att)

## model with status
model_rq2_status <-
    
    lmerTest::lmer(
                  min_bias ~ 1 + status + (status | country)
                , data = df_rq2_att)

## summarise
summary(model_rq2_permeability)

##################################################
# Research Question 3

## For research question 3, we assess whether real-world bias (towards
## the nation and/or family) is predicted by minimal group bias. Below
## is analysis using one outcome, attitudes.

## measures real-world bias
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

## real-world
model_real_world <- 

    lmerTest::lmer(att_real_bias ~ att_min_bias * group_type + 
                       (1 | id)  + 
                       (att_min_bias + group_type | country),
                   data = df_rq3_att)

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
