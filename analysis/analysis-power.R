## ---- package-information, message = FALSE, class.source = 'fold-hide'--------
## R package information

## load-packages
## list of packages required
packages <- c(
  "tidyverse", # data wrangling
  "lme4", # random effects models
  "lmerTest", # random effects models
  "simr", # simulating data
  "pwr", # power test
  "patchwork" # combine plots
)
## create list of packages that have not been installed
new_packages <-
    packages[!(packages %in% installed.packages()[,"Package"])]
## install packages that have not been installed
if (length(new_packages)) 
    install.packages(new_packages, dependencies = TRUE)
## load all packages
sapply(packages, library, character.only = TRUE)

source("./custom-functions.R") # load custom functions

set.seed(1970) # to reproduce analysis

sessionInfo() # session info


## ---- simulate-data-----------------------------------------------------------
## first generate the basic characteristics: id, country

## set the number of labs

## start with half value of number of interested collaborators at
## first submission which is n = 59
list_n_countries <- c(seq(from = 20, to = 60, by = 10))

##  vary number of participants per country
list_n_ids_per_country <- c(seq(from = 100, to = 400, by = 50))

## iteratively create datasets that vary in the number of countris
for (n_countries in list_n_countries) {

    ## and that vary in number of participants per country
    for (n_ids in list_n_ids_per_country) {
        
        ## create subject ids
        ids <- (1:n_ids) # start with n subjects per country
        
        ## create list of country names
        country_names <-
            
            letters_beyond_single_digits(n_countries) %>%
            tibble() %>%
            pull()
        
        ## create dataframe
        fake_data <-
            
            expand.grid(id = ids, country = country_names) %>%
            tibble() %>%
            mutate(id = c(1:nrow(.)))
        
        ## number of participants
        n_total <- nrow(fake_data)
        
        ## RQ1 variables
        
        ## create one of the minimal group dependent measures, based on the
        ## average attitude towards in-group and towards out-group
        
        ## mean and sd values are taken from pilot 02
        
        ## add outcomes for RQ1 and RQ2
        fake_data <-
            
            fake_data %>%
            mutate(min_in = 
                       round(
                           rtruncnorm(n_total, 
                                      mean = 6.74, sd = 4.25, min = 0, max = 20), 
                           0),
                   min_out = 
                       round(
                           rtruncnorm(n_total, 
                                      mean = 5.83, sd = 4.49, min = 0, max = 20), 
                           0)
                   ) %>%
            mutate(min_bias = min_in - min_out)
        
        ## RQ2 variables
        ## generate one of the measures, we use self-esteem as we have a good
        ## cross-country estimate using a similar scale
        
        ## generate self-esteem
        self_esteem_score <- 
            
            rnorm(n_ids
                , mean = 3.5, sd = 1.1 # from Robins-et-al_2001
                  ) 
        
        ## add to artificial data frame
        ## each repeated n times for each country
        fake_data <-
            
            fake_data %>%
            mutate(self_esteem = rep(self_esteem_score, n_countries)
                   )
        ## RQ3 variables

        ## mean and sd values are taken from pilot 02
        
        ## add real-world measures
        fake_data <-
            
            fake_data %>%
            ## nation
            mutate(
                nat_in = 
                    round(
                        rtruncnorm(n_total, 
                                   mean = 6.29, sd = 4.26, min = 0, max = 20), 
                        0),
                nat_out = 
                    round(
                        rtruncnorm(n_total, 
                                   mean = 6.26, sd = 4.59, min = 0, max = 20), 
                        0)) %>%
            mutate(nat_bias = nat_in - nat_out) %>%
            ## family        
            mutate(
                fam_in = 
                    round(
                        rtruncnorm(n_total, 
                                   mean = 3, sd = 3, min = 0, max = 10), 
                        0),
                fam_out = 
                    round(
                        rtruncnorm(n_total, 
                                   mean = 1, sd = 3, min = 0, max = 10), 
                        0)) %>%
            mutate(fam_bias = fam_in - fam_out)
        
        ## assign to object for later use
        assign(
            ## name of object
            paste0("fake_data_n", n_ids, "_c", n_countries),
            ## object
            fake_data
        )
        
    }

}

## gather all fake datasets 
gather_dfs <- grep("fake_data_n", names(.GlobalEnv), value = TRUE)
list_dfs <- do.call("list", mget(gather_dfs))


## ---- n-simulations-----------------------------------------------------------
n_sim <- 1 # to troubleshoot
## n_sim <- 1000 # actual run


## ---- outcomes----------------------------------------------------------------
## set effect sizes
es_outcome_larger <- 0.35
es_outcome_smaller <- 0.16
min_bias_es <- es_outcome_smaller


## ---- rq1-1000-codebook, echo = FALSE, message = FALSE, results = "asis"------
read_csv("pwr-rqs-codebook.csv") %>%
  knitr::kable(., caption = "Codebook for power analysis output tables")


## ---- rq1-specs---------------------------------------------------------------
## sigma in models
residual_sd <- 1

## group beta
beta <- 0.3
## based on in-group vs out-group/stranger of beta 0.29 in Romano et
## al (2021) https://doi.org/10.1038/s41467-021-24787-1

## we set the lowest value for the variance to 0.05

## for sensitivity analysis
## create list of random intercepts
list_random_intercepts <- c(0.05, 0.30, 0.55, 0.80, 1.5)


## ---- rq1-iter, eval = FALSE--------------------------------------------------
## iterate over different random intercepts
for (random_intercept in list_random_intercepts) {

## iterate over different number of ids per country
  for (df in list_dfs) {

    ## dataframe
    ## extract dataframe that has "n" participants across "c" countries
    ## make long-form dataframe
    df <-

      df %>%
      pivot_longer(cols = c("min_in", "min_out"),
                   names_to = "group",
                   values_to = "decision") %>%
      mutate(group = if_else(group == "min_in", "in", "out")) %>%
      select(id, country, group, decision)

    ## fixed intercept
    fixed <- c(
      min_bias_es # outcome
      , 0.4 # moderator
    )

    ## random intercepts
    random <- list(
      random_intercept
    )

    ## construct model
    model_rq1 <-

      makeLmer(
        decision ~ group + (1 | country),
        fixef = fixed,
        VarCorr = random,
        sigma = residual_sd,
        data = df
      )

    ## check power
    sim_rq1 <-

      powerSim(
        model_rq1,
        test = fixed("group"),
        nsim = n_sim,
        alpha = 0.05/3
      )

    ## extract number of countries and participants per country
    n_country <- pull(count(unique(select(df, country))))
    n_id <- pull(count(unique(select(df, id))))/n_country

    ## assign powersim to object for later use
    summary_sim_rq1 <-

      summary(sim_rq1) %>%
      mutate(bias_es = min_bias_es,
             random_var = random_intercept,
             n_ids = n_id,
             n_countries = n_country,
             beta = beta,
             class = "rq1"
      )

    ## assign powercurve to object for later use
    assign(
      ## name of object
      paste0("pwr_rq1_es", min_bias_es,
             "_ri", random_intercept,
             "_n", n_id,
             "_c", n_country),
      ## summary
      summary_sim_rq1
    )

  }
}

## gather all power simulations
gather_pwr_sim <- grep("pwr_rq1", names(.GlobalEnv), value = TRUE)
list_pwr_sim <- do.call("list", mget(gather_pwr_sim))

bind_rows(list_pwr_sim) %>%
    arrange(bias_es, random_var, n_ids, n_countries) %>%
mutate(powered = if_else(mean > 0.95, "yes", "no")) %>%
    select(n_ids, n_countries, bias_es,	beta, random_var,	trials, successes, mean,	lower, upper,	powered, class) %>%
    write_csv(path = "./pwr-rq1-sims-combined.csv")


## ---- rq1-1000-results, echo = FALSE, message = FALSE, results = "asis"-------
read_csv("pwr-rq1-sims-combined.csv") %>%
  arrange(n_ids) %>%
  mutate(powered = if_else(mean > 0.95, "yes", "no")) %>%
  select(n_ids, n_countries, bias_es,	random,	trials, successes, mean, lower, upper,	powered) %>%
  collapse_rows_df(n_ids) %>%
  knitr::kable(., caption = "RQ1 power analysis (n simulations = 1,000)")


## ---- rq2-iter, eval = FALSE--------------------------------------------------
## sigma in models
residual_sd <- 1

## moderator beta
beta <- 0.4
## based on beta 0.419 from https://app.cooperationdatabank.org/

## this selected State Trust as independent variable, which as as of
## 2022-11-22, was composed of 11 effects and a total of 2036 participants

## we set the lowest value for the variance to 0.05

## for sensitivity analysis
## create list of random intercepts
list_random_intercepts <- c(0.05, 0.30, 0.55, 0.80, 1.5)

## iterate over different random intercepts
for (random_intercept in list_random_intercepts) {

  ## iterate over different number of ids per country
  for (df in list_dfs) {

    ## fixed intercept
    fixed <- c(
      min_bias_es # outcome
      , 0.4 # moderator
     )

    ## random intercepts
    random <- list(
      random_intercept
    )

    ## construct model
    model_rq2 <-

      makeLmer(
        min_bias ~ self_esteem + (1 | country),
        fixef = fixed,
        VarCorr = random,
        sigma = residual_sd,
        data = df
      )

    ## check power
    sim_rq2 <-

      powerSim(
        model_rq2,
        test = fixed("self_esteem"),
        nsim = n_sim,
        alpha = 0.05/3
      )

    ## extract number of countries and participants per country
    n_country <- pull(count(unique(select(df, country))))
    n_id <- pull(count(unique(select(df, id))))/n_country

    ## assign powersim to object for later use
    summary_sim_rq2 <-

      summary(sim_rq2) %>%
      mutate(bias_es = min_bias_es,
             random = random_intercept,
             n_ids = n_id,
             n_countries = n_country,
             beta = beta,
             class = "rq2"
      )

    ## assign powercurve to object for later use
    assign(
      ## name of object
      paste0("pwr_rq2_es", min_bias_es,
             "_ri", random_intercept,
             "_n", n_id,
             "_c", n_country),
      ## summary
      summary_sim_rq2
    )

  }

}

## gather all power simulations
gather_pwr_sim <- grep("pwr_rq2", names(.GlobalEnv), value = TRUE)
list_pwr_sim <- do.call("list", mget(gather_pwr_sim))

bind_rows(list_pwr_sim) %>%
  arrange(bias_es, random_var, n_ids, n_countries) %>%
mutate(powered = if_else(mean > 0.95, "yes", "no")) %>%
    select(n_ids, n_countries, bias_es,	beta, random_var,	trials, successes, mean,	lower, upper,	powered, class) %>%
    write_csv(path = "./pwr-rq2-sims-combined.csv")


## ---- rq2-1000-results, echo = FALSE, message = FALSE, results = "asis"-------
read_csv("pwr-rq2-sims-combined.csv") %>%
  arrange(n_ids) %>%
  mutate(powered = if_else(mean > 0.95, "yes", "no")) %>%
  select(n_ids, n_countries, bias_es, random,	trials, successes, mean,	lower, upper,	powered) %>%
  collapse_rows_df(n_ids) %>%
  knitr::kable(., caption = "RQ2 power analysis (n simulations = 1,000)")


## ---- rq3-iter, eval = FALSE--------------------------------------------------
## adjust dataframe to contain relevant predictors 
## (MGE, group type)
df <-
  
  df %>%
  pivot_longer(cols = c("nat_bias", "fam_bias"),
               names_to = "group_type", values_to = "real_bias") %>%
  rename(MGE = min_bias) %>%
  separate(group_type, into = c("group_type", "bias")) %>%
  select(id, country, MGE, group_type, real_bias)

## sigma in models
residual_sd <- 1

## effect size
real_bias_es <- es_outcome_smaller

## moderators beta
beta_mge <- 0.3
beta_group_type <- 0.3 # same as for rq1
beta_interaction <- 1.0

## we set the lowest value for the variance to 0.05 in our power
## analysis

## for sensitivity analysis
## create list of random intercepts
list_random_intercepts <- c(0.05, 0.30, 0.55, 0.80, 1.5)

## iterate over different random intercepts
for (random_intercept in list_random_intercepts) {

  ## iterate over different number of ids per country
  for (df in list_dfs) {

    ## fixed intercept
    fixed <- c(
      real_bias_es # outcome
      , beta_mge # MGE
      , beta_group_type # group type
      , beta_interaction # interaction
    )

    ## random intercepts
    random <- list(
      random_intercept # for country
    )

    ## construct model
    model_rq3 <-

      makeLmer(
        real_bias ~ MGE * group_type + (1 | country),
        # we use simplier random structure to make code easier to run
        fixef = fixed,
        VarCorr = random,
        sigma = residual_sd,
        data = df
      )

    ## check power
    sim_rq3 <-

      powerSim(
        model_rq3,
        test =  fcompare(~ MGE + group_type),
        nsim = n_sim,
        alpha = 0.05/3
      )

    ## extract number of countries and participants per country
    n_country <- pull(count(unique(select(df, country))))
    n_id <- pull(count(unique(select(df, id))))/n_country

    ## assign powersim to object for later use
    summary_sim_rq3 <-

      summary(sim_rq3) %>%
      mutate(bias_es = real_bias_es,
             random = random_intercept,
             n_ids = n_id,
             n_countries = n_country,
             class = "rq3"
      )

    ## assign powercurve to object for later use
    assign(
      ## name of object
      paste0("pwr_rq3_es", real_bias_es,
             "_ri", random_intercept,
             "_n", n_id,
             "_c", n_country),
      ## summary
      summary_sim_rq3
    )

  }

}

## gather all power simulations
gather_pwr_sim <- grep("pwr_rq3", names(.GlobalEnv), value = TRUE)
list_pwr_sim <- do.call("list", mget(gather_pwr_sim))

bind_rows(list_pwr_sim) %>%
  arrange(bias_es, random_var, n_ids, n_countries) %>%
mutate(powered = if_else(mean > 0.95, "yes", "no")) %>%
    select(n_ids, n_countries, bias_es, random_var,	trials, successes, mean,	lower, upper,	powered, class) %>%
    write_csv(path = "./pwr-rq3-sims-combined.csv")


## ---- rq3-1000-results, echo = FALSE, message = FALSE,  results = "asis"------
read_csv("pwr-rq3-sims-combined.csv") %>%
  arrange(n_ids) %>%
  mutate(powered = if_else(mean > 0.95, "yes", "no")) %>%
  select(n_ids, n_countries, bias_es,	random,	trials, successes, mean,	lower, upper,	powered) %>%
  collapse_rows_df(n_ids) %>%
  knitr::kable(., caption = "RQ3 power analysis (n simulations = 1,000)")


## ---- rq1-meta-analysis-------------------------------------------------------
## overall effect size
effect_size <- es_outcome_smaller

## number of countries
## lower limit: 20 is a reasonable minimal number based on previous PSA projects
## upper limit: 60, as 59 countries expressed an interest at first submission
range_countries <- c(seq(20, 60, 10))

## number of participants per treatment
## corresponding to half total number of participants
range_group <- c(25, 50, 75, 100, 125, 150, 175, 200)

## calculate critical z
c_z <- qnorm(p = (0.05/3)/2, # adjusted alpha, halved for two-tailed
             lower.tail = FALSE)

## create tibbles with estimated power

## across range of heterogeneity = tau squared
for (heterogeneity in c(.33, 1, 3)) {
    
    ## across range of countries
    for (n_k in range_countries) {
        
        ## across range of number of participants per treatment
        for (n_per_group in range_group) {
            
            eq1 <- 
                ((n_per_group + n_per_group)/((n_per_group) * (n_per_group))) + 
                ((effect_size^2)/(2*(n_per_group + n_per_group)))
            
            eq2 <- heterogeneity*(eq1)
        
            eq3 <- eq2 + eq1
            
            eq4 <- eq3/n_k
            
            eq5 <- (effect_size/sqrt(eq4))
        
            power <- (1 - pnorm(c_z - eq5)) # two-tailed
            
            power

            ## create tibbles
            assign(
                paste0("tbl_", n_per_group, "_", n_k, "_", heterogeneity),
                tibble(n = n_per_group, k = n_k, pwr = power, hg = heterogeneity))
            
        }
    }
    
}

## gather all tibbles 
gather_tbls <- grep("tbl", names(.GlobalEnv), value=TRUE)
tbls_list <- do.call("list", mget(gather_tbls))

## create plot
plot_power <-

    bind_rows(tbls_list) %>%
    mutate(n = n*2) %>% # double to get total sample size
    mutate(k = as.factor(k)) %>%
    ggplot() +
    aes(x = n, y = pwr, group = k, colour = k) +
    geom_point(size = 3) +
    geom_line(alpha = 0.5, size = 1.5) +
    geom_hline(yintercept = 0.95, linetype = 2) +
    facet_wrap(. ~ hg,
               labeller = labeller(hg = as_labeller(c("0.33" = "0.33 (low heterogeneity)",
                                                      "1" = "1.00 (mid heterogeneity)",
                                                      "3" = "3.00 (high heterogeneity)"
                                                      )))) +
    scale_colour_brewer(palette = "Dark2",
                        name = "number of\ncountries",
                        na.translate = FALSE) +
    labs(x = "sample size (total n)",
         y = "power (Beta)") +
    guides(colour = guide_legend(reverse = TRUE)) +
    theme_bw(
      ## base_size = 33 ## for legibility of printed plot, comment out size
      ) 

## visualise plot
plot_power

 ## save plot # comment out to save in folder 
 plot_power %>%
     ggsave(filename = paste(Sys.Date(), "power-rq1.png", sep = "_"),  
            bg = "transparent")

