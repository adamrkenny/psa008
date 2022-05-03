##################################################
## load packages
##################################################

library(tidyverse)

source("./custom-functions.R") # for functions

set.seed(1970) # to reproduce analysis

##################################################
## simulate data
##################################################

## first generate the basic characteristics: id, lab, country

## arbitrarily set the number of labs to the same as PSACR002
n_countries <- 20
n_labs <- n_countries + 5
## create number of subjects per country
n_ids_per_country <- 100

## create subject ids
ids <- (1:n_ids_per_country) # start with n subjects per country

## create list of lab names
lab_names <-
    
    seq(1:n_labs) %>%
    tibble() %>%
    ## mutate(labs = if_else(. < 10, paste0("lab0", .), paste0("lab", .))) %>%
    pull()

## create list of country names
country_names <-
    
    ## seq(1:n_countries) %>%
    letters[seq( from = 1, to = n_countries )] %>%
    tibble() %>%
    ## mutate(labs = if_else(. < 10, paste0("country0", .), paste0("country", .))) %>%
    pull()
                   
## df with labs randomly matched to a country
## with 15 countries with 2 labs
## FIXME we'll want 5? USA/Italy/China
df_lab_country <-
    
    tibble(
        lab_id = lab_names,
        country = rep_len(country_names, n_labs)
    ) %>%
    arrange(country) %>%
    unite(lab_country, country, lab_id) %>%
    pull()

## create dataframe
fake_data <-
    
    expand.grid(id = ids, lab = df_lab_country) %>%
    tibble() %>%
    separate(col = lab, into = c("country", "lab")) %>%
    select(id, lab, country) %>%
    mutate(id = c(1:nrow(.)))

## count total number of labs and countries
n_labs_countries <-  
    
    nrow(fake_data)/n_ids_per_country

## number of participants
n_total <- nrow(fake_data)


##################################################
## RQ1 variables

## create the minimal group dependent measures, based on the three
## dictator games (in-group--self, out-group--self,
## in-group--out-group) and the average attitude towards in-group and
## towards out-group

## three minimal group measures are: difference between in-group and
## out-group attitudes (att_bias), difference between in-group–self
## and out-group–self decisions in the dictator game (dg_first_bias),
## and the decision in the in-group–out-group dictator game
## (dg_third_bias)

## add outcomes
fake_data <-
    
    fake_data %>%
    mutate(
        dg_min_in_self = 
            round(
                rtruncnorm(n_total, 
                           mean = 3, sd = 3, min = 0, max = 10), 
                  0),
        dg_min_out_self = 
            round(
                rtruncnorm(n_total, 
                           mean = 1, sd = 3, min = 0, max = 10), 
                0),
        dg_min_in_out = 
                round(
                rtruncnorm(n_total, 
                           mean = 4, sd = 3, min = 0, max = 10), 
                0),
        att_min_in = 
                round(
                rtruncnorm(n_total, 
                           mean = 6, sd = 3.5, min = 1, max = 7), 
                1),
        att_min_out = 
                round(
                rtruncnorm(n_total, 
                           mean = 2, sd = 3.5, min = 1, max = 7), 
                1)
    ) %>%
    mutate(dg_min_bias_first = dg_min_in_self - dg_min_out_self,
           dg_min_bias_third = dg_min_in_out,
           att_min_bias = att_min_in - att_min_out)


##################################################
## RQ2 variables

## generate permeability
permeability_score <- 
    
    rnorm(n_ids_per_country 
        , mean = -0.002, sd = 1.003 # from Schulz data, column KII
          ) 

## generate trust
trust_score <- 
    
    rnorm(n_ids_per_country 
        , mean = -0.003, sd = 1.006 # from Schulz data, column wvs_OutInTrust_std
            ) 
## generate self-esteem
self_esteem_score <- 
    
    rnorm(n_ids_per_country 
        , mean = 3.5, sd = 1.1 # from Robins-et-al_2001
          ) 

## add to artificial data frame
## each repeated n times for each lab/country
fake_data <-

    fake_data %>%
    mutate(trust = rep(trust_score, n_labs_countries),
           self_esteem = rep(self_esteem_score, n_labs_countries),
           permeability = rep(permeability_score, n_labs_countries)
           ) %>%
    ## scale and remove [,1]: https://stackoverflow.com/a/39671848
    mutate(
        self_esteem = as.vector(
             scale(self_esteem)),
        trust = as.vector(
             scale(trust)),
        permeability = as.vector(
             scale(permeability))
    )

##################################################
## RQ3 variables

## add real-world measures
fake_data <-
    
    fake_data %>%
    ## nation
    mutate(
        dg_nat_in_self = 
            round(
                rtruncnorm(n_total, 
                           mean = 3, sd = 3, min = 0, max = 10), 
                  0),
        dg_nat_out_self = 
            round(
                rtruncnorm(n_total, 
                           mean = 1, sd = 3, min = 0, max = 10), 
                0),
        dg_nat_in_out = 
                round(
                rtruncnorm(n_total, 
                           mean = 4, sd = 3, min = 0, max = 10), 
                0),
        att_nat_in = 
                round(
                rtruncnorm(n_total, 
                           mean = 5, sd = 3.5, min = 1, max = 7), 
                0),
        att_nat_out = 
                round(
                rtruncnorm(n_total, 
                           mean = 3, sd = 3.5, min = 1, max = 7), 
                0)
    ) %>%
    mutate(dg_nat_bias_first = dg_nat_in_self - dg_nat_out_self,
           dg_nat_bias_third = dg_nat_in_out,
           att_nat_bias = att_nat_in - att_min_out) %>%
    ## family        
    mutate(
        dg_fam_in_self = 
            round(
                rtruncnorm(n_total, 
                           mean = 3, sd = 3, min = 0, max = 10), 
                0),
        dg_fam_out_self = 
            round(
                rtruncnorm(n_total, 
                           mean = 1, sd = 3, min = 0, max = 10), 
                0),
        dg_fam_in_out = 
                round(
                rtruncnorm(n_total, 
                           mean = 4, sd = 3, min = 0, max = 10), 
                0),
        att_fam_in = 
                round(
                rtruncnorm(n_total, 
                           mean = 6, sd = 1, min = 1, max = 7), 
                0),
        att_fam_out = 
                round(
                rtruncnorm(n_total, 
                           mean = 4, sd = 3.5, min = 1, max = 7), 
                0)
    ) %>%
    mutate(dg_fam_bias_first = dg_fam_in_self - dg_fam_out_self,
           dg_fam_bias_third = dg_fam_in_out,
           att_fam_bias = att_fam_in - att_min_out)

##################################################
## demographic variables and additional measures

## age categories
age_cats <-

    c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75 or above") 

## political orientation categories
pol_cats <-

    c("Very liberal", "Somewhat liberal", "A little liberal", "Moderate",
      "A little conservative", "Conservative", "Very conservative") 

## income categories
inc_cats <-

    c("Far below average", "Below average", "Average", "Above average", "Far above average") 


## add to data
fake_data <-

    fake_data %>%
    mutate(
        age = as.factor(sample(age_cats, nrow(fake_data), replace = TRUE)),
        gender = as.factor(sample(c("male", "female", "other"),
                                  nrow(fake_data), replace = TRUE, prob = c(0.49, 0.49, 0.02))),
        political = as.factor(sample(pol_cats, nrow(fake_data), replace = TRUE)),
        income = as.factor(sample(inc_cats, nrow(fake_data), replace = TRUE))
        )

##################################################
## clean up df

## remove not needed variables and reorder
fake_data <-

    fake_data %>%
    select(-contains("_self"), -contains("_in"), -contains("_out"))

##################################################
## wrangle data
##################################################

## create dataframes for specific RQs
## most of these are "long" versions of the above

##################################################
## RQ1

## long dataframe with only relevant vars
df_rq1 <-

    fake_data %>%
    select(id, lab, country,
           att_min_bias, dg_min_bias_first, dg_min_bias_third) %>%
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

##################################################
## RQ2

## long dataframe with only relevant vars
df_rq2 <-

    fake_data %>%
    select(id, lab, country, 
          att_min_bias, dg_min_bias_first, dg_min_bias_third,
          self_esteem, trust, permeability) %>%
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

##################################################
## RQ3

## longish dataframe with relevant vars
## NB for only one measure
df_rq3 <-
    
    fake_data %>%
    select(id, lab, country,
           att_min_bias, att_nat_bias, att_fam_bias,
           dg_min_bias_first, dg_nat_bias_first, dg_fam_bias_first) %>%
    pivot_longer(cols = c(att_nat_bias, att_fam_bias),, 
                 names_to = "group_type",
                 values_to = "att_real_bias") %>%
    mutate(group_type = case_when(str_detect(group_type, "nat") ~ "nat",
                                  str_detect(group_type, "fam") ~ "fam"
                                  ))
