## packages
library(tidyverse)
library(simr)
library(lmerTest)
library(furrr)

## simulation parameters
## n_sim <- 2 # for troubleshooting 
n_sim <- 1000 # for reliable results 

plan(multisession) # to speed up simulations

set.seed(1971)

##################################################
## RQ1

simulation_swarm_rq1 <- function(N, K, beta, beta_variance) {

    ## set variables for sample
    country <- rep(1:K, each = 2*N) 
    subject <- rep(1:(N*K), each = 2)
    y <- rnorm(N*K*2, mean = 0, sd = 1) # var for outcome
    group <- rep(c(0, 1), times = N*K) # var for in/outgroup

    ## bind variables into dataframe
    df_rq1 <-
        data.frame(
            country = factor(country),
            subject = factor(subject),
            y = y,
            group = factor(group)
        )

    contrasts(df_rq1$group) <- contr.sum(2) 

    model_rq1 <-
        simr::makeLmer(
                  y ~ group + (1 | subject) + (1 + group | country),
                  fixef = c(0, beta),
                  VarCorr = list(subject = 1,
                                 country = matrix(c(1, 0, 0, beta_variance), ncol = 2)),
                  sigma = 1, 
                  data = df_rq1                 
              )


    ## check power
    sim_rq1 <- 
      
        powerSim(
            model_rq1,
            test = fixed("group"),
            nsim = n_sim,
            alpha = 0.05/3
        )

doTest(model_rq1)

    ## create table with powerSim output and info on variables
    summary(sim_rq1) %>%
    mutate(subjects = N,
           countries = K,
           beta = beta,
           beta_variance = beta_variance,
           class = "rq1"
           )

}

## create tibble with all combinations of parameters
parameter_space_rq1 <-

    crossing(
        N = c(100, 150, 200, 250), # number of subjects per country
        K = c(30, 40, 50), # number of countries
        beta = c(0.10, 0.05, 0.15), # beta for group 
        beta_variance = c(0.01, 0.02, 0.03) # beta for random slope
    )

## run power analysis across all parameters
pwr_rq1 <-
     
    future_pmap_dfr(parameter_space_rq1,
                    simulation_swarm_rq1,
                    .options = furrr_options(seed = TRUE),
                    .progress = TRUE
                    )

## write output
pwr_rq1 %>%
    write_csv(file = paste0("./pwr-rq1.csv"))

##################################################
## RQ2

simulation_swarm_rq2 <- function(N, K, beta, beta_variance) {

    ## set variables for sample
    country <- rep(1:K, each = 2*N) 
    subject <- rep(1:(N*K), each = 2)
    MGE <- rnorm(N*K*2, mean = 0, sd = 1) # var for outcome (MGE) 
    selfesteem <- rnorm(N*K*2, mean = 3.5, sd = 1) # var for predictor (e.g. self-esteem)

    ## bind variables into dataframe
    df_rq2 <-
        data.frame(
            country = factor(country),
            subject = factor(subject),
            MGE = MGE,
            predictor = selfesteem
            ## predictor = scale(selfesteem, scale = F)
        )

    model_rq2 <-
        simr::makeLmer(
                  MGE ~ 1 + predictor +
                      (1 + predictor | country),
                  fixef = c(0, beta),
                  VarCorr = matrix(c(1.5, # var of country random intercept
                                     0, 0, beta_variance),
                                   ncol = 2),
                  sigma = 1,
                  data = df_rq2
              )
    model_rq2

    ## check power
    sim_rq2 <- 
      
        powerSim(
            model_rq2,
            test = fixed("predictor", "kr"), # "kr"/"pb" appropriate for continuous
            nsim = n_sim,
            alpha = 0.05/3
        )

    ## create table with powerSim output and info on variables
    summary(sim_rq2) %>%
    mutate(subjects = N,
           countries = K,
           beta = beta,
           beta_variance = beta_variance,
           class = "rq2"
           )

}

## create tibble with all combinations of parameters
parameter_space_rq2 <-

    crossing(
        N = c(100, 150, 200, 250), # number of subjects per country
        K = c(30, 40, 50), # number of countries
        beta = c(0.10, 0.05, 0.15), # beta for "predictor" 
        beta_variance = c(0.01, 0.02, 0.03) # beta for random slope
    )

## run power analysis across all parameters
pwr_rq2 <-
     
    future_pmap_dfr(parameter_space_rq2,
                    simulation_swarm_rq2,
                    .options = furrr_options(seed = TRUE),
                    .progress = TRUE
                    )

## write output
pwr_rq2 %>%
    write_csv(file = paste0("./pwr-rq2.csv"))

##################################################
## RQ3

simulation_swarm_rq3 <- function(N, K, beta_MGE, beta_variance) {

    ## set variables for sample
    country <- rep(1:K, each = 2*N) 
    subject <- rep(1:(N*K), each = 2)
    realbias <- rnorm(N*K*2, mean = 0, sd = 1) # variable for outcome
    MGE <- rep(rnorm(N*K, mean = 0, sd = 1), each = 2) # variable for minimal
    grouptype <- rep(c(0, 1), times = N*K) # variable for national/family

    ## bind variables into dataframe
    df_rq3 <-
        data.frame(
            country = factor(country),
            subject = factor(subject),
            realbias = realbias,
            MGE = MGE,
            grouptype = factor(grouptype)
        )

    contrasts(df_rq3$grouptype) <- contr.sum(2)

    model_rq3 <-
        simr::makeLmer(
                  realbias ~ MGE * grouptype + (1 | subject) +
                      (1 + MGE + grouptype | country), 
                  fixef = c(0, # beta for realbias
                            beta_MGE, # beta for MGE (marginal)
                            0.075, # beta for grouptype
                            0.02), # beta for interaction      
                  VarCorr = 
                      list(people = 1,
                           country = matrix(c(1.5, # var of country random intercept
                                              rep(0, 6),
                                              beta_variance,
                                              0.01),
                                            ncol = 3)), 
                  sigma = 1, 
                  data = df_rq3                 
              )
    
    summary(model_rq3)

    ## you can check interaction plot:
    ## sjPlot::plot_model(model_rq3, type = "int")

    ## check power
    sim_rq3 <- 
      
        powerSim(
            model_rq3,
            test = fixed("MGE", "z"), 
            ## test = fcompare(~grouptype), # alternatively, power for MGE by comparing model without it
            nsim = n_sim,
            alpha = 0.05/3,
            fitOpts = list(control = lmerControl(optCtrl = list(maxfun = 2e5), optimizer = "bobyqa"))
            # solution for convergence issues https://github.com/pitakakariki/simr/issues/169
        )


    ## create table with powerSim output and info on variables
    summary(sim_rq3) %>%
    mutate(subjects = N,
           countries = K,
           beta_MGE = beta_MGE,
           beta_variance = beta_variance,
           class = "rq3"
           )
    
}

## create tibble with all combinations of parameters
parameter_space_rq3 <-

    crossing(
        N = c(100, 150, 200, 250), # number of subjects per country
        K = c(30, 40, 50), # number of countries
        beta_interaction = c(0.05, 0.025, 0.10), # beta for marginal effect of MGE 
        beta_variance = c(0.01, 0.02, 0.03) # beta for random slope
    )

## run power analysis across all parameters
pwr_rq3 <-
     
    future_pmap_dfr(parameter_space_rq3,
                    simulation_swarm_rq3,
                    .options = furrr_options(seed = TRUE),
                    .progress = TRUE
                    )

## print output
print(pwr_rq3)

## write output
pwr_rq3 %>%
    write_csv(file = paste0("./pwr-rq3.csv"))
