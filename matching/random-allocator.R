##################################################
## load packages
library(tidyverse)
library(jsonlite)

## set seed
set.seed(1970) # FIXME to remove, just to get the same result on first run

##################################################

## create tibble to export to json
## json is easily imported by qualtrics, with embedded data

## FIXME evenutally, randomly select from downloaded data

##################################################
## create fake tibble for troubleshooting

## set parameters
n_length <- 1 # 10
focal_nationality <- "American"
list_nationalities <- c("American", "British", "other")
list_min_groups <- c("J", "H", "M", "K", "L", "N", "S", "P")
default_donation <- 10 # represents half the points

## NB if necessary, nationalities could use language codes from qualtrics:
## https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/translate-survey/#AvailableLanguageCodes


## create tibble
random_allocator <-

    tibble(
        minimal = sample(list_min_groups, 1),
        nationality = sample(list_nationalities, 1),
        dg_min_in_self = rep(default_donation, n_length),
        dg_min_out_self = rep(default_donation, n_length),
        dg_min_in_out = rep(default_donation, n_length),
        dg_nat_in_self = rep(default_donation, n_length),
        dg_nat_out_self = rep(default_donation, n_length),
        dg_nat_in_out = rep(default_donation, n_length)    
    )

##################################################
## create tibble using pilot data

## read data
## FIXME upload pilot data if it can be shared
## alternatively, manually place csv file in ./data
df <-
    
    read_csv("../data/MGP-PSA+Mock-up+Survey+for+peer+review_US+version+-+Internal+Pilot_September+21,+2021_09.16.csv") %>%
    ## remove first two lines which is qualtrics bumf
    slice(-1, -2) %>%
    ## allocation decisions
    select(
        ## info on minimal group
        GroupAttitude_1, Q91_1,
        ## dg_min
        Q177_1,	Q177_2,	Q178_1,	Q178_2,	Q179_1,	Q179_2, # for Flurps
        Q180_1,	Q180_2,	Q181_1,	Q181_2,	Q182_1,	Q182_2, # for Zazzes
        ## dg_nat
        Q186_1, Q186_2, Q188_1_1, Q188_2_1, Q189_1, Q189_2) %>% # FIXME duplicate Q188
    ## rename columns
    mutate(flurp_q = GroupAttitude_1,
           zazz_q = Q91_1,
           dg_min_in_self_flurp = as.numeric(Q177_1),
           dg_min_in_self_zazz = as.numeric(Q180_1),
           dg_min_out_self_flurp = as.numeric(Q178_1),
           dg_min_out_self_zazz = as.numeric(Q181_1),
           dg_min_in_out_flurp = as.numeric(Q179_1),
           dg_min_in_out_zazz = as.numeric(Q182_1),
           dg_nat_in_self = as.numeric(Q186_1),
           dg_nat_out_self = as.numeric(Q188_1_1),
           dg_nat_in_out = as.numeric(Q189_1)) %>%
    ## add nationality FIXME for pilot, all US
    mutate(nationality = "American") %>%
    select(nationality, ends_with("_q"), starts_with("dg_"))

## FIXME decided whether identifier (e.g. Response ID) should be included (for traceability)

## create zazz and flurp df
for (min_group in c("flurp", "zazz")) {
    
    assign(paste0("df_", min_group),
           df %>%
           filter(!is.na(get(paste0(min_group, "_q")))) %>%
           set_names(~ str_replace_all(., paste0("_", min_group), "")) %>%
           mutate(minimal = min_group, .before = 1) %>%
           select(minimal, nationality,
                  dg_min_in_self, dg_min_out_self, dg_min_in_out,
                  dg_nat_in_self, dg_nat_out_self, dg_nat_in_out)
           )

}      

## combine zazz and flurp dfs
df <-

    bind_rows(df_flurp, df_zazz) %>%
    ## FIXME in case there are NAs in allocations, replace with midpoint
    replace(is.na(.), 10)

## randomly sample one row
random_allocator <-

    slice_sample(df, n = 1)

##################################################
## convert to json and export

## create json
df_as_json <-

    toJSON(random_allocator,
           pretty = TRUE)

## write lines
writeLines(df_as_json,
           paste0("random-allocator.json") # FIXME per nation?
           )
