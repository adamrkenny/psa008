##################################################
## load packages
library(tidyverse)

## convert attitudinal responses into numeric values
## dependencies: tidyverse
numeric_likert_attitude <- function(x) {

    case_when(
        x %in% c("Strongly Agree") ~ 7,
        x %in% c("Agree") ~ 6,
        x %in% c("Somewhat Agree") ~ 5,
        x %in% c("Neither Agree nor Disagree") ~ 4,
        x %in% c("Somewhat Disagree") ~ 3,
        x %in% c("Disagree") ~ 2,
        x %in% c("Strongly Disagree") ~ 1
    )
    
}

## convert trust responses into numeric values
## dependencies: tidyverse
numeric_likert_trust <- function(x) {

    case_when(
        x %in% c("Completely") ~ 4,
        x %in% c("Somewhat") ~ 3,
        x %in% c("Not very much") ~ 2,
        x %in% c("Not at all") ~ 1
    )
    
}

## ##################################################
## ## create tibble using final pilot data

## ## code to input data downloaded from qualtrics and remove certain rows/cols

## ## this will not run in repo as data downloaded from qualtrics is not shared
## ## (as it contains identifying information like IP address)
## ## commented out code has been kept for transparency

## ## the final downloaded data used by ARK was 2022-04-05 11:56
## ## remove first rows, data from preview tests, and columns with identifying information

## df_raw <-
    
##     ## FIXME include personal filepath to downloaded data if different
##     read_csv("../data/PSA-MGP Prolific pilot (US + UK)_April 5, 2022_11.56.csv") %>% 
##     ## remove first lines which are qualtrics bumf
##     slice(-(1:3)) %>%
##     ## create start date to filter from 2022-04-04 (i.e. remove preview tests)
##     separate(StartDate, c("start_date", "start_time"), sep = " ") %>%
##     mutate(start_date = lubridate::ymd(start_date)) %>%
##     filter(start_date > "2022-04-03") %>%
##     ## create end date
##     separate(EndDate, c("end_date", "end_time"), sep = " ") %>%
##     rename(duration = `Duration (in seconds)`) %>%
##     ## remove embedded data from qualtrics such as IP address
##     select(-5, -6, -(9:18), -20) %>%
##     ## add alphannumeric id code
##     mutate(id =
##                case_when(row_number() < 10 ~ paste0("p00", row_number()),
##                          row_number() < 100 ~ paste0("p0", row_number()),
##                          row_number() < 1000 ~ paste0("p", row_number())),
##            .before = 1
##            ) %>%
##     ## remove embedded data from qualtrics
##     ## including prolific id and click data that correspond to countdown timer
##     select(-starts_with("0."), -PROLIFIC_PID, -contains("Click"), -contains("Submit"))

## ## save as csv
## df_raw %>% write_csv("../data/data-pilot-raw.csv")

##################################################
## create tibble using pilot data and save

## FIXME use either above processing or
## include personal filepath to pilot data
## by commenting out relevant line

## read raw data
df_raw <-

    ## FIXME use either above tibble or input raw data
    ## df_raw %>% 
    read_csv("../data/data-pilot-raw.csv")

## code to wrangle raw data
df_processed <-

    df_raw %>%
    ## allocation decisions
    mutate(
        ## merge minimal decisions
        Allocate_IG_M = if_else(is.na(Allocate_IG_M_1), Allocate_IG_P_1, Allocate_IG_M_1),
        Allocate_OG_M = if_else(is.na(Allocate_OG_M_1), Allocate_OG_P_1, Allocate_OG_M_1),
        Allocate_TG_M = if_else(is.na(Allocate_TG_M_1), Allocate_TG_P_1, Allocate_TG_M_1)) %>%
    ## rename columns
    mutate(minimal_group = ingroupletter,
           ## FIXME double check that above manipulation tracks
           dg_min_in_self = as.numeric(Allocate_IG_M),
           dg_min_out_self = as.numeric(Allocate_OG_M),
           dg_min_in_out = as.numeric(Allocate_TG_M),
           dg_nat_in_self = as.numeric(Allocate_IG_Nat_1),
           dg_nat_out_self = as.numeric(Allocate_OG_Nat_1),
           dg_nat_in_out = as.numeric(Allocate_TG_Nat_1),
           dg_fam_in_self = as.numeric(Allocate_IG_Fam_1),
           dg_fam_out_self = as.numeric(Allocate_OG_Fam_1),
           dg_fam_in_out = as.numeric(Allocate_TG_Fam_1)
           ) %>%
        ## attitude decisions
    mutate(across(starts_with("Attitude"), numeric_likert_attitude)) %>%
    mutate(
        ## merge minimal decisions
        Attitude_IG_M_1 = if_else(is.na(Attitude_IG_M_1), Attitude_IG_P_1, Attitude_IG_M_1),
        Attitude_IG_M_2 = if_else(is.na(Attitude_IG_M_2), Attitude_IG_P_2, Attitude_IG_M_2),
        Attitude_IG_M_3 = if_else(is.na(Attitude_IG_M_3), Attitude_IG_P_3, Attitude_IG_M_3),
        Attitude_IG_M_4 = if_else(is.na(Attitude_IG_M_4), Attitude_IG_P_4, Attitude_IG_M_4),
        Attitude_OG_M_1 = if_else(is.na(Attitude_OG_M_1), Attitude_OG_P_1, Attitude_OG_M_1),
        Attitude_OG_M_2 = if_else(is.na(Attitude_OG_M_2), Attitude_OG_P_2, Attitude_OG_M_2),
        Attitude_OG_M_3 = if_else(is.na(Attitude_OG_M_3), Attitude_OG_P_3, Attitude_OG_M_3),
        Attitude_OG_M_4 = if_else(is.na(Attitude_OG_M_4), Attitude_OG_P_4, Attitude_OG_M_4)
    ) %>%
    ## calculate bias
    mutate(dg_min_bias_first = (20 - dg_min_in_self) - (20 - dg_min_out_self),
           dg_min_bias_third = dg_min_in_out - (20 - dg_min_in_out),
           dg_nat_bias_first = (20 - dg_nat_in_self) - (20 - dg_nat_out_self),
           dg_nat_bias_third = dg_nat_in_out - (20 - dg_nat_in_out),
           dg_fam_bias_first = (20 - dg_fam_in_self) - (20 - dg_fam_out_self),
           dg_fam_bias_third = dg_fam_in_out - (20 - dg_fam_in_out)
           ) %>%
    mutate(att_min_in =
               (Attitude_IG_M_1 + Attitude_IG_M_2 + Attitude_IG_M_3 + Attitude_IG_M_4)/4,
           att_min_out =
               (Attitude_OG_M_1 + Attitude_OG_M_2 + Attitude_OG_M_3 + Attitude_OG_M_4)/4,
           att_min_bias = att_min_in - att_min_out
           ) %>%
    mutate(att_nat_in =
               (Attitude_IG_Nat_1 + Attitude_IG_Nat_2 + Attitude_IG_Nat_3 + Attitude_IG_Nat_4)/4,
           att_nat_out =
               (Attitude_OG_Nat_1 + Attitude_OG_Nat_2 + Attitude_OG_Nat_3 + Attitude_OG_Nat_4)/4,
           att_nat_bias = att_nat_in - att_nat_out
           ) %>%
    mutate(att_fam_in =
               (Attitude_IG_Fam_1 + Attitude_IG_Fam_2 + Attitude_IG_Fam_3 + Attitude_IG_Fam_4)/4,
           att_fam_out =
               (Attitude_OG_Fam_1 + Attitude_OG_Fam_2 + Attitude_OG_Fam_3 + Attitude_OG_Fam_4)/4,
           att_fam_bias = att_fam_in - att_fam_out
           )%>%
    ## lowercase all columns
    rename_with(str_to_lower) %>%
    ## remove original allocation and attitude measures for clarity
    select(-starts_with("attitude"), -starts_with("allocate")) %>%
    ## create lab and country variables
    mutate(
        ## fake lab for pilot analyses
        lab = case_when(userlanguage == "EN" ~ "01",
                        userlanguage == "EN-GB" ~ "01",
                        TRUE ~ "other"),
        country = case_when(userlanguage == "EN" ~ "USA",
                            userlanguage == "EN-GB" ~ "GBR",
                            TRUE ~ "other"),
        .before = 2) %>%
    ## remove leading 0s from gamepayoffs
    mutate(gamepayoff = str_remove(gamepayoff, "^0+")) %>%
    mutate(gamepayoff = if_else(gamepayoff == "", as.numeric(0), as.numeric(gamepayoff))) %>%
    ## create trust variable
    mutate(across(starts_with("interpersonaltrust"), numeric_likert_trust)) %>%
    mutate(trust_in = (interpersonaltrust_1 + # family
                       interpersonaltrust_2 + # neighourhood
                       interpersonaltrust_3)/3, # personally
           trust_out = (interpersonaltrust_4 + # first time
                        interpersonaltrust_5 + # another religion
                        interpersonaltrust_6)/3, # another nationality
           trust_in_out = trust_in - trust_out
           ) %>%
    ## create self-esteem variable (remove text)
    mutate(self_esteem = as.numeric(str_extract(selfesteem_1, "[0-9]+"))) %>%
    ## convert collectivism-individualism responses into numeric values
    mutate(across(starts_with("col_ind"), ~ as.numeric(str_extract(., "[0-9]+")))) %>%
    ## create collectivism-individualism measure
    mutate(
        horizontal_individualism =
            (col_ind_6 + # different and unique from others
             col_ind_8 + # do my own thing
             col_ind_11)/3, # unique individual
        vertical_collectivism =
            (col_ind_2 + # do what would please my family
             col_ind_3 + # sacrifice my self-interest
             col_ind_7 + # children should feel honored
             col_ind_12)/4, # sacrifice an activity that I enjoy very much
        permeability = vertical_collectivism - horizontal_individualism) %>%
    ## rename basics
    mutate(date = start_date) %>%
    ## rename attention checks
    mutate(attention_general_1 = attention1_1,
           attention_minimal = minimalgroupcheck,
           attention_general_2 = attention2
           ) %>%
    mutate(pass_attention_general_1 = if_else(attention_general_1 == "Somewhat Disagree",
                                              1, 0),
           pass_attention_minimal = if_else(attention_minimal == minimal_group,
                                            1, 0),
           pass_attention_general_2 = if_else(attention_general_2 == 18,
                                              1, 0),
           pass_attention_total =
               pass_attention_general_1 +
               pass_attention_minimal +
               pass_attention_general_2) %>%
    ## combine political affiliation, rename demographic vars
    mutate(political_affiliation =
               if_else(is.na(politicalaffi_us),
                       politicalaffi_uk, politicalaffi_us),
           political_orientation = politicalorie,
           education = edu,
           num_children = numchildren,
           num_siblings = numsibling,
           num_cousins = numcousins,
           size_of_residence = size_of_town,
           region_childhood =
               if_else(is.na(state_childhood),
                       region_childhood, state_childhood),
           region_current =
               if_else(is.na(state_current),
                       region_childhood, state_current),
           mgp_familiarity = familiarity_wmgp
           ) %>% 
    ## reorder
    select(
        ## basics
        id,
        date, start_time, end_time, progress, duration, 
        lab, country, userlanguage, minimal_group,
        ## attitudes and allocations
        starts_with("dg_"), starts_with("att_"),
        ## explanatory measures
        self_esteem,
        trust_in, trust_out, trust_in_out,
        horizontal_individualism, vertical_collectivism, permeability,
        ## attention checks
        starts_with("attention_"),
        starts_with("pass_attention_"),
        ## trust
        contains("interpersonaltrust"),
        contains("institutiontrust"),
        contains("familytie"),
        contains("col_ind_"),
        ## demographics
        gender, age,
        political_affiliation,
        political_orientation,
        income_when16,
        work_situation,
        education,
        marital_status,
        starts_with("num_"),
        residence_wparents,
        size_of_residence,
        freq_moving,
        region_childhood,
        region_current,
        comment,
        mgp_familiarity,
        mgp_followup,
        starts_with("trade_off"),
        ## random allocation and embedded data
        currency, conversionrate, dvorder, mingroup,
        ingroupletter, outgroupletter,
        role, gamepayoff, totalmoney
        )

## ## save as csv
## df_processed %>% write_csv("../data/data-pilot-processed.csv")
