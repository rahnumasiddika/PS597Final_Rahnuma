# library (tidyverse)
pacman::p_load(tidyverse, rio)

#Project Title: Moving Towards Authoritarianism: Estimating Its Impact on Self-Censorship
#Downloading all the data sets and relevant variables 

#Selected items are available in all waves 

#Pre-treatment dataframes for BD: Wave 3 (data collected in 1996 ) & 4 (data collected in 2002)
#Post-treatment dataframe for BD: Wave 7 (data collected in 2018)

#Party in power at the time of data collection: BAL 
#treatment= authoritarian regime (proxy for state repression)  

df1 <- import("WV3_Data_Bangladesh_Stata_v20221107.dta", setclass = "tibble")|> 
  #a set of politically sensitive questions, a set of political nonsensitive questions and a set of nonsensitive questions

  select(wave = V1, conf_tv = V139, trust = V27, discuss = V37, 
         national_pride = V205,  income_inequality = V125,
         party_vote = V210, conf_gov = V142, sign_petition = V118, 
         boycott = V119, democracy = V157, imp_family = V4, happiness = V10, leisure = V6, imp_friends = V5,  
         imp_work = V8,  pol_interest = V117, sex = V214, age = V216,
         educ = V217R, income = V227, party_vote_local = V210_local, 
         party_vote2 = V211, party_vote3 = V212, survey_interest = V231)|> 

  
  
  #mutating party vote to regime support 
  mutate(party_vote = if_else(party_vote > 10000, party_vote - 50000, party_vote)) |> 
  mutate(regime_supporter = if_else(party_vote == 1, 1, 0))|> 

  ## mutating pol_interest into binary category of respondents who are interested (very and somewhat) in politics (1 & 2) and who are not  
  mutate(interested_pol = if_else(pol_interest %in% c(1,2), 1, 0)) |> 
  
  ## mutating survey_interest into binary category of respondents who are interested (very and somewhat) while answering the survey (1 & 2) and who are not      
  mutate (interested_survey = if_else(survey_interest %in% c(1, 2), 1, 0))  

df2 <- import("WV4_Data_Bangladesh_Stata_v20201117.dta", setclass = "tibble") |> 
  #a set of politically sensitive questions, a set of political nonsensitive questions and a set of nonsensitive questions
  #nationalism for future use
  select(wave = V1, conf_tv = V150, trust = V25, discuss = V32,
         national_pride = V216, income_inequality = V141,
         party_vote = V220, conf_gov = V153, sign_petition = V134, 
         boycott = V135, democracy = V167, imp_family = V4, happiness = V11, leisure = V6, imp_friends = V5, 
         imp_work = V8, pol_interest = V133, sex = V223, age = V225, 
         educ = V226R, income = V236, survey_interest = V240) |> 
  
  #mutating party vote to regime support 
  mutate(party_vote = if_else(party_vote > 10000, party_vote - 50000, party_vote)) |> 
  mutate(regime_supporter = if_else(party_vote == 1, 1, 0))|> 

  #mutating pol_interest into binary category of respondents who are interested (very and somewhat) in politics (1 & 2) and who are not  
  mutate(interested_pol = if_else(pol_interest %in% c(1,2), 1, 0)) |> 
  
  #mutating survey_interest into binary category of respondents who are interested (very and somewhat) while answering the survey (1 & 2) and who are not      
  mutate (interested_survey = if_else(survey_interest %in% c(1, 2), 1, 0))  

df3 <- import("WVS_Wave_7_Bangladesh_Stata_v2.0.dta", setclass = "tibble") |>
  mutate(wave = 7) |> 
  #a set of politically sensitive questions, a set of political nonsensitive questions and a set of nonsensitive questions
  select(wave, conf_tv = Q67, trust = Q57, discuss = Q200,
         national_pride = Q254, income_inequality = Q106,
         party_vote = Q223, conf_gov = Q71, sign_petition = Q209, 
         boycott = Q210, democracy = Q238, imp_family = Q1, happiness = Q46, leisure = Q3, imp_friends = Q2,  
         imp_work = Q5,  pol_interest = Q199, sex = Q260, age = Q262, 
         educ = Q275R, income = Q288, survey_interest = E_RESPINT) |> 
  
  #mutating party vote to regime support 
  mutate(party_vote = if_else(party_vote > 10000, party_vote - 50000, party_vote)) |> 
  mutate(regime_supporter = if_else(party_vote == 1, 1, 0)) |>

  #mutating pol_interest into binary category of respondents who are interested (very and somewhat) in politics (1 & 2) and who are not  
  mutate(interested_pol = if_else(pol_interest %in% c(1,2), 1, 0)) |> 
  
  #mutating survey_interest into binary category of respondents who are interested (very and somewhat) while answering the survey (1 & 2) and who are not      
  mutate (interested_survey = if_else(survey_interest %in% c(1, 2), 1, 0)) 

# politically sensitive questions: party_vote, sign_petition, boycott, conf_gov, democracy
# politically nonsensitive questions: conf_tv, trust, discuss, national_pride, income_inequality
# totally nonsensitive questions: imp_family, happiness, leisure, imp_friends, imp_work
# confounders and controls: polinterest, sex, age, educ, income, survey_interest

# dichotomizing variables (-1 and -2 are coded for Don't know and No answer options in the original datasets)

# for selfcens_index1 we use nonresponses in politically sensitive and politically nonsensitive items 
# for selfcens_index2 we use nonresponses in politically sensitive and totally nonsensitive items
# for selfcens_index3 we only use nonresponses in politically sensitive items 


# Combine the three data frames into a single data frame
df <- bind_rows(df1, df2, df3) |> 
  mutate(treatment = if_else(wave %in% c(3, 4), 0, 1)) |> 
  mutate(selfcens_vote = if_else(party_vote %in% c(-1, -2), 1, 0),
         selfcens_petition = if_else(sign_petition %in% c(-1, -2), 1, 0),
         selfcens_boycott = if_else(boycott %in% c(-1, -2), 1, 0),
         selfcens_confgov = if_else(conf_gov %in% c(-1, -2), 1, 0),
         selfcens_democracy= if_else(democracy %in% c(-1, -2), 1, 0),
         
         selfcens_conftv = if_else(conf_tv %in% c(-1, -2), 1, 0),
         selfcens_trust = if_else(trust %in% c(-1, -2), 1, 0),
         selfcens_incomeineq = if_else(income_inequality %in% c(-1, -2), 1, 0),
         selfcens_nationalpride = if_else(national_pride %in% c(-1, -2), 1, 0),
         selfcens_discuss= if_else(discuss %in% c(-1, -2), 1, 0), 
         
         selfcens_family = if_else(imp_family %in% c(-1, -2), 1, 0), 
         selfcens_happiness= if_else(happiness %in% c(-1, -2), 1, 0), 
         selfcens_leisure = if_else(leisure %in% c(-1, -2), 1, 0), 
         selfcens_friends = if_else(imp_friends %in% c(-1, -2), 1, 0), 
         selfcens_work = if_else(imp_work %in% c(-1, -2), 1, 0)) |>
  
  rowwise() |> 
  mutate(selfcens_index1 = ((sum(selfcens_petition, selfcens_boycott, 
                                selfcens_vote, selfcens_confgov, selfcens_democracy)) / 5 - 
                              (sum(selfcens_conftv, selfcens_trust, 
                                  selfcens_incomeineq, selfcens_nationalpride, selfcens_discuss)) / 5) * 100) |> 
  
  rowwise() |> 
  mutate(selfcens_index2 = ((sum(selfcens_petition, selfcens_boycott, 
                                selfcens_vote, selfcens_confgov, selfcens_democracy)) / 5 - 
                              (sum(selfcens_family, selfcens_happiness, 
                                  selfcens_leisure, selfcens_friends, selfcens_work)) / 5) * 100) |> 
  
  rowwise() |> 
  mutate(selfcens_index3 = ((sum(selfcens_petition, selfcens_boycott, 
                                selfcens_vote, selfcens_confgov, selfcens_democracy))/5)*100)

  
  write_csv(df, "merged_data.csv")

df 


###Descriptive 
summary_table <- df %>%
  filter(treatment %in% c(0, 1)) %>%
  group_by(treatment) %>%
  summarize(
    selfcens_petition_mean = mean(selfcens_petition),
    selfcens_boycott_mean = mean(selfcens_boycott),
    selfcens_vote_mean = mean(selfcens_vote),
    selfcens_confgov_mean = mean(selfcens_confgov),
    selfcens_democracy_mean = mean(selfcens_democracy),
    selfcens_conftv_mean = mean(selfcens_conftv),
    selfcens_trust_mean = mean(selfcens_trust),
    selfcens_incomeineq_mean = mean(selfcens_incomeineq),
    selfcens_nationalpride_mean = mean(selfcens_nationalpride),
    selfcens_discuss_mean = mean(selfcens_discuss),
    selfcens_family_mean = mean(selfcens_family),
    selfcens_happiness_mean = mean(selfcens_happiness),
    selfcens_leisure_mean = mean(selfcens_leisure),
    selfcens_friends_mean = mean(selfcens_friends),
    selfcens_work_mean = mean(selfcens_work),
    age_mean = mean(age),
    income_mean = mean(income),
    education_mean = mean(educ),
    sex_mean = mean(sex),
    regime_supporter_mean = mean(regime_supporter),
  )

summary_table

#library(writexl)
#write_xlsx(summary_table, "all variables.xlsx")
