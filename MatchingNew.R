
# Loading necessary packages
library(optmatch)
library(rio)
library(tidyverse)
library(RItools)

#Importing data: The script reads in the merged data file in CSV format using rio::import() function 
#and sets it as a tibble class.

df <- import("./merged_data.csv", setclass = "tibble")

# Just descriptive analysis: examine the distribution of these variables among different treatment and control groups.
df |> 
  group_by(regime_supporter, treatment, selfcens_vote) |> 
  count()
df |> 
  group_by(regime_supporter, treatment, selfcens_petition) |> 
  count()
df |> 
  group_by(regime_supporter, treatment, selfcens_boycott) |> 
  count()
df |> 
  group_by(regime_supporter, treatment, selfcens_confgov) |> 
  count()



# For matching we are choosing confounding variables  
covs <- c("sex", "age", "educ", "income")

#Dropping missing values: The script drops all the rows that have missing values in any of the covariate variables using dplyr::drop_na() function. The covariate variables are defined in the covs object and include sex, age, educ, and income

df |> drop_na(any_of(covs))

#Creating the formula for balancing: The script creates a formula for balancing covariates between treatment and control groups using the reformulate() function. The formula includes only the covariate variables defined in the covs object and sets treatment as the response variable.

formula_balance <- reformulate(covs, response = "treatment")

#Matching on covariates: The script matches the data on the covariate formula using optmatch::match_on() function and the Mahalanobis distance method. The exactMatch() function specifies the variables that should be exactly matched on (treatment, interest in survey and politics). The function outputs a matching vector (mah_dist) that assigns each observation to a matched group.

mah_dist <- match_on(formula_balance, 
                     exactMatch(treatment ~ interested_pol + interested_survey + sex, data = df),
                     data = df, 
                     method = "rank_mahalanobis")

cal1 <- caliper(match_on(treatment ~ income, data = df, method = "euclidean"), 1)
cal2 <- caliper(match_on(treatment ~ educ, data = df, method = "euclidean"), 1)
cal3 <- caliper(match_on(treatment ~ age, data = df, method = "euclidean"), 3.1)

mah_cals <- mah_dist  + cal2 + cal3  + cal1

fmatch1_cal <- fullmatch(mah_cals, data = df)

df$strata <- fmatch1_cal

df_balance <- balanceTest(treatment ~ income + educ + age + sex + interested_pol + interested_survey + strata(strata), data = df, p.adjust.method = "none")
df_balance$overall[1,3]  

df %>% 
  filter(treatment == 1 & !is.na(strata))
  


mean(is.na(fmatch1_cal))


unique(df$strata)

df %>% filter(strata == "0.1.1.310") %>% 
  select(c(1:9), treatment)


#Creating the matched dataset: The script creates a full matching using optmatch::fullmatch() function and the matching vector (mah_dist). The function assigns each observation to a matched group and creates a new variable match_group that contains the group number for each observation.

exclud
full_match <- fullmatch(mah_dist, data = df) 


df <- df |> mutate(match_group = full_match)

tab <- df |> 
  group_by(match_group) |> 
  summarize(
    n = n(),
    interested_pol = mean(interested_pol[treatment == 1]) - mean(interested_pol[treatment == 0]),
    interested_survey = mean(interested_survey[treatment == 1]) - mean(interested_survey[treatment == 0]),
    sex = mean(sex[treatment == 1]) - mean(sex[treatment == 0]),
    age = mean(age[treatment == 1]) - mean(age[treatment == 0]),
    educ = mean(educ[treatment == 1]) - mean(educ[treatment == 0]),
    income = mean(income[treatment == 1]) - mean(income[treatment == 0])
  )
tab |> arrange(educ) |> print(n = 50)
tab |> arrange(desc(educ)) |> print(n = 50)

exclude_group <- tab |> 
  filter(sex %in% c(-1, 1) | abs(age) > 10 | abs(educ) > 0.5 | abs(income) > 0.7) |> 
  pull(match_group)

# How many did we lost?
nrow(df |> filter(match_group %in% exclude_group))


#Checking balance: The script checks the balance of the covariate variables between treatment and control groups using the RItools::balanceTest() function. The function calculates standardized mean differences (SMD) for each covariate and the overall balance test statistics. The filter() function is used to exclude any matched group that has a covariate imbalance larger than a pre-defined threshold for any of the covariate variables. In this case, the threshold is set as 0.5 for educ and 0.7 for income. The excluded groups are saved in the exclude_group object.

balance_test <- balanceTest(update(formula_balance, . ~ . + strata(match_group)),
                            data = df |> filter(!(match_group %in% exclude_group)))
  
balance_test$overall["match_group",]
balance_test$results[,, "match_group"]

#Exporting the matched dataset: The script exports the final matched dataset to a CSV file using rio::export() function. The excluded groups are removed from the dataset.
df |> 
  filter(!(match_group %in% exclude_group)) |> 
  export(file = "matched_datanew.csv")

