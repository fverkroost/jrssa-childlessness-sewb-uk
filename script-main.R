# Clear workspace and console and install/load packages
source('script-initialisation.R')

# Set class of life satisfaction (integer or ordinal)
class_lifesat <- "integer"

# Source the functions
source('functions-main.R')

# Data operationalisation
source('script-data-prep.R')

# Check attrition and censoring
source('script-attrition-censoring.R')

# Number of women who already had a child before age 26
res = womenBCS %>% 
  group_by(ID) %>%
  do(data.frame(val = childBefore26(.)))
print(paste0("Share of those who were mothers at age 26 (relative to all female respondents): ", table(res$val)['TRUE'] / sum(table(res$val))))

# Number of men who already had a child before age 26
res = menBCS %>% 
  group_by(ID) %>%
  do(data.frame(val = childBefore26(.)))
print(paste0("Share of those who were fathers at age 26 (relative to all male respondents): ", table(res$val)['TRUE'] / sum(table(res$val))))

# Change some of the variables (based on the results from above) and deal with censoring
men = dataPrep(
  data = menBCS, 
  age16 = 'delete', 
  class_lifesat = 'gaussian', 
  remove_parents_26 = FALSE, 
  censoring_removal = "at_least_one_forties"
)
women = dataPrep(
  data = womenBCS, 
  age16 = 'delete', 
  class_lifesat = 'gaussian', 
  remove_parents_26 = FALSE, 
  censoring_removal = "at_least_one_forties"
)
tot = rbind(men, women)

# Descriptive statistics and analysis
source('script-descriptives.R')

# Trajectories before and after first childbirth
source('script-trajectories-before-after-childbirth.R')

# Multiply impute male and female data
source('script-multiple-imputation.R')

# -----------------------------------------------------------------------------------------------------------------------
# Run all models on ARC server
# Six main models for men and women each: childless, education, marital, economic acitivity, health and total
# Five robustness models for men and women each: age 16 missing or zero (instead of deleted), 
# priors uninformative or automatic (instead of lightly informative), % missing as number of imputations (instead of 5)
# All model scripts and job files are found in subfolder "run_scripts"
# -----------------------------------------------------------------------------------------------------------------------

# No control variables
source('script_women_uncontrols_swb.R')
source('script_women_uncontrols_ewb.R')
source('script_men_uncontrols_swb.R')
source('script_men_uncontrols_ewb.R')

# Control variables for child arrival and number of children
source('script_women_child_swb.R')
source('script_women_child_ewb.R')
source('script_men_child_swb.R')
source('script_men_child_ewb.R')

# Control variables for marital status and cohabitation
source('script_women_marital_swb.R')
source('script_women_marital_ewb.R')
source('script_men_marital_swb.R')
source('script_men_marital_ewb.R')

# Control variable for education
source('script_women_education_swb.R')
source('script_women_education_ewb.R')
source('script_men_education_swb.R')
source('script_men_education_ewb.R')

# Control variable for economic activity
source('script_women_econact_swb.R')
source('script_women_econact_ewb.R')
source('script_men_econact_swb.R')
source('script_men_econact_ewb.R')

# Control variable for health status
source('script_women_health_swb.R')
source('script_women_health_ewb.R')
source('script_men_health_swb.R')
source('script_men_health_ewb.R')

# Control variable for the other dependent variable
source('script_women_otherdep_swb.R')
source('script_women_otherdep_ewb.R')
source('script_men_otherdep_swb.R')
source('script_men_otherdep_ewb.R')

# Control variable for child arrival and number, marital status and cohabitation, education,
# economic activity, health status and the other dependent variable
source('script_women_controls_swb.R')
source('script_women_controls_ewb.R')
source('script_men_controls_swb.R')
source('script_men_controls_ewb.R')

# Fully controlled model with automatic priors
source('script_women_controls_auto_swb.R')
source('script_women_controls_auto_ewb.R')
source('script_men_controls_auto_swb.R')
source('script_men_controls_auto_ewb.R')

# Fully controlled model with uninformative priors
source('script_women_controls_uninf_swb.R')
source('script_women_controls_uninf_ewb.R')
source('script_men_controls_uninf_swb.R')
source('script_men_controls_uninf_ewb.R')

# Fully controlled model without economic activity predicting EWB
source('script_women_controls_no_econact_ewb.R')
source('script_men_controls_no_econact_ewb.R')

# Multinomial models
source('script_multinom_cohabitation.R')
source('script_multinom_marital.R')
source('script_multinom_education.R')
source('script_multinom_econact.R')
source('script_multinom_health.R')

# Make outputs of models interpretable (tabels and figures)
source('script-results.R')

# source('script-explanations.R')
source('script-convergence.R')
