#_____________________________________________________________________________
### Survey level analysis setup ----
#_____________________________________________________________________________

# All datasets will be saved within a list to be accessed separately, but variables
# should be consistent across them. This is to save space, make analysis quicker
# and avoid issues of treating different years as pooled dataset

#.............................................................................
#### Prepare analysis data ----
#.............................................................................


# Define which variables to transform to labels, which are automatically kept for analysis
label_var_vec <- c("SEX","GOVTOF","ILODEFR","BENFTS","INDS07M",
                   "lev_quals","RELIG11","ETHUKEUL")

# Define which further variables to keep for analysis to save memory, specifically custom vars or numeric ones 
analysis_var_vec <- c("AGE","adult1664","weight_val",
                      "employed","london_resident","ethnicity",
                      "inactive","unemployed","disability",
                      "age_group","wfh_d","pt_d","insecure_work")

# Prepare all datasets by:
# 1 creating consistent variable across datasets for those that otherwise vary by year (e.g. SOC)
# 2 converting specified variables to string labels as chosen above
# 3 removing all labels from variables due to issues in R
# 4 recoding the datasets to create new custom variables
# 5 keeping the selected variables from above
# 6 Create a custom ID ref, since some datasets just have NAs

dataset_list_adj <- dataset_list %>% 
  lapply(align_vars) %>% 
  lapply(convert_to_label,var_vec=label_var_vec) %>% 
  lapply(haven::zap_labels) %>% 
  lapply(recode_dta) %>% 
  lapply(select,c(analysis_var_vec,paste(label_var_vec,"_label",sep=""))) %>% 
  lapply(create_id_ref)

# To save space, remove original dataset list
#rm(dataset_list)

#.............................................................................
#### Build survey design ----
#.............................................................................

# Set up survey design within list
## The id variable is household, where clustering happens in survey
## Strata is lowest possible geography, for EUL data is region
## nest set to TRUE to detect if primary sampling units appear in multiple strate (they should not)

# Note: UKDS data often does not have HH id numbers, so use at individual level. 
# Will result in too tight uncertainty ranges!

survey_design_full <- lapply(X=dataset_list_adj,
                             as_survey_design,
                             ids   = alt_id_ref, # should be clustered at household, NOT family unit
                             strata = GOVTOF_label,#lowest possible geo 
                             weights = weight_val, 
                             nest  = TRUE)
