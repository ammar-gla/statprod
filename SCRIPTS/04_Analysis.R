
#_____________________________________________________________________________
### Analysis ----
#_____________________________________________________________________________

# All datasets are in lists, so use lapply to perform on each.
# Then can use delist_results to combine all datasets with dataset name shown

#.............................................................................
#### Prepare analysis data ----
#.............................................................................

# Create a new variable with Bangladeshi+Pakistani together
survey_design_blackother <- survey_design_full %>% 
  map(~mutate(.,
              ethnicity_blackother = case_when(ethnicity %in% c("Black or Black British") ~ "Black or Black British",
                                               is.na(ethnicity) ~ NA_character_,
                                               TRUE ~ "All other ethnicities")))

# Run for sex*age group
demog_vars_sex_age <- c("SEX_label","age_group")

emp_rates_sex_age <- lapply(survey_design_blackother,
                            summarise_econ_stat,
                            econ_status = "employed",
                            adult_only = F,
                            ldn_only = F,
                            var_vector = demog_vars_sex_age) %>% 
  delist_results(var_vector = demog_vars_sex_age)

emp_rates_sex_age_ldn <- lapply(survey_design_blackother,
                                summarise_econ_stat,
                                econ_status = "employed",
                                adult_only = F,
                                ldn_only = T,
                                var_vector = demog_vars_sex_age) %>% 
  delist_results(var_vector = demog_vars_sex_age)

# Run onfor agegroup2 * black v others
demog_vars_blackother <- c("SEX_label","ethnicity_blackother","age_group2")

emp_rates_blackother <- lapply(survey_design_blackother,
                               summarise_econ_stat,
                               econ_status = "employed",
                               adult_only = F,
                               ldn_only = F,
                               var_vector = demog_vars_blackother) %>% 
  delist_results(var_vector = demog_vars_blackother)

emp_rates_blackother_ldn <- lapply(survey_design_blackother,
                                   summarise_econ_stat,
                                   econ_status = "employed",
                                   adult_only = F,
                                   ldn_only = T,
                                   var_vector = demog_vars_blackother) %>% 
  delist_results(var_vector = demog_vars_blackother)

# Run for ethnicity*sex
demog_vars_sex_eth <- c("SEX_label","ethnicity")

emp_rates_sex_eth <- lapply(survey_design_blackother,
                            summarise_econ_stat,
                            econ_status = "employed",
                            adult_only = F,
                            ldn_only = F,
                            var_vector = demog_vars_sex_eth) %>% 
  delist_results(var_vector = demog_vars_sex_eth)

emp_rates_sex_eth_ldn <- lapply(survey_design_blackother,
                                summarise_econ_stat,
                                econ_status = "employed",
                                adult_only = F,
                                ldn_only = T,
                                var_vector = demog_vars_sex_eth) %>% 
  delist_results(var_vector = demog_vars_sex_eth)


# Export the results to Excel
list_of_datasets <- list("sex_age" = emp_rates_sex_age, "sex_age ldn" = emp_rates_sex_age_ldn,
                         "age2_black" = emp_rates_blackother, "age2_black ldn" = emp_rates_blackother_ldn,
                         "sex_eth" = emp_rates_sex_eth, "sex_eth ldn" = emp_rates_sex_eth_ldn)
write.xlsx(list_of_datasets, file = paste0(DATA_OUT,"emp_rates.xlsx"))
