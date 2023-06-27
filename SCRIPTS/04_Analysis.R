
#_____________________________________________________________________________
### Analysis ----
#_____________________________________________________________________________

# All datasets are in lists, so use lapply to perform on each.
# Then can use delist_results to combine all datasets with dataset name shown

#.............................................................................
#### Prepare analysis data ----
#.............................................................................

# Create a new variable with Bangladeshi+Pakistani together
survey_design_bangpak <- survey_design_full %>% 
  map(~mutate(.,
           ethnicity_bangpak = case_when(ETHUKEUL_label %in% c("Bangladeshi","Pakistani") ~ "Bangladeshi/Pakistani",
                                         TRUE ~ ETHUKEUL_label)))

# Run for sex*ethnicity
demog_vars_sex_eth <- c("GOVTOF_label","SEX_label","ethnicity_bangpak")

unemp_rates_sex_eth <- lapply(survey_design_bangpak,
                      summarise_econ_stat,
                      econ_status = "unemployed",
                      var_vector = demog_vars_sex_eth) %>% 
  delist_results(var_vector = demog_vars_sex_eth)

emp_rates_sex_eth <- lapply(survey_design_bangpak,
                         summarise_econ_stat,
                         econ_status = "employed",
                         var_vector = demog_vars_sex_eth) %>% 
  delist_results(var_vector = demog_vars_sex_eth)

# Run only for ethnicity
demog_vars_eth <- c("GOVTOF_label","ethnicity_bangpak")

insecure_rates_eth <- lapply(survey_design_bangpak,
                         summarise_econ_stat,
                         econ_status = "employed",
                         var_vector = demog_vars_eth) %>% 
  delist_results(var_vector = demog_vars_eth)
