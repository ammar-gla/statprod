
#_____________________________________________________________________________
### Analysis ----
#_____________________________________________________________________________

# All datasets are in lists, so use lapply to go on each.
# Alternatively combine all datasets and then remember to specify year as cutting variable

#.............................................................................
#### Prepare analysis data ----
#.............................................................................

summarise_econ_stat(survey_design_full$apsp_jd22,
                    econ_status = "unemployed",
                    var_vector = c("GOVTOF_label","SEX_label","ethnicity"))