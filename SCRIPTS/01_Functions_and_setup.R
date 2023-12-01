#_____________________________________________________________________________
### Functions ----
#_____________________________________________________________________________

#.............................................................................
#### Processing functions ----
#.............................................................................

# Function to import tab data and manipulate. Alternatively load R datafile
import_save_dta <- function(dta_num=NA,
                            aps_lfs=NA,
                            loadRDS=FALSE,
                            sav_dat=TRUE,
                            dataset_nm_vector=NULL) {
  
  
  checkmate::assert_choice(str_to_lower(aps_lfs), choices = c("aps","lfs"))
  
  
  # Relevant names
  temp_year <- paste0("20",gsub("(\\w{4}_\\w{2})(\\d{2})(.*)","\\2", dataset_nm_vector[dta_num]))
  temp_name <- regmatches(dataset_nm_vector[dta_num],regexpr("\\w{4}_\\w{2}\\d{2}",dataset_nm_vector[dta_num]))
  
  # Note: very old datasets have lowercase names, change to uppercase
  
  if (loadRDS==FALSE) {
    if (sav_dat==FALSE) { #if tabulated data
      temp_dta <- read.table(file = paste0(INPUT,"\\",dataset_nm_vector[dta_num],".tab"),
                             header = TRUE) %>% 
        rename_with(toupper)
        
    }
    else { #otherwise load SPSS datasets
      temp_dta <-  read_sav(paste0(INPUT,"\\",dataset_nm_vector[dta_num],".sav")) %>% 
        rename_with(toupper)
    }
    
    # Save relevant weight in own column 
    if (str_to_lower(aps_lfs)=="aps") {
      if (temp_year < 2012) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PWTA14,
                 weight_var = "PWTA14")
      } else if (temp_year %in% c(2012:2019)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PWTA18,
                 weight_var = "PWTA18")
      } else if (temp_year %in% c(2020:2022)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PWTA22,
                 weight_var = "PWTA22")
      }
    } else if (str_to_lower(aps_lfs)=="lfs") {
      if (temp_year %in% c(2010:2011)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PWT14,
                 weight_var = "PWT14")
      } else  if (temp_year %in% c(2012:2019)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PWT18,
                 weight_var = "PWT18")
      } else if  (temp_year %in% c(2020:2022)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PWT22,
                 weight_var = "PWT22")
      }
    }
    
    # Save dataset year
    temp_dta <- temp_dta %>%
      mutate(dta_year = temp_year)
    
    # Save R data and allow loading
    saveRDS(temp_dta,
            file=paste0(RDATA,temp_name,".rds"))
    
  } else { #otherwise load dataset directly
    temp_dta <- readRDS(file=paste0(RDATA,temp_name,".rds"))
  }
  
  temp_list <- list("dta"=temp_dta,"name"=temp_name,"year"=temp_year)
  return(temp_list)
}

# Function to adjust the data for our use, inspired by previous SPS code
recode_dta <- function(dta=NA) #in case we want sumstats on whole pop 
{
  # Check year and set SOC var accordingly
  dta_year_check <- dta %>% 
    group_by(dta_year) %>% 
    filter(row_number()==1) %>% 
    pull(dta_year)
  
  # Change data
  dta_adj <- dta %>% 
    mutate(quarter_response = case_when(IOUTCOME %in% c(1,2) ~ "Yes",
                                        IOUTCOME == 6 ~ "No",
                                        TRUE ~ "NA"),
           employed=case_when(ILODEFR==1 ~ 1,
                              is.na(ILODEFR) | ILODEFR %in% c(-9,-8,4) ~ NA_real_,
                              TRUE ~ 0),
           unemployed=case_when(ILODEFR==2 ~ 1,
                                is.na(ILODEFR) | ILODEFR %in% c(-9,-8,3,4) ~ NA_real_, # no inactive or u16
                                TRUE ~ 0),
           inactive=case_when(ILODEFR==3 ~ 1,
                              is.na(ILODEFR) | ILODEFR %in% c(-9,-8,4) ~ NA_real_, #exclude u16s
                              TRUE ~ 0),
           econ_active=case_when(ILODEFR %in% c(1,2) ~ 1,
                                 is.na(ILODEFR) | ILODEFR %in% c(-9,-8,4) ~ NA_real_,
                                 TRUE ~ 0),
           adult = case_when(AGE>=16 ~ 1,
                             TRUE ~ 0),
           adult1664 = case_when(between(AGE,16,64) ~ 1,
                                 TRUE ~ 0),
           london_resident = case_when(GOVTOF == 8 ~ 1,
                                       TRUE ~ 0),
           age_group = case_when(between(AGE,16,19) ~ "Aged 16-19",
                                 between(AGE,20,24) ~ "Aged 20-24",
                                 between(AGE,25,34) ~ "Aged 25-34",
                                 between(AGE,35,49) ~ "Aged 35-49",
                                 between(AGE,50,64) ~ "Aged 50-64",
                                 AGE>64 ~ "Aged 65+"),
           age_group2 = case_when(between(AGE,16,29) ~ "Aged 16-29",
                                 between(AGE,30,39) ~ "Aged 30-39",
                                 between(AGE,40,49) ~ "Aged 40-49",
                                 between(AGE,50,59) ~ "Aged 50-59",
                                 between(AGE,60,64) ~ "Aged 60-64",
                                 AGE>64 ~ "Aged 65+")
    ) 
  return(dta_adj)
}

# Duplicate the rows to have a UK total in addition to London and RoUK
dup_dataset <- function(dta=NA) {
  dup_dta <- dta %>% 
    uncount(2, .id="row_version") %>% 
    mutate(london_resident = case_when(row_version == 2 ~ 2,
                                       TRUE ~ london_resident))
  
  return(dup_dta)
}

#.............................................................................
#### Utility functions ----
#.............................................................................

# Create new columns using value labels of existing ones
convert_to_label <- function(dta=NULL,
                             var_vec=c()) {
  dta_convert <- dta %>% 
    mutate(across(all_of(var_vec),sjlabelled::as_label,.names='{col}_label'))
  
  return(dta_convert)
}

# Make consistent variables that vary across years
align_vars <- function(dta=NULL){
  
  dta_year_check <- dta %>% 
    group_by(dta_year) %>% 
    filter(row_number()==1) %>% 
    pull(dta_year)
  # 
  # # First for quals
  # if (dta_year_check>=2022) {
  #   quals_var <- "HIQUL22D"
  # } else if (between(dta_year_check,2015,2021)) {
  #   quals_var <- "HIQUL15D"
  # }
  # 
  # # For occupations
  # if (dta_year_check>=2021) {
  #   soc_var_1d <- "SC20MMJ"
  #   soc_var_3d <- "SC20MMN"
  # } else if (between(dta_year_check,2015,2020)) {
  #   soc_var_1d <- "SC10MMJ"
  #   soc_var_3d <- "SC10MMN"
  # }
  # 
  # dta_convert <- dta %>% 
  #   mutate(lev_quals = !!sym(quals_var),
  #          soc_1d = !!sym(soc_var_1d),
  #          soc_3d = !!sym(soc_var_3d))
  
  # simple version for this task
  # ethnicity
  if (dta_year_check<2010) {
    
    dta_convert <- dta %>% 
      mutate(ethnicity = case_when(ETH01 == 1 ~ "White",
                                   ETH01 == 2 ~ "Mixed ethnicity",
                                   ETH01 == 4 ~ "Black or Black British",
                                   ETH01 %in% c(5,6) ~ "Other ethnic group",
                                   ETH01 == 3 & ETHAS %in% c(1) ~ "Indian", 
                                   ETH01 == 3 & ETHAS %in% c(2,3) ~ "Pakistani/Bangladeshi",
                                   ETH01 == 3 & ETHAS %in% c(4) ~ "Other ethnic group",
                                   ETH01 == -8 ~ "No answer",
                                   ETH01 == -9 ~ "NA"))
  } else {
    
    dta_convert <- dta %>% 
      mutate(ethnicity = case_when(ETHUKEUL == 1 ~ "White",
                                   ETHUKEUL == 3 ~ "Indian",
                                   ETHUKEUL %in% c(4,5) ~ "Pakistani/Bangladeshi",
                                   ETHUKEUL == 8 ~ "Black or Black British",
                                   ETHUKEUL == 2 ~ "Mixed ethnicity",
                                   ETHUKEUL %in% c(6,7,9) ~ "Other ethnic group",
                                   ETHUKEUL == -8 ~ "No answer",
                                   ETHUKEUL == -9 ~ "NA"))
    
  
  }
  return(dta_convert)
}

# Create a made-up ID reference number as some datasets do not have them
create_id_ref <- function(dta=NULL) {
  
  # Only create if there is no
  dta_with_ref <- dta %>% 
    mutate(alt_id_ref = row_number())
  
  return(dta_with_ref)
  
}


# Function to produce regression results based on model of variables input
svyglm_regress <- function(reg_model_vars=NULL,
                           design=NULL,
                           region_dummies=FALSE) {
  
  if (region_dummies==TRUE) { # Do same regressions adding other regions for interest
    
    # Construct a formula object
    reg_form <- formula_helper(outcome_var = "employed",
                               formula_vars = c(reg_model_vars,"manchester_resident","birmingham_resident"))
    
  } else {
    
    # Construct a formula object
    reg_form <- formula_helper(outcome_var = "employed",
                               formula_vars = reg_model_vars)
  }
  
  reg_emp_results <- svyglm(design=design,
                            formula = reg_form)
  
  return(reg_emp_results)
}


# Econ status sumstat
# Choose between employment, unemployment or inactivity rates
# Then assign a vector of variables to cut by, e.g. demography and geography
# Will produce a dataset with rates and number of observations

summarise_econ_stat <- function(data = NULL,
                                econ_status = NULL,
                                adult_only = TRUE,
                                ldn_only = TRUE,
                                var_vector = NULL) {
  
  checkmate::assert_logical(adult_only)
  checkmate::assert_logical(ldn_only)
  checkmate::assert_choice(econ_status, c("unemployed","employed","inactive","insecure_work"))
  checkmate::assert_vector(var_vector)
  
  # Ensure correct filters are on
  if (adult_only==TRUE) {
    data_temp <- data %>% 
      filter(adult1664 == 1)
  } else {
    data_temp <- data
  }
  
  if (ldn_only==TRUE) {
    data_temp <- data_temp %>% 
      filter(london_resident == 1)
  } else {
    data_temp <- data_temp
  }
  
  # Calculate the rate with appropriate uncertainty bounds
  subtab <- data_temp %>% 
    filter(!!sym(econ_status) %in% c(0,1)) %>% 
    group_by(across(all_of(var_vector))) %>% 
    summarise(survey_mean(!!sym(econ_status),
                          vartype = "ci"))
  
  # Simple aggregate counts of unweighted and weighted
  subtab_count <- data_temp %>% 
    filter(!!sym(econ_status) %in% c(0,1)) %>% #only econ active
    group_by(!!sym(econ_status),across(all_of(var_vector))) %>% 
    summarise(sum_obs_wt=sum(weight_val),
              sum_obs=n()) %>% 
    pivot_wider(names_from = !!sym(econ_status),
                values_from = c(sum_obs_wt,sum_obs))
  
  # Merge two datasets
  outtab <- subtab %>% 
    left_join(subtab_count,
              by=var_vector)
  
  return(outtab)
}

# Function to delist the results, create ID column
delist_results <- function(list_nm = NULL,
                           var_vector = NULL) {
  
  new_df <- bind_rows(list_nm,
                      .id="dataset") %>%  
    remove_rownames() %>% 
    unite("byvar_characteristic",all_of(var_vector), sep='_',remove = FALSE) %>% 
    mutate(data_year = as.numeric(paste0("20",str_extract(dataset,"\\d{2}"))),
           id = paste0(data_year,"_",byvar_characteristic)) %>%
    relocate(id)
  
  return(new_df)
}
