#_____________________________________________________________________________
### Dataload and data processing ----
#_____________________________________________________________________________

# Clear out existing lists if taking memory
dataset_list <- list()
dataset_list_adj <- list()

# Turns out we need APS datasets, not LFS
aps_dataset_ext_names <- c("apsp_jd06_eul","apsp_jd11_eul_pwta14","apsp_jd16_eul_pwta18","apsp_jd17_eul_pwta18",
                           "apsp_jd18_eul_pwta18","apsp_jd19_eul_pwta18","apsp_jd20_eul_pwta22",
                           "apsp_jd21_eul_pwta22","apsp_jd22_eul_pwta22")



# NB: SET THIS TO LFS OR APS!!!
ons_data_used <- "aps"

#.............................................................................
#### Load LFS stats, automatically clean and save ----
#.............................................................................

dataset_nm <- c()
dataset_years <- c()

if (str_to_lower(ons_data_used)=="lfs") dataset_ext_names <- lfs_dataset_ext_names
if (str_to_lower(ons_data_used)=="aps") dataset_ext_names <- aps_dataset_ext_names

for (y in 1:length(dataset_ext_names)) {
  
  # Produce a list with dataframe, name and year
  temp_list <- import_save_dta(dta_num = y,
                  loadRDS = T,
                  sav_dat =  TRUE,
                  aps_lfs = ons_data_used,
                  dataset_nm_vector = dataset_ext_names)
  
  # Create dataframe with name and save its name
  assign(temp_list[["name"]],temp_list[["dta"]])
  dataset_nm <- c(dataset_nm,temp_list[["name"]])
  
  # Save years in vector
  dataset_years <- c(dataset_years,temp_list[["year"]])

  # Delete list
  rm(temp_list)
  
}

rm(y)

# Give years as names to the vector
names(dataset_nm) <- as.character(dataset_years)

#.............................................................................
#### Adjustments to data ----
#.............................................................................

# Create list with datasets, including their name

dataset_list <- setNames(lapply(dataset_nm, get),dataset_nm)

# Create overview HTMLs of the datasets
#sjPlot::view_df(apsp_jd22,show.prc = T, file = paste0(OTHERDATA,"APS_overview_labels_2022.html"))
#sjPlot::view_df(dataset_list$apsp_jd11,show.prc = T, file = paste0(OTHERDATA,"APS_overview_labels_2011.html"))
#sjPlot::view_df(dataset_list$apsp_jd06,show.prc = T, file = paste0(OTHERDATA,"APS_overview_labels_2006.html"))
#sjPlot::view_df(dataset_list$apsp_jd16,show.prc = T, file = paste0(OTHERDATA,"APS_overview_labels_2016.html"))

# Remove individual datasets to save memory
rm(list=dataset_nm)

