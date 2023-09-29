#_______________________________________________________________________________
###  RUN ALL SCRIPTS ----
#_______________________________________________________________________________
  
  # HERE package needed for dynamic pathfinding
  library("here") 

#...............................................................................
#### Run Setup scripts ----
#...............................................................................

  # Create paths as strings
  source(here("SCRIPTS","SUBSCRIPTS","GLAE_paths.r"))
  
  # Data packages
  source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))

  # Inputs such as borough codes in Nomis
  source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))
  
  # Run the subscripts necessary for markdown
  source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))
  
  # Main data
  source(paste0(SCRIPTS,"01_Functions_and_setup",".r"))
  
  source(paste0(SCRIPTS,"02_Dataload",".r"))
  
  source(paste0(SCRIPTS,"03_Setup",".r"))
  
  #...............................................................................
  #### Run analysis ----
  #............................................................................... 

  source(paste0(SCRIPTS,"04_Analysis",".r"))

  
  
  
  