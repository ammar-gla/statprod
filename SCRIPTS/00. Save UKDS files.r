## NOTE: this assumes that the folders contain SPSS .sav files to extract!
## ALSO requires that you already have the INPUT folder within your project fold

library("here")

# Shorten the names of the folders
all_in_folder <- "C:\\Users\\ALjubijankic\\Downloads" #<--- This depends on where your files are
files_names <- list.files(all_in_folder,pattern = "\\d{4}spss.*\\.zip",recursive = T)
new_names <- gsub("(\\d{4})(.*)(spss.*)*(\\.zip)","\\1",files_names)
#rename
file.rename(from=c(paste0(all_in_folder,"/",files_names)),
            to=c(paste0(all_in_folder,"/",new_names,".zip")))

# Unzip and copy out the datasets
for (fold in new_names) {
  
  # First unzip
  zipF<- paste0(all_in_folder,"/",fold,".zip")
  unzip(zipF, 
        exdir=paste0(here::here("INPUT"),"/",fold),
        overwrite=TRUE) # NOTE: This will copy the unzipped folder to the project for documentation purposes
  
  # Extract the data file
  files_path <- list.files(path=here::here("INPUT",fold),pattern=".sav",recursive = TRUE) # should only be one .sav file in folder
  
  file.copy(from=here::here("INPUT",fold,files_path), to=paste0(here::here("INPUT"),"/","DATA"), 
            recursive = TRUE,
            overwrite = TRUE)
  
}

# 
# # Moving all files at once (not needed in general running)
# file.copy(from=file.path(here::here("INPUT"),dir(here::here("INPUT"),"/*.sav")),
#           to=here::here("INPUT","DATA"),
#           recursive = FALSE,
#           overwrite = TRUE,
#           copy.mode = TRUE)
