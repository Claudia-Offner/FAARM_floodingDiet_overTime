### ------------------------------------------------------------------------ ### 
### MAIN FILE TO RUN ALL SCRIPTS
### ------------------------------------------------------------------------ ### 


#### !!! IMPORTANT !!! (READ ME) ####

# 1. Set file paths to appropriate folder locations
data_path <- 'C:/Users/claer14/OneDrive - University of Cambridge/V. Other/Flooding-Diets-HFP/Data/'
git_path  <- 'C:/Users/claer14/Documents/GitHub/FAARM_floodingDiet_overTime/3_R'
setwd(git_path)

# 2. Make sure all packages in the R0_Dependencies.R file are installed prior to running

# 3. Scripts require less input if you clear the folders first. They should 
# automatically be cleared when you pull from the repo. However, if you re-run 
# the main script on your local disk, it helps if files are cleared from 3_R folders.

# 4. Figure MF2 needs an api key to load the basemap. It will run without a key, 
# but if you want the basemap used in the manuscript, create your own key at
# https://www.appsilon.com/post/r-ggmap, and use the following code:
# ggmap::register_stadiamaps("[KEY]", write = TRUE)

# NB: Indira, you can use my key, but I will remove when this goes public
ggmap::register_stadiamaps(key='f2f7765b-7259-42c9-a46d-fc1a61dc4375')


#### MAIN CODE (~3.5 hours) ####
# NB: R2 takes the longest (3 hours), followed by R3 (30min)

start_time <- Sys.time()

scripts <- c('R1_Data_formating.R', 'R2_Sensitivity_analysis.R', 'R3_Main_analysis.R', 
             'R4A_Generate_tables.R', 'R4B_Generate_figures.R')

for(s in scripts){
  
  # Clear environment
  rm(list=ls()[!ls() %in% c('s', 'scripts', 'data_path', 'git_path', 'start_time')]); graphics.off(); gc()
  
  # Re-load path & dependencies
  setwd(git_path)
  source('R0_Dependencies.R')
  
  # Run script 
  print(s)
  source(s, echo=TRUE)
  
}

end_time <- Sys.time()

# Print timings
total_time <- end_time - start_time
print(paste0('TIME TO RUN ANALYSIS: ', total_time))  
