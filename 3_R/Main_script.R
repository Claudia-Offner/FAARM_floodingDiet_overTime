### ------------------------------------------------------------------------ ### 
### MAIN FILE TO RUN ALL SCRIPTS
### ------------------------------------------------------------------------ ### 


#### !!! IMPORTANT !!! (READ ME) ####

# 1. Set file paths to appropriate folder locations
data_path <- 'C:/Users/offne/OneDrive - University of Cambridge/V. Other/Flooding-Diets-HFP/Data/'
git_path  <- 'C:/Users/offne/Documents/GitHub/FAARM_floodingDiet_overTime/3_R'
setwd(git_path)

# 2. Scripts require the following folders in 3_R: 
# Main Results, Sensitivity Results, Tables, Figures. These should automatically 
# be created when you pull from the repo, but if not, create them before running.

# 3. Figure MF2 needs an api key to load the basemap. It will run without a key, 
# but if you want the basemap used in the manuscript, create your own key at
# https://www.appsilon.com/post/r-ggmap, and use the following code:
# ggmap::register_stadiamaps('KEY', write = TRUE)

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
