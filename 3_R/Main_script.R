### ------------------------------------------------------------------------ ### 
### MAIN FILE TO RUN ALL SCRIPTS
### ------------------------------------------------------------------------ ### 


#### !!! IMPORTANT !!! (READ ME) ####

# 1. Set file paths to appropriate folder locations
data_path <- 'C:/Users/claer14/OneDrive - University of Cambridge/V. Other/Flooding-Diets-HFP/Data/'
git_path  <- 'C:/Users/claer14/Documents/GitHub/FAARM_floodingDiet_overTime/3_R'
setwd(git_path)

# 2. Make sure all packages in the R0_Dependencies.R file are installed prior to running

# 3. Scripts run more smoothly if you clear the folders first (keep the folder 
# structure within 3_R, just delete the folders/files within)

#### MAIN CODE ####

start_time <- Sys.time()

scripts <- c('R1_Data_formating.R', #'R2_Sensitivity_analysis.R', 'R3_Main_analysis.R',
             'R4A_Generate_tables.R', 'R4B_Generate_figures.R')
for(s in scripts){
  
  # Clear environment
  rm(list=ls()[!ls() %in% c('s', 'scripts', 'data_path', 'git_path')]); graphics.off(); gc()
  
  # Re-load dependencies
  source('R0_Dependencies.R')
  
  # Run script
  print(s)
  source(s, echo=TRUE)
  
}
