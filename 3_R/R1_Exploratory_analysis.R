################################################################################
#### EXPLORATORY ANALYSIS #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/2. Data/')

## Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)


# 1. Descriptive Statistics ####

hist(df$Flood_1Lag)
plot(df$dd10r_score, df$Flood_1Lag)


# 2. Tests for temporal trends and dependencies

# 3. Tests for spatial distributions and dependencies

