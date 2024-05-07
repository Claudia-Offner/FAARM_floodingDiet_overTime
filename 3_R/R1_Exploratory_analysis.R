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

# CHECK THE NUMBER OF OBSERVATIONS & RANDOM EFFECTS GROUPINGS IN MODEL ####
# Number of obs: 21561, groups:  wcode, 2620; c_code, 96; season_id, 24

# CONFIRM NUMBER OF WOMEN WITH DATA COLELCTED OVER SURVEILLANCE
# Get amount of data collected for each woman
y <- df %>%
  group_by(wcode) %>%
  summarise(count=n(),
            diet=sum(!is.na(dd10r_score_m)),
            flood=sum(!is.na(perc_flooded_c)),
            DDperc=diet/count*100,
            FloodPerc=flood/count*100)
# Number of women who had no observations over surveillance (can get IDs too)
count(y[y$diet==0,])
count(y[y$diet>=8,])

# Descriptive proportions (binary outcomes)
agg <-aggregate(cbind(dd10r_min_m) ~ perc_flooded_c+treatment+season_flood, sum, data = df)
agg$prop <- agg$dd10r_min_m/2620;agg


# 2. Tests for temporal trends and dependencies

# 3. Tests for spatial distributions and dependencies

