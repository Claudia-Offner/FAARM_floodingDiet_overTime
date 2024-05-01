# Create Flood Shock Metric


#### 0. Format data ####
# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/')

library(tidyr)
library(dplyr)
library(INLA)

# Load FAARM data
df <- read.csv(file='GitHub/FAARM_Analysis/Data/Cluster100_10mflood_diet_df.csv', fileEncoding='UTF-8-BOM')

# Select relevant variables
df <- df %>% select(c_code, wcode, year, month, year_month, treatment, perc_flooded, 
                    dd10r_score, dd10r_min, DDLag_score, DDLag_bin,
                    age_3_BL, g_2h_BL, fam_type_BL, dep_ratio,  dd10r_score_EL,
                    dd10r_score_BL, dd10r_min_BL, g_2h_BL, fam_type_BL, 
                    wi_hl_BL, wi_al_BL, wi_land_BL,  
                    woman_edu_cat__BL, mobility_BL, support_BL, 
                    communication_BL, decision_BL, pb_621_BL,know_score_BL,
                    dep_ratio, md_score_BL, wealth_BL, dec_BL, quint_BL, 
                    terc_BL, wealth2_BL, dec2_BL, quint2_BL, terc2_BL)

# Remove baseline & Set date column
df<-subset(df, year_month!='0-0')

### NOTE: INLA struggles with +100k rows - so subset for now
df <-subset(df, year != 2019)
df <-subset(df, year != 2018)

#### Metrics ####

df$season <- NULL
df$season[df$month == 1] <- 'BoroStart'
df$season[df$month == 2] <- 'BoroStart'
df$season[df$month == 3] <- 'BoroEnd'
df$season[df$month == 4] <- 'BoroEnd'
df$season[df$month == 5] <- 'Aus' 
df$season[df$month == 6] <- 'Aus' 
df$season[df$month == 7] <- 'AmanStart'
df$season[df$month == 8] <- 'AmanStart'
df$season[df$month == 9] <- 'Aman'
df$season[df$month == 10] <- 'Aman'
df$season[df$month == 11] <- 'AmanEnd'
df$season[df$month == 12] <- 'AmanEnd'

flood_mean <- df %>%
  group_by(c_code, season) %>%
  summarise_at(vars(perc_flooded), list(cluster_season_mean = mean))%>%
  as.data.frame()

df <- merge(df, flood_mean, by=c('c_code'))
df <- df[order(df$wcode, df$year_month), ]

df$flood_anom = df$perc_flooded - df$cluster_season_mean

#### Test ####

#### Gaussian Mixed Effects Models w/ lag
inla_Gme <- inla(DDLag_score ~ perc_flooded+treatment+wealth2_BL+dd10r_score_BL
                     +f(wcode, model = 'iid'),
                     family ='gaussian', data = df, 
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), # compute model selection criteria
                     control.predictor = list(compute = TRUE))
summary(inla_Gme) # Mixed-Effects (Gaussian w/ lag)

#### Gaussian Mixed Effects Models w/ lag
inla_Gme.anom <- inla(DDLag_score ~ flood_anom+treatment+wealth2_BL+dd10r_score_BL
                     +f(wcode, model = 'iid'),
                     family ='gaussian', data = df, 
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), # compute model selection criteria
                     control.predictor = list(compute = TRUE))
getINLA_res(inla_Gme.anom) # Mixed-Effects (Gaussian w/ lag)
