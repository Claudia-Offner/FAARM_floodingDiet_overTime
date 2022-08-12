# VALIDATE RS FLOOD METRIC 

# To run this code, set the work directory to folder containing the provided files & data
# setwd('C:/Users/offne/Documents/GitHub/RINLA_FloodDiet_Modelling/')

# Load Data
library(here)
# suppressWarnings(source(here::here("0_DataFormatting.R")))

#### Load FAARM Flood Survey data
flood <- read.csv(file='C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/- DD-Flooding TimeSeries - CO/4. Data/Final/FAARM/JW_2017Flood.csv', fileEncoding='UTF-8-BOM')

#### Load packages
library(tidyr)


#### 1. Format data ####

# Subset and merge GEE & FAARM datasets
df <- df %>%
  group_by(wcode) %>%
  fill(dd10r_score_m, .direction = "down") %>%
  as.data.frame()

df <- df[df$year_season == '2017-3',]
df <- as.data.frame(left_join(df, flood, by = c("wcode", "c_code", "treatment")))

# Refactor survey data
df$flood33_bin <- ifelse(df$flood33 == 'Great extent' | df$flood33 == 'Somewhat', 1, 0)
df$flood234 <- ifelse(df$flood234 == 'Yes', 1, 0)
df$flood231 <- ifelse(df$flood231 == 'No ', 1, 0)


#### 2. Crude Associations ####

# T TESTS
t.test(df$flood33_bin, df$perc_flooded)
t.test(df$flood234, df$perc_flooded)
t.test(df$flood231, df$perc_flooded)
t.test(df$dd10r_min_m, df$perc_flooded)
## Lagged flooding
t.test(df$flood33_bin, df$Flood_1Lag)
t.test(df$flood234, df$Flood_1Lag)
t.test(df$flood231, df$Flood_1Lag) # not sig
t.test(df$dd10r_min_m, df$Flood_1Lag)

# GENERAL LINEAR MODELS
#### COPING STRATEGY
bme <- glm(flood231 ~ perc_flooded, family='binomial', data=df)
summary(bme) 

# Strong positive association: increases in flooding give an 8.18 odds in developing coping strategies 

####  BOROWING MONEY/FOOD
bme <- glm(flood234 ~ perc_flooded, family='binomial', data=df)
summary(bme) 

# Some evidence for positive association: increases in flooding  give 4.2 odds in borrowing money/food to coping strategies 

####  FLOOD PERCEPTION
bme <- glm(flood33_bin ~ perc_flooded, family='binomial', data=df)
summary(bme) 


#### 3.1. Compare ME Models for DD (no need to control for wcode because individual observations) ####

#### With most recent DD as outcome
x <- glm(dd10r_score_m ~ Flood_1Lag+treatment+wealth2_BL+dd10r_score_m_BL, data=df)
summary(x)

y1 <- glm(dd10r_score_m ~ flood33_bin+treatment+wealth2_BL+dd10r_score_m_BL, family='gaussian', data=df)
summary(y1)

y2 <- glm(dd10r_score_m ~ flood234+treatment+wealth2_BL+dd10r_score_m_BL, family='gaussian', data=df)
summary(y2)

y3 <- glm(dd10r_score_m ~ flood231+treatment+wealth2_BL+dd10r_score_m_BL, family='gaussian', data=df)
summary(y3)

#### With Endline as outcome

x <- glm(dd10r_score_m_EL ~ Flood_1Lag+treatment+wealth2_BL+dd10r_score_m_BL, data=df)
summary(x)

y1 <- glm(dd10r_score_m_EL ~ flood33_bin+treatment+wealth2_BL+dd10r_score_m_BL, family='gaussian', data=df)
summary(y1)

y2 <- glm(dd10r_score_m_EL ~ flood234+treatment+wealth2_BL+dd10r_score_m_BL, family='gaussian', data=df)
summary(y2)

y3 <- glm(dd10r_score_m_EL ~ flood231+treatment+wealth2_BL+dd10r_score_m_BL, family='gaussian', data=df)
summary(y3)


#### 3.2. Compare INLA models for DD (with space, not time because this is 1 time point) ####

RS_metric <- inla(dd10r_score_m ~ Flood_1Lag+treatment+wealth2_BL+dd10r_score_m_BL+
                    f(OBJECTID_1, model = 'besagproper', graph = W.adj.mat),
                  family ='gaussian', data = df, 
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
Res_RS_metric <- getINLA_res(RS_metric)
plotResults(Res_RS_metric)
Res_RS_metric

FAARM_metric <- inla(dd10r_score_m ~ flood234+treatment+wealth2_BL+dd10r_score_m_BL+
                       f(OBJECTID_1, model = 'besagproper', graph = W.adj.mat),
                     family ='gaussian', data = df, 
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute = TRUE))
Res_FAARM_metric <- getINLA_res(FAARM_metric)
plotResults(Res_FAARM_metric)
Res_FAARM_metric
