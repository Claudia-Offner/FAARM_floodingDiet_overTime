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

# setwd('G:/My Drive/1. Work/13. LSHTM/2. Research/FAARM/2. Data')
# data <- read.csv(file='JW_All_DD_women_Apr-10-2023.csv')
# # data <- read.csv(file='JW_womensDD_long.csv', fileEncoding='UTF-8-BOM')
# # data <- read.csv(file='TS_FSN-MH-Data_220314.csv', fileEncoding='UTF-8-BOM')
# length(unique(data$wcode))


# 1. Descriptive Statistics ####

hist(df$Flood_1Lag)
plot(df$dd10r_score, df$Flood_1Lag)

# CHECK THE NUMBER OF OBSERVATIONS & RANDOM EFFECTS GROUPINGS IN MODEL ####
# Number of obs: 21561, groups:  wcode, 2620; c_code, 96; season_id, 24
outcome <- 'dd10r_score_m'
fixed <- ' ~ Flood_1Lag*season_flood*treatment + dd10r_score_m_BL + ramadan + quint2_BL'
lme3_mod <- lme(
  fixed = as.formula(paste0(outcome, fixed)),
  random = list(wcode = (~1|season_id), c_code = (~1)), # Random effects
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  na.action = na.omit,  # Handle missing data using na.omit
  data = df
)
summary(lme3_mod)

# CONFIRM NUMBER OF WOMEN WITH DATA COLELCTED OVER SURVEILLANCE

# Number of women in surveillance sample (no BL or EL) 
length(unique(df$wcode)) # 2701

# Get amount of data collected for each woman
y <- df %>%
  group_by(wcode) %>%
  summarise(count=n(),
            treat=sum(treatment),
            diet=sum(!is.na(dd10r_score_m)),
            flood=sum(!is.na(perc_flooded_c)),
            DDperc=diet/count*100,
            FloodPerc=flood/count*100)

# Number of women who had no observations over surveillance (by trial arm)
count(y[y$diet==0,]) # 81 - excluded by model
y %>%
  filter(diet == 0) %>%
  group_by(treat) %>%
  summarise(count = n())
# Control 44; Treat 37

# # Number of women with BL and EL values only (by trial arm) - should be 4 but getting 3??
# ys_elim <-  c("2015-1", "2015-2", "2015-3", "2015-4", 
#               "2019-5", "2019-6", "2020-1", "2020-2", "2020-3") 
# y <- data %>%
#   filter(year_season %in% ys_elim) %>%
#   group_by(wcode, treatment) %>%
#   summarise(
#     count = n(),
#     diet = sum(!is.na(dd10r_score_m)),
#     flood = sum(!is.na(perc_flooded_c)),
#     DDperc = diet / count * 100,
#     FloodPerc = flood / count * 100
#   )
# y %>%
#   filter(diet == 0) %>%
#   group_by(treatment) %>%
#   summarise(count = n())


# Number of women who had at least 8 panels of data over surveillance (can get IDs too)
count(y[y$diet>=8,]) # 1894

# Model includes women with at least 1 panel of data collected over surveillance
# 2701-81 = 2620

# Model observations are a result of pregnancy surveillance
table(df$preg)
# 16588 + 4973 = 21561

# Number of observations per arm
df %>%
  filter(dd10r_score_m >= 0) %>%
  group_by(treatment) %>%
  summarize(count = n())
# 10684 + 10877 = 21561

# Number of women included per arm
df %>%
  filter(dd10r_score_m >= 1) %>%
  distinct(wcode, .keep_all = TRUE) %>%
  group_by(treatment) %>%
  summarize(count = n())
# Control: 1320; Treatment: 1300

# Number of clusters included per arm
df %>%
  filter(dd10r_score_m >= 1) %>%
  distinct(c_code, .keep_all = TRUE) %>%
  group_by(treatment) %>%
  summarize(count = n())
# Control: 48; Treatment: 48

# #### Number of partial data
# df %>%
#   filter(dd10r_score_m < 8) %>%
#   distinct(wcode, .keep_all = TRUE) %>%
#   group_by(treatment) %>%
#   summarize(count = n())
# # Control: 1; Treatment: None
# 
# # 2. Tests for temporal trends and dependencies
# # Descriptive proportions (binary outcomes)
# agg <-aggregate(cbind(dd10r_min_m) ~ perc_flooded_c+treatment+season_flood, sum, data = df)
# agg$prop <- agg$dd10r_min_m/2620;agg
# 
# # 3. Tests for spatial distributions and dependencies

