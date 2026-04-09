### ------------------------------------------------------------------------ ### 
### Data cleaning and formatting
### ------------------------------------------------------------------------ ### 

#### MAIN CODE ####

# 1. Load data ####

# Load FAARM data
data <- read.csv(file=paste0(data_path, '3_FloodMetrics.csv'), fileEncoding='UTF-8-BOM')

# Load shape data (as spatial vector df)
cluster_shp <- st_read(dsn=paste0(data_path, "96_Cluster_final.shp"))
cluster_shp <- st_transform(cluster_shp, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") # set projection
cluster_shp$centroid <- st_centroid(cluster_shp$geometry) # Get centroids
cluster_shp[c('lon', 'lat')] <- do.call(rbind, st_geometry(cluster_shp$centroid)) %>% as_tibble() %>% setNames(c("long","lat"))
cluster_shp <- cluster_shp %>% dplyr::select( -c(OBJECTID, OBJECTID_1, Shape_Area, Shape_Leng, Shape_Le_1, AREA_M, centroid)) %>% dplyr::rename(c_code = cluster_co)

# Select relevant variables
df <- data %>% dplyr::select(c_code, wcode, year_season, year, season, season_DD, season_flood,
                      perc_flooded_c, Flood_1Lag, flooded_diff, flooded_anom_w_lag, flooded_anom_lag, flooded_weight_lag,
                      dd_elig, dd10r_starch, dd10r_legume, dd10r_nuts,	dd10r_dairy,	dd10r_flesh,	
                      dd10r_eggs,	dd10r_dglv,	dd10r_vita,	dd10r_othf,	dd10r_othv,	
                      dd10r_score, dd10r_min, dd10r_score_m, dd10r_min_m, wdiet_wt,
                      temp_mean, temp_min, temp_max, temp_mean_lag,
                      evap_mean, evap_min, evap_max, evap_mean_lag,
                      ndvi_mean, ndvi_min, ndvi_max, ndvi_mean_lag,
                      prec_mean, prec_min, prec_max, prec_mean_lag, elev,
                      treatment, ramadan, preg, dd10r_score_m_BL, dd10r_score_m_EL,
                      age_3_BL, g_2h_BL, fam_type_BL, dep_ratio, g_2h_BL, fam_type_BL,
                      wi_hl_BL, wi_al_BL, wi_land_BL, num_crops_BL, hfias_BL,
                      woman_edu_cat__BL, mobility_BL, support_BL,
                      communication_BL, decision_BL, know_score_BL,
                      dep_ratio, md_score_BL, wealth_BL, dec_BL, quint_BL,
                      terc_BL, wealth2_BL, dec2_BL, quint2_BL, terc2_BL, hh1hh_mem_EL)

# 2. Clean panels and set seasons ####

# Merge cat 4& 5 education
df$woman_edu_cat__BL[df$woman_edu_cat__BL==5] <- 4

# Create a copy of DF with BL values
df_BL <- df
df_BL$Baseline[df_BL$year_season %in% c('2015-1', '2015-2', '2015-3', '2015-4')] <- 'Baseline'
df_BL <- df_BL[df_BL$dd10r_othv != 88, ] # Remove 88 values from other veg

# REMOVE: BL & EL year_season  
# NOTE: do not include 2019_05 because the cluster averages missing
ys_elim <-  c("2015-1", "2015-2", "2015-3", "2015-4", 
              "2019-5", "2019-6", "2020-1", "2020-2", "2020-3")
for (i in ys_elim) {
  df<-df[!(df$year_season==i),]
}

# Create numeric year-season_DD id for temporal effects
df$season_id <- as.numeric(factor(df$year_season))

# Re-factor Season codes so Mar/Apr is used as the reference level (dry season)
df$season_flood[df$season_flood == "Sept/Oct"] <- "Sep/Oct"
df$season_DD[df$season_DD == "Sept/Oct"] <- "Sep/Oct"
df$season_flood <- factor(df$season_flood, levels=c("Jan/Feb", "Mar/Apr","May/Jun", "Jul/Aug", "Sep/Oct", "Nov/Dec"))

# Reset index
rownames(df) <- NULL

# 3. Categorical Exposure: Create Average Seasonal Flood Thresholds (MAIN ANALYSIS) ####

# Group by season and calculate the average for each season
(range(df$Flood_1Lag))
(season_means <- df %>% group_by(season_flood) 
  %>% summarize(
    flood_mean = mean(Flood_1Lag, na.rm = TRUE),
    flood_min = min(Flood_1Lag, na.rm = TRUE),
    flood_max = max(Flood_1Lag, na.rm = TRUE),
    flood_sd = sd(Flood_1Lag, na.rm = TRUE)))
(year_means <- df %>% group_by(year) %>% summarize(flood_means = mean(Flood_1Lag, na.rm = TRUE)))

# Merge seasonal averages back into the original dataframe
df <- df %>% left_join(season_means, by = "season_flood")

# Add a new variable indicating above, below, or at the seasonal average
df <- df %>%
  mutate(Flood_SThresh = case_when(
    Flood_1Lag >= (flood_mean+(flood_sd*2)) ~ 3, # Greater than 2 SD Above Mean
    (Flood_1Lag > flood_mean+flood_sd*1) & Flood_1Lag <= (flood_mean+flood_sd*2) ~ 2, # Within 2 SD Above Mean
    (Flood_1Lag > flood_mean) & Flood_1Lag <= (flood_mean+flood_sd*1) ~ 1, # Within 1 SD Above Mean
    (Flood_1Lag <= flood_mean) ~ 0)) # Below average/ no difference
flood_cat_levels <- c(0, 1, 2, 3)

# Check counts
df %>% count(Flood_SThresh)

# 4. Descriptive checks ####

# BASELINE DATA ONLY
# Get number of women per HFP at baseline
df_BL %>% 
  filter(Baseline=='Baseline') %>% 
  group_by(treatment) %>% 
  summarise(count = n()) %>%
  adorn_totals()

# SURVEILLANCE DATA ONLY
# NB: df2 is the complete case data used in model
df2 <- df[complete.cases(df), ]

# # !! Make sure only women included in surveillance are in baseline measure !!
# df_BL <- df_BL %>% filter(wcode %in% unique(df2$wcode))

# Number of obs: 20648, groups:  wcode, 2620; c_code, 96; season_id, 24
nrow(df2)
n_distinct(df2$wcode)
n_distinct(df2$c_code)

# Check number of women in each treatment group 
df2 %>% 
  distinct(wcode, treatment) %>% 
  group_by(treatment) %>% 
  summarise(n = n())

# Check outcome distribution of women, over time
df2 %>%
  group_by(year_season, treatment) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from = treatment, values_from = count)

# Check number of measures collected for each woman, over surveillance 
y <- df2 %>%
  group_by(wcode) %>%
  summarise(count=n())
count(y[y$count==0,]) # no surveillance
count(y[y$count>=8,]) # at least 8 measures

# Check number of flood categories, by cluster
df2 %>% 
  dplyr::select(c_code, year_season, Flood_SThresh) %>% 
  distinct() %>%
  group_by(Flood_SThresh) %>% 
  summarise(count = n())

#### EXPORT ####
save(df, df_BL, cluster_shp, season_means, flood_cat_levels,  file='main_data.RData')
