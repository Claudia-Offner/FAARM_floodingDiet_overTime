################################################################################
#### FORMAT MAIN DATA FOR PROCESSING #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/2. Data/')

## Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)

#### IMPORTANT - Run R0_Data_formatting first

# PACKAGES ####

# Required packages
packages <- c('openxlsx', 'dplyr', 'zoo', 'tidyr', 'reshape2', 'spdep', 'nlme', 
              'lme4', 'ggplot2', 'emmeans')

library <- 'C:/Users/offne/Documents/R/win-library/FAARM/' # set path

# Create new library for project
# (.libPaths()) # Check Library Paths
# dir.create(library, recursive = TRUE) # create
# (.libPaths(library)) # Set library directory

#### Install packages to library
for (p in packages){
  install.packages(p, lib = library) # devtools::install_github(username/repository)
}

#### Load packages from library
for (p in packages){
  library(get(p), lib.loc = library)
}

#### OTHER SETTINGS
## Detatch packages & clear environment/plots
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
# rm(list = ls())
## Get citations
# options(citation.bibtex.max=999)
# citation('nlme')
## Check which packages were used
# sessionInfo() 

# FUNCTIONS ####

# Function to round numeric columns of data frame
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}

# Function to calculate odds ratios from Logistic Regressions
getORs <- function(df, columns) {
  
  # OR TRANSFORMATION
  for (c in columns) {
    df[[c]] <- exp(df[[c]])
  }
  
  return(df)
}

# Function to calculate probabilities from Logistic Regressions
getPROBS <- function(df, columns) {
  
  # OR TRANSFORMATION
  for (c in columns) {
    df[[c]] <- exp(df[[c]])/ (1 + exp(df[[c]]))
  }
  
  return(df)
}

# Function to read GLMER results in table
getGLMM <- function(glmm_model, var=0, rep=0) {
  
  # Get glmm_results
  glmm_res <- as.data.frame(summary(glmm_model)$coef)
  
  # Calcualte CIs
  colnames(glmm_res) <- c("Estimate", "Std.Error", "z-value", "p-value") # Rename columns
  glmm_res$LowerCI <- round(glmm_res[["Estimate"]] - 1.96 * glmm_res[["Std.Error"]], 4)
  glmm_res$UpperCI <- round(glmm_res[["Estimate"]] + 1.96 * glmm_res[["Std.Error"]], 4)
  glmm_res$Variable <- rownames(glmm_res)
  rownames(glmm_res) <- NULL
  glmm_res <- glmm_res %>% # Re-order columns
    select(Variable, Estimate, Std.Error, LowerCI, UpperCI, `p-value`)
  
  # Plot
  glmm_res$index <- 1:nrow(glmm_res) # set indeglmm_res
  names(glmm_res) <- c("Variables","Estimate","SD","Lower_CI","Upper_CI", 'P_Value',"Index")
  
  # Set significance col (for plotting)
  glmm_res$P_Value <- as.numeric(glmm_res$P_Value)
  glmm_res$Importance <- '0_None'
  glmm_res$Importance[glmm_res$P_Value <= 0.10 & glmm_res$P_Value >= 0.05] <- '1_Weak'
  glmm_res$Importance[glmm_res$P_Value <= 0.05] <- '2_Strong'
  
  # Set variable names
  if (var != 0) {
    glmm_res$Variables <- var
  } 
  
  # Set representation of results
  if (rep=='OR'){
    # Convert to ORs
    glmm_res <- getORs(glmm_res, c('Estimate', 'Lower_CI','Upper_CI'))
    return(round_df(glmm_res, 5))
  } else if (rep=='PROBS') {
    glmm_res <- getPROBS(glmm_res, c('Estimate', 'Lower_CI','Upper_CI'))
    return(round_df(glmm_res, 5))
  } else {
    return(round_df(glmm_res, 5))
  }
  
}

# Function to read LME results in table
getLME <- function(lme_model, var=0){
  
  # Use the summary function to get a summary of the model
  summary_table <- summary(lme_model)
  
  # Extract relevant information (coefficient estimates, standard errors, p-values, and confidence intervals)
  lme_res <- round(as.data.frame(summary_table$tTable[, c("Value", "Std.Error", "DF", "t-value", "p-value")]), 4)
  lme_res$Variable <- rownames(lme_res)
  rownames(lme_res) <- NULL
  colnames(lme_res) <- c("Estimate", "Std.Error", "df", "t-Value", "p-value", "Variable") # Rename columns
  
  # Calculate confidence intervals
  lme_res$LowerCI <- round(lme_res$Estimate - qt(0.975, summary_table$tTable[, "DF"]) * lme_res$Std.Error, 4)
  lme_res$UpperCI <- round(lme_res$Estimate + qt(0.975, summary_table$tTable[, "DF"]) * lme_res$Std.Error, 4)
  lme_res <- lme_res %>% # Re-order columns
    select(Variable, Estimate, Std.Error, LowerCI, UpperCI, `p-value`)
  
  # Plot
  lme_res$index <- 1:nrow(lme_res) # set index
  names(lme_res) <- c("Variables","Estimate","SD","Lower_CI","Upper_CI", 'P_Value',"Index")
  
  # Set significance col (for plotting)
  lme_res$P_Value <- as.numeric(lme_res$P_Value)
  lme_res$Importance <- '0_None'
  lme_res$Importance[lme_res$P_Value <= 0.10 & lme_res$P_Value >= 0.05] <- '1_Weak'
  lme_res$Importance[lme_res$P_Value <= 0.05] <- '2_Strong'
  
  # Get results
  if(var != 0) {
    
    lme_res$Variables <- var
    
  }
  
  return(round_df(lme_res, 5))
  
}

# Function for formatting contrast and trend tables for forest plotting function (plotResults)
formatRES <- function(df) {
  for (c in names(df)) {
    
    est <- c('estimate', '1.trend', 'emmean', 'prob')
    lci <- c('asymp.LCL', 'lower.CL')
    uci <- c('asymp.UCL', 'upper.CL')
    if (c %in% est) {
      colnames(df)[colnames(df) %in% est] <- "Estimate"
    } 
    if (c %in% lci) {
      colnames(df)[colnames(df) %in% lci] <- "Lower_CI"
    } 
    if (c %in% uci) {
      colnames(df)[colnames(df) %in% uci] <- "Upper_CI"
    } 
    if (all('p.value' == c)) {
      colnames(df)[colnames(df) %in% 'p.value'] <- "P_Value"
    } 
    if (all('SE' == c)) {
      colnames(df)[colnames(df) %in% 'SE'] <- "SD"
    } 
  }
  
  # Plot
  df$Index <- 1:nrow(df) # set index
  df$Variables <- apply(df[, 1:3], 1, paste, collapse = ":") # set variables
  df <- df[, c("Variables", "Estimate", "SD", "Lower_CI", "Upper_CI", "P_Value", "Index")]
  
  # Set significance col (for plotting)
  df$P_Value <- as.numeric(df$P_Value)
  df$Importance <- '0_None'
  df$Importance[df$P_Value <= 0.10 & df$P_Value >= 0.05] <- '1_Weak'
  df$Importance[df$P_Value <= 0.05] <- '2_Strong'
  
  return(df)
}

# Function to plot results in forest plot; NOTE: use round_df function on df
plotResults <- function(res1, x=0, name=0) {
  
  res <- round_df(res1, 3)
  
  if (name != 0) {
    result_name <- name
  } else {
    result_name <- deparse(substitute(res1))
  }
  
  cols <- c("0_None" = "#dadada","1_Weak" = "#ff9530","2_Strong" = "#029921")
  
  # Plot Results
  ggplot(data=res, aes(y=Index, x=Estimate, xmin=Lower_CI, xmax=Upper_CI)) +
    geom_point() +
    geom_text(aes(label = Estimate, colour = Importance),
              size = 3.5, nudge_x = 1.5, nudge_y = 0, check_overlap = FALSE) +
    scale_colour_manual(values = cols) +
    theme(legend.position = "bottom") +
    geom_errorbarh(height=.3) +
    scale_y_continuous(breaks=1:nrow(res), labels=res$Variables) +
    labs(title=paste('Effect Size by Variable - ', result_name), x='Effect Size', y = 'Variable') +
    geom_vline(xintercept=x, color='black', linetype='dashed', alpha=.5) +
    theme_minimal()
  
}

# Variable Names for 3-way formulas
var <- c('(Intercept) Jan/Feb season', 
         'Flood Extent: Jan/Feb season', 
         'Mar/Apr season', 'May/Jun season', 'Jul/Aug season',
         'Sep/Oct season', 'Nov/Dec season', 
         'Jan/Feb season : Treatment',
         'WDDS (BL)', 'Ramadan', 'Religion', 'Wealth',
         'Flood Extent : Mar/Apr season',
         'Flood Extent : May/Jun season',
         'Flood Extent : Jul/Aug season',
         'Flood Extent : Sep/Oct season',
         'Flood Extent : Nov/Dec season',
         'Mar/Apr season : Treatment',
         'May/Jun season : Treatment',
         'Jul/Aug season : Treatment',
         'Sep/Oct season : Treatment',
         'Nov/Dec season : Treatment',
         'Flood Extent : Jan/Feb season : Treatment',
         'Flood Extent : Mar/Apr season : Treatment',
         'Flood Extent : May/Jun season : Treatment',
         'Flood Extent : Jul/Aug season : Treatment',
         'Flood Extent : Sep/Oct season : Treatment',
         'Flood Extent : Nov/Dec season : Treatment')

# 2. Load & Select Data ####

# Load shape data (as spatial vector df)
cluster_shp <- st_read(dsn="96_Cluster_final.shp")
cluster_shp <- st_transform(cluster_shp, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") # set projection
cluster_shp$centroid <- st_centroid(cluster_shp$geometry) # Get centroids
cluster_shp[c('lon', 'lat')] <- do.call(rbind, st_geometry(cluster_shp$centroid)) %>% as_tibble() %>% setNames(c("long","lat"))
cluster_shp <- cluster_shp %>% dplyr::select( -c(OBJECTID, OBJECTID_1, Shape_Area, Shape_Leng, Shape_Le_1, AREA_M, centroid)) %>% dplyr::rename(c_code = cluster_co)

# Load FAARM data
data <- read.csv(file='3_FloodMetrics.csv', fileEncoding='UTF-8-BOM')

# Select relevant variables
df <- data %>% select(c_code, wcode, year_season, year, season, season_DD, season_flood,
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

# 3. Cleaning ####

# Check outcome distribution over time
y <- df %>%
  group_by(year_season, treatment) %>%
  summarise(count=n(),
            diet=sum(!is.na(dd10r_score_m)),
            flood=sum(!is.na(perc_flooded_c)),
            DDperc=diet/count*100,
            FloodPerc=flood/count*100)

# Create a copy of DF with BL values
df_BL <- df
df_BL$Baseline[df_BL$year_season %in% c('2015-1', '2015-2', '2015-3', '2015-4')] <- 'Baseline'

# REMOVE: BL & EL year_season  
# NOTE: do not include 2019_05 because the cluster averages missing
ys_elim <-  c("2015-1", "2015-2", "2015-3", "2015-4", 
              "2019-5", "2019-6", "2020-1", "2020-2", "2020-3") # y$year_season[y$DDperc < 29 | y$DDperc > 39]
for (i in ys_elim) {
  df<-df[!(df$year_season==i),]
}

# Create numeric year-season_DD id for temporal effects
df$season_id <- as.numeric(factor(df$year_season))

# Re-factor Season codes so Mar/Apr is used as the reference level (dry season)
df$season_flood[df$season_flood == "Sept/Oct"] <- "Sep/Oct"
df$season_DD[df$season_DD == "Sept/Oct"] <- "Sep/Oct"
df$season_flood <- factor(df$season_flood, levels=c("Jan/Feb", "Mar/Apr","May/Jun", "Jul/Aug", "Sep/Oct", "Nov/Dec"))
# levels(df$season_DD) # Check levels

# Reset index
rownames(df) <- NULL



# 4. Categorical Exposure: Create Average Seasonal Flood Thresholds ####

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
df <- df %>%
  left_join(season_means, by = "season_flood")

# Add a new variable indicating above, below, or at the seasonal average
df <- df %>%
  mutate(Flood_SThresh = case_when(
    Flood_1Lag >= (flood_mean+(flood_sd*2)) ~ 3, # Greater than 2 SD Above Mean
    (Flood_1Lag > flood_mean+flood_sd*1) & Flood_1Lag <= (flood_mean+flood_sd*2) ~ 2, # Within 2 SD Above Mean
    (Flood_1Lag > flood_mean) & Flood_1Lag <= (flood_mean+flood_sd*1) ~ 1, # Within 1 SD Above Mean
    (Flood_1Lag <= flood_mean) ~ 0)) # Below average
    # Flood_1Lag == 0 ~ 0)) # No difference

# Check counts
df %>% count(Flood_SThresh)

# Set levels
levels <- c(0, 1, 2, 3)

df$Flood_1Lag <- df$Flood_SThresh

# levels <- c('No difference', '1 SD above mean', '2 SD above mean', '> 2 SD above mean')
# df$Flood_SThresh <- factor(df$Flood_SThresh, levels=levels)

# # 5. Continuous Exposure: Center & Scale Flooding ####
# 
# ## Scale flood exposure to improve interpret ability of the model
# ## https://stats.stackexchange.com/questions/407822/interpretation-of-standardized-z-score-rescaled-linear-model-coefficients
# ## https://towardsai.net/p/data-science/how-when-and-why-should-you-normalize-standardize-rescale-your-data-3f083def38ff
# 
# ## NB: The variable needs to be centered because polynomial interactions
# ##     introduce multi-colinearity that need to minimized.
# ##     The flood variable is not normal and also needs to be scale so that we
# ##     interpret results by 1% increases, instead of 100% increases.
# ##     So we center and divide by 0.01 (NOT SD because not Gaussian), so we can
# ##     interpret our model as 1% increases.
# 
# df$Flood_1Lag_norm <- df$Flood_1Lag
# # Center and scale the variable
# mean_value <- mean(df$Flood_1Lag, na.rm = TRUE)
# df$Flood_1Lag  <- (df$Flood_1Lag - mean_value)/0.01 # Check what it means when you standardize
# 
# # Identify levels for predictor (based on center & scale)
# levels <- c((0 - mean_value)/0.01,
#             (0.01 - mean_value)/0.01, 
#             (0.05 - mean_value)/0.01,
#             (0.1 - mean_value)/0.01,
#             (0.2 - mean_value)/0.01)
# 
