# FORMAT MAIN DATA FOR PROCESSING

#### Detatch packages & clear environment/plots
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
# rm(list = ls())

#### Load packages
library(INLA)
library(sf)
library(rgdal)
library(dplyr)
library(spdep)
library(ggplot2)
library(bayestestR)
library(lme4) # R4_Results

#### Ensure that RINLA can handle big datasets
inla.setOption("num.threads", 4)

#### IMPORTANT - set file path to data folder location
setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/- DD-Flooding TimeSeries - CO/4. Data/Final')

# write.csv(df, "C:/Users/ClaudiaOffner/Downloads/CLEAN", row.names=FALSE)


#### 1. Load & Select Data ####

# Load shape data (as spatial vector df)
# cluster_shp <- st_read(dsn="FAARM/96_Cluster_final.shp")
# cluster_shp <- st_transform(cluster_shp, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") # set projection
# cluster_shp <- cluster_shp %>% select( -c(OBJECTID, Shape_Leng, Shape_Le_1, AREA_M)) %>% rename(c_code = cluster_co)
cluster_shp <- st_read(dsn="FAARM/96_Cluster_final.shp")
cluster_shp <- st_transform(cluster_shp, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") # set projection
cluster_shp$centroid <- st_centroid(cluster_shp$geometry) # Get centroids
cluster_shp[c('lat', 'long')] <- do.call(rbind, st_geometry(cluster_shp$centroid)) %>% as_tibble() %>% setNames(c("lon","lat"))
cluster_shp <- cluster_shp %>% dplyr::select( -c(OBJECTID, Shape_Leng, Shape_Le_1, AREA_M, centroid)) %>% dplyr::rename(c_code = cluster_co)

# Load shape data (as spatial polygon df)
bound <- readOGR(dsn="FAARM/96_Cluster_final.shp")
bound <- spTransform(bound, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
bound@data <- bound@data %>% dplyr::select(c(cluster_co)) %>% dplyr::rename(c_code = cluster_co)
bound@data$c_code <- as.numeric(bound@data$c_code)


# Load FAARM data
# df <- read.csv(file='2_FAARM_GEE_df.csv', fileEncoding='UTF-8-BOM')
df <- read.csv(file='3_FloodMetrics.csv', fileEncoding='UTF-8-BOM')

# Select relevant variables
df <- df %>% select(c_code, wcode, year_season, year, season, season_DD, season_flood,
                    perc_flooded, Flood_1Lag,
                    flooded_diff, flooded_anom_w_lag, flooded_anom_lag, flooded_weight_lag,
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
                    terc_BL, wealth2_BL, dec2_BL, quint2_BL, terc2_BL)


#### 2. Cleaning ####

# Check outcome distribution over time
y <- df %>%
  group_by(year_season) %>%
  summarise(count=n(),
            diet=sum(!is.na(dd10r_score_m)),
            flood=sum(!is.na(perc_flooded)),
            DDperc=diet/count*100,
            FloodPerc=flood/count*100)

# REMOVE: year_season that have DD not between 30%-40% 
# NOTE: do not include 2019_05 because the cluster averages missing
ys_elim <- y$year_season[y$DDperc < 30 | y$DDperc > 40]
for (i in ys_elim) {
  df<-df[!(df$year_season==i),]
}


# Merge to get OBJCTID_1 for spatial effects
df <- as.data.frame(dplyr::left_join(df, cluster_shp, by = c('c_code')))
# Create numeric year-season_DD id for temporal effects
df$season_id <- as.numeric(factor(df$year_season))

# Get spatial weight matrices
adj.mat <- poly2nb(cluster_shp)
W.adj.mat <- nb2mat(adj.mat, style = "B", zero.policy=T) 

# Re-factor Season codes so Mar/Apr is used as the reference level (dry season)
df$season_flood <- factor(df$season_flood, levels=c("Mar/Apr","May/Jun", "Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb"))
# levels(df$season_DD) # Check levels

# Reset index
rownames(df) <- NULL

# Set generic priors
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

# Scale Environmental Variables
# https://stats.stackexchange.com/questions/407822/interpretation-of-standardized-z-score-rescaled-linear-model-coefficients
df$elev <- scale(df$elev) # NEED to standardize to make interpretative
df$temp_mean <- scale(df$temp_mean)
df$prec_mean <- scale(df$prec_mean)
df$evap_mean <- scale(df$evap_mean)
df$ndvi_mean <- scale(df$ndvi_mean)
df$temp_mean_lag <- scale(df$temp_mean_lag)
df$prec_mean_lag <- scale(df$prec_mean_lag)
df$evap_mean_lag <- scale(df$evap_mean_lag)
df$ndvi_mean_lag <- scale(df$ndvi_mean_lag)
df$flooded_anom_lag <- scale(df$flooded_anom_lag) 
df$flooded_weight_lag <- scale(df$flooded_weight_lag) 
df$flooded_anom_w_lag <- scale(df$flooded_anom_w_lag) 
# Scale main flood exp
sd(df$Flood_1Lag) # SD is approx 1% flood coverage
df$Flood_1Lag  <- (df$Flood_1Lag - mean(df$Flood_1Lag))/sd(df$Flood_1Lag)
# df$Flood_1Lag  <- (df$Flood_1Lag - min(df$Flood_1Lag))/(max(df$Flood_1Lag) - min(df$Flood_1Lag)) # normalise?


#### 3. Functions ####

# Function to round numeric columns of data frame
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# Function to read ME results in table
getME_res <- function(model){
  
  # Get p-values
  CI <- confint(model) # get CI
  gme_res <- data.frame(coef(summary(model)))
  gme_res$Lower_CI <- CI[3:(length(CI)/2),1]
  gme_res$Higher_CI <- CI[3:(length(CI)/2),2]
  gme_res$p.z <- round(2 * (1 - pnorm(abs(gme_res$t.value))), 6)
  gme_res <- gme_res %>% dplyr::select(Estimate, Std..Error,Lower_CI,Higher_CI,p.z)

  # Plot
  gme_plot = tibble::rownames_to_column(gme_res, "variables") # create column for intercepts
  gme_plot$index <- 1:nrow(gme_plot) # set index
  names(gme_plot) <- c("Variables","Mean","SD","Lower_CI","Upper_CI", 'P_Value',"Index")
  
  # Set significance col (for plotting)
  gme_plot$P_Value <- as.numeric(gme_plot$P_Value)
  gme_plot$Importance <- '0_None'
  gme_plot$Importance[gme_plot$P_Value <= 0.10 & gme_plot$P_Value >= 0.05] <- '1_Weak'
  gme_plot$Importance[gme_plot$P_Value <= 0.05] <- '2_Strong'

  return(round_df(gme_plot, 5))
  
}

# Function to read R-INLA results in table
getINLA_res <- function(model) {
  # NOTE: Maximum A Posteriori (MAP) is the probability distribution of te parameter haven seen the data
  # posterior probability distribution tells us the degree of belief we should have for any particular value of the parameter.
  # https://stats.stackexchange.com/questions/341553/what-is-bayesian-posterior-probability-and-how-is-it-different-to-just-using-a-p
  
  # Function to Format INLA results in table
  result <- rbind(cbind(model[["summary.fixed"]]))      # summary(model)$fixed[,-7])) #, cbind(summary(model)$hyperpar)
  result <- tibble::rownames_to_column(data.frame(result), "variables") # create column for intercepts
  result$index <- 1:nrow(result) # set index
  # Get posterior distribution & Calculate MAP
  # https://easystats.github.io/bayestestR/reference/p_map.html
  names <- model$names.fixed
  posterior = data.frame()
  for (i in names) {
    x <- data.frame(model$marginals.fixed[i])[,1]
    x <- cbind(i, p_map(x)[1])
    posterior <- rbind(posterior, x)
  }
  colnames(posterior) <- c('variables', 'MAP P')
  result <- as.data.frame(dplyr::left_join(as.data.frame(result), posterior, by = c('variables')))
  
  # Reorganize data frame
  colnames(result) <- c("Variables", "Mean","SD", "Lower_CI", "median", "Upper_CI", "mode", "kld", "Index", 'MAP_P') #, "Z_score", "P_Value"
  result <- as.data.frame(result) %>% 
    dplyr::select(Index, Variables, Mean, SD, Lower_CI, Upper_CI, MAP_P) #%>% #  Z_score, P_Value
    # mutate_if(is.numeric, round, 5)
  
  # Set significance col (for plotting)
  result$MAP_P <- as.numeric(result$MAP_P)
  result$Importance[result$Lower_CI > 0 & result$Upper_CI > 0] <- '2_Strong'
  result$Importance[result$Lower_CI < 0 & result$Upper_CI < 0] <- '2_Strong'
  result$Importance[result$Lower_CI <= 0 & result$Lower_CI >= -0.01 | result$Lower_CI >= 0 & result$Lower_CI <= 0.01] <- '1_Weak'
  result$Importance[result$Upper_CI <= 0 & result$Upper_CI >= -0.01 | result$Upper_CI >= 0 & result$Upper_CI <= 0.01] <- '1_Weak'
  result$Importance[result$Lower_CI < 0 & result$Upper_CI > 0 ] <- '0_None'

  # result$Importance <- '0_None'
  # result$Importance[result$MAP_P <= 0.10 & result$MAP_P >= 0.05] <- '1_Weak'
  # result$Importance[result$MAP_P <= 0.05] <- '2_Strong'

  return(result)
}

# Function to plot R-INLA results in forest plot; NOTE: use round_df function on df
plotResults <- function(res1) {
  
  res <- round_df(res1, 3)
  
  result_name <- deparse(substitute(res1))
  cols <- c("0_None" = "#dadada","1_Weak" = "#ff9530","2_Strong" = "#029921")

  # Plot Results
  ggplot(data=res, aes(y=Index, x=Mean, xmin=Lower_CI, xmax=Upper_CI)) +
    geom_point() + 
    geom_text(aes(label = Mean, colour = Importance),
              size = 3.5, nudge_x = 1.5, nudge_y = 0, check_overlap = FALSE) +
    scale_colour_manual(values = cols) + 
    theme(legend.position = "bottom") +
    geom_errorbarh(height=.3) +
    scale_y_continuous(breaks=1:nrow(res), labels=res$Variables) +
    labs(title=paste('Effect Size by Variable - ', result_name), x='Effect Size', y = 'Variable') +
    geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
    theme_minimal()
  
}

# Function to compare R-INLA models in table
selINLA_mod <- function(models){
  
  res <- data.frame() 
  
  for (mod in models) {
    # Get model name
    m <- get(mod)
    # print(deparse(substitute(m)))
    # Add row to data frame w/ matching col names
    x <- as.data.frame(cbind(summary(m)$mlik[2], summary(m)$dic$dic, summary(m)$waic$waic)) #sum(log(m$cpo$cpo))
    colnames(x) = c('MLIK', 'DIC', 'WAIC') # 'CPO'
    res <- rbind(res, x)
  }
  row.names(res) <- models
  res <- res[order(res$DIC), ]
  
  return(res)
  
}



#### Other ####

# # Check outcome distribution over women codes
# x <- df %>%
#   group_by(wcode) %>%
#   summarise(count=n(),
#             diet=sum(!is.na(dd10r_score_m)),
#             flood=sum(!is.na(Flood_1Lag)),
#             DDperc=diet/count*100,
#             FloodPerc=flood/count*100)
# 
# # REMOVE: wcodes that have less than 20% DD
# wc_elim <- x$wcode[x$DDperc < 20]
# for (i in wc_elim) {
#   df<-df[!(df$wcode==i),]
# }

# Cubic transformation for flooding (based on initial exploratory analysis)
# df$perc_flooded_cub <- df$perc_flooded^(1/3)
# df$Flood_1Lag_cub <- df$Flood_1Lag^(1/3)

# # Re-factor Season codes so Jul/Aug is used as the reference level (flooding season)
# df$season_DD <- factor(df$season_DD, levels=c("Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb", "Mar/Apr","May/Jun"))
# df$season_flood <- factor(df$season_flood, levels=c("Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb", "Mar/Apr","May/Jun"))
# # levels(df$season_DD)

# Create treatment subsets
# treat <- subset(df, treatment == 1)
# control <- subset(df, treatment == 0)

