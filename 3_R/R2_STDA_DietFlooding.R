# Identify Model Requirements: Test associations & auto correlations

# # To run this code, set the work directory to folder containing the provided files & data
# setwd('C:/Users/offne/Documents/GitHub/RINLA_FloodDiet_Modelling/')
# 
# # Load Formatted RS data file
# library(here)
# suppressWarnings(source(here::here('0_DataFormatting.R')))

# Load packages
# install.packages('MuMIn')
library(reshape)
library(ggcorrplot)
library(tmap)
library(lme4)
library(tseries)
library(ggplot2)

#### 0. NON-ST Analysis ####

hist(df$dd10r_score_m, col="#ff5050", )

df$season_flood2 <- factor(df$season_flood, levels=c("Jan/Feb", "Mar/Apr","May/Jun", "Jul/Aug", "Sept/Oct", "Nov/Dec"))
ggplot(df,aes(x=season_flood2,y=perc_flooded))+
  geom_boxplot(fill="#6699FF",outlier.color="black")+
  labs(x = "Season", y = "Flooding (Cluster %)", title = "Central Tendancy of Flooding by Season")

# One Way ANOVA for baseline
# base<-df[df$year_season=='2015-5',]
# aov(dd10r_score_m ~ 
#       treatment + ramadan + preg + dd10r_score_m_BL +
#       g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
#       communication_BL + quint2_BL + woman_edu_cat__BL + 
#       mobility_BL + decision_BL + know_score_BL + (1|wcode), base)


#### ** Spatial-temporal Formatting ** ####

# Moran's Local I Function
moranCluster <- function(shape, W, var, alpha=0.05, p.adjust.method="bonferroni")
{
  Ii <- localmoran(var, W)
  shape$Ii <- Ii[,"Ii"]
  shape$Iip <- Ii[,"Pr(z != E(Ii))"]
  shape$sig <- shape$Iip<alpha
  # Scale the data to obtain low and high values
  shape$scaled <- scale(var) # high low values at location i
  shape$lag_scaled <- lag.listw(W, shape$scaled) # high low values at neighbours j
  shape$lag_cat <- factor(ifelse(shape$scaled>0 & shape$lag_scaled>0, "High-High",
                                 ifelse(shape$scaled>0 & shape$lag_scaled<0, "High-Low",
                                        ifelse(shape$scaled<0 & shape$lag_scaled<0, "Low-Low",
                                               ifelse(shape$scaled<0 & shape$lag_scaled<0, "Low-High", "Equivalent")))))
  shape$sig_cluster <- as.character(shape$lag_cat)
  shape$sig_cluster[!shape$sig] <- "Non-significant"
  shape$sig_cluster <- as.factor(shape$sig_cluster)
  results <- data.frame(Ii=shape$Ii, pvalue=shape$Iip, type=shape$lag_cat, sig=shape$sig_cluster)
  
  return(list(results=results))
}

# Use centroids to get 3 nearest neighbor list
centroid <- st_centroid(cluster_shp$geometry)
knn <- knearneigh(centroid, k=3, longlat = NULL, use_kd_tree=TRUE)
knn.mat <- knn2nb(knn, row.names = NULL, sym = FALSE)
W <- nb2listw(knn.mat)

#### ** Matrix Transformations **

# Cast to wide form (not long form) - Flooding
exp_time <-cast(df, c_code~year, mean, value = "perc_flooded") # in years
s <- subset(df, select = c("c_code", "lat", "long"))
s <- s[!duplicated(s), ] # remove duplicates
exp_time <- merge(s, exp_time, by='c_code') 
exp_matrix <-data.matrix(exp_time[,4:ncol(exp_time)]) # set to matrix
# exp_time$year_season <- paste0(exp_time$year_season, '-01') # Add Day to  year_season
# names(exp_time)[names(exp_time)=="year_season"] <- "time" # rename year_season
exp_time_ys<-cast(df, c_code~year_season, mean, value = "perc_flooded") # in years
s <- subset(df, select = c("c_code", "lat", "long"))
s <- s[!duplicated(s), ] # remove duplicates
exp_time_ys <- merge(s, exp_time_ys, by='c_code') 
exp_matrix_ys <-data.matrix(exp_time_ys[,4:ncol(exp_time_ys)]) # set to matrix


# Cast to wide form (not long form) - Diet
out_time <-cast(df, c_code~year, mean, value = "dd10r_score_m", na.rm=TRUE) # in years
s <- subset(df, select = c("c_code", "lat", "long"))
s <- s[!duplicated(s), ] # remove duplicates
out_time <- merge(s, out_time, by='c_code') 
out_matrix <-data.matrix(out_time[,4:ncol(out_time)]) # set to matrix
# out_time$year_season <- paste0(out_time$year_season, '-01') # Add Day to  year_season
# names(out_time)[names(out_time)=="year_season"] <- "time" # rename year_season
out_time_ys<-cast(df, c_code~year_season, mean, value = "dd10r_score_m", na.rm=TRUE) # in years
s <- subset(df, select = c("c_code", "lat", "long"))
s <- s[!duplicated(s), ] # remove duplicates
out_time_ys <- merge(s, out_time_ys, by='c_code') 
out_matrix_ys <-data.matrix(out_time_ys[,4:ncol(out_time_ys)]) # set to matrix

#### 1. FLOOD: Spatial Analysis ####
tmap_mode("plot")

#### Spatial distribution over annual average (FLOOD)
df2 <- merge(bound, exp_time, by='c_code') 
df2 <- subset(df2, select=-c(lat, long))
df3 <- df2
# df2@data <- melt(df2@data,
#                  id.vars = "c_code",
#                  variable = "year")
# 
# black_style  <- structure(list(basemaps=list("Esri.WorldGrayCanvas","OpenStreetMap","Esri.WorldTopoMap" )))
# tmap_options(black_style)
# 
# 
# map <- tm_shape(df2) +
#   tm_polygons("value", title="Flood Severity Level (%)") + #palette=c("#ff5050", "#ffe5e5")
#   tm_compass(position=c("left","top"))+
#   tm_scale_bar()+
#   tm_facets("year")
# tmap_save(map, filename="map_a.png")


df3@data$mean <- rowMeans(data.matrix(df3@data[,2:ncol(df2@data)]))
map <- tm_basemap(leaflet::providers$Esri.WorldTopoMap) + 
  tm_shape(df3) +
  tm_polygons('mean', title="Flood Severity Level")+ # palette=c("#ff5050")
  tm_compass(position=c("left","top"))+
  tm_scale_bar()
lf <- tmap_leaflet(map)
map
# mapshot(lf, file = "world_map.png")

#https://stackoverflow.com/questions/52208470/saving-a-tmap-with-a-basemap-as-an-image


##### QUANTIFY SPATIAL DEPENDENCIES
# Get data
spaceExpMean <- rowMeans(exp_matrix)
# Moran's Global I
moran.test(x=spaceExpMean, listw=W)
# the null hypothesis that the level of autocorrelation we observe could have been due to chance (random)
# = We reject the null hypothesis --> so there is a strong spatial dependency
# Local Morans I
expClusters <- moranCluster(bound, W=W, var=spaceExpMean)$results
bound$Ii_expCluster <- expClusters$sig
tm_shape(bound) + tm_polygons(col="Ii_expCluster", palette=c("#ff5050", "#6699FF"), title = "Moran's Local I - Flooding")


#### 1. DIET: Spatial Analysis ####

#### Spatial distribution over annual average (DIET)
df2 <- merge(bound, out_time, by='c_code') 
df2 <- subset(df2, select=-c(lat, long))
df3 <- df2

# df2@data <- melt(df2@data,
#                  id.vars = "c_code",
#                  variable = "year",
#                  value = "value")
# # tm_shape(df2) +
#   tm_polygons("level", title="Dietary Diversity Level")+ #, palette=c("#ff5050", "#ffe5e5")) +
#   tm_compass(position=c("left","top"))+
#   tm_scale_bar()+
#   tm_facets("year")

df3@data$mean <- rowMeans(data.matrix(df3@data[,2:ncol(df2@data)]))
map <- tm_basemap(leaflet::providers$Esri.WorldTopoMap) + 
  tm_shape(df3) +
  tm_polygons('mean', title="Dietary Diversity Level")+
  tm_compass(position=c("left","top"))+
  tm_scale_bar()
lf <- tmap_leaflet(map)
map
##### QUANTIFY SPATIAL DEPENDENCIES
# Get data
spaceOutMean <- rowMeans(out_matrix)
# Moran's Global I
moran.test(x=spaceOutMean, listw=W)
# the null hypothesis that the level of autocorrelation we observe could have been due to chance (random)
# = We reject the null hypothesis --> so there is a strong spatial dependency
# Local Morans I
outClusters <- moranCluster(bound, W=W, var=spaceOutMean)$results
bound$Ii_outCluster <- outClusters$sig
tm_shape(bound) + tm_polygons(col="Ii_outCluster", palette=c("#ff5050", "#6699FF"), title = "Moran's Local I Clusters - Diet")


#### ** TREAT: Spatial Analysis ####
# tmap_mode('view')
# x <- df %>% distinct(c_code, .keep_all = TRUE)
# df2 <- merge(bound, x, by='c_code') 
# df2 <- subset(df2, select=c(c_code, treatment))
# 
# df2$level <- "Control"
# df2$level[which(df2$treatment == 1)] <- "Treatment"
# 
# tm_shape(df2) +
#   tm_polygons("level", title="Dietary Diversity Level", palette=c("#ff5050", "#ffe5e5")) +
#   tm_compass(position=c("left","top"))
#   tm_scale_bar()
# 
# 
# 
#### 2. FLOOD: Temporal Analysis ####
timeMean <- colMeans(exp_matrix_ys)
  
# Look at the mean few_ipc across country over time (in years)
d<- as.data.frame(colMeans(exp_matrix))

ggplot(data = d, aes(y=colMeans(exp_matrix), x = seq(2015, 2019, by=1)))+
  geom_line(color = "#6699FF", size = 2)+
  scale_x_continuous(breaks=seq(2010, 2019, by=1))+
  labs(x = "Year", y = "Flooding (Cluster %)", title = "Yearly Flood Severity Average")

# Look at the mean few_ipc across country over time (in seasons)
d<- as.data.frame(colMeans(exp_matrix_ys))
a <- 1:24
ggplot(data = d, aes(y=colMeans(exp_matrix_ys), x = a))+
  geom_line(color = "#ff5050", size = 2)+
  # scale_x_continuous(breaks=a[seq(1, length(a),12)], labels = c("2010_01", "2011_01", "2012_01", "2013_01", "2014_01","2015_01", "2016_01", "2017_01", "2018_01", "2019_01"))+
  labs(x = "Season", y = "Average Flood %", title = "Seasonal Time Series of Flood Severity")

# Order heat map by c_code & WCODE % flooded means to examine effects
ggplot(df) +
  geom_tile(aes(x=year_season, y=reorder(c_code, wcode, mean, order=TRUE), fill = perc_flooded)) +
  scale_fill_gradient(name = "% Flooded",low = "blue", high = "red") +
  labs(x = "Year-Season", y = "Cluster Code")

# Order heat map by c_code & LAT % flooded means to examine effects
ggplot(df) +
  geom_tile(aes(x=year_season, y=reorder(c_code, lat, mean, order=TRUE), fill = perc_flooded)) +
  scale_fill_gradient(name = "% Flooded",low = "blue", high = "red") +
  labs(x = "Year-Season", y = "Cluster Code")

# QUANTIFY TEMPORAL DEPENDENCIES

# Augmented Dickey-Fuller test 
adf.test(colMeans(exp_matrix_ys))
# We fail to reject the null hypothesis, so the series is non-stationary (needs difference)
adf.test(diff(colMeans(exp_matrix_ys), lag=6))

# Check Temporal Dependency

bacf <- acf(timeMean, lag.max=50, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept=0.20, linetype='dashed', col = 'blue')+
  geom_hline(yintercept=-0.20, linetype='dashed', col = 'blue')+
  scale_y_continuous(breaks=seq(-1.0, 1.0, by=0.2))+
  labs(y="ACF", x="Lag", title="ACF - Flooding")

bacf <- acf(diff(timeMean), lag.max=50, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept=0.20, linetype='dashed', col = 'blue')+
  geom_hline(yintercept=-0.20, linetype='dashed', col = 'blue')+
  scale_y_continuous(breaks=seq(-1.0, 1.0, by=0.2))+
  labs(y="ACF", x="Lag", title="ACF Differenced - Flooding")

bacf <- pacf(timeMean, lag.max=50, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept=0.20, linetype='dashed', col = 'blue')+
  geom_hline(yintercept=-0.20, linetype='dashed', col = 'blue')+
  scale_y_continuous(breaks=seq(-1.0, 1.0, by=0.2))+
  labs(y="PACF", x="Lag", title="PACF - Flooding")

bacf <- pacf(diff(timeMean), lag.max=50, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept=0.20, linetype='dashed', col = 'blue')+
  geom_hline(yintercept=-0.20, linetype='dashed', col = 'blue')+
  scale_y_continuous(breaks=seq(-1.0, 1.0, by=0.2))+
  labs(y="PACF", x="Lag", title="PACF Differenced - Flooding")


#### 2. DIET: Temporal Analysis ####

# Look at the mean few_ipc across country over time (in years)
d<- as.data.frame(colMeans(out_matrix))
ggplot(data = d, aes(y=colMeans(out_matrix), x = seq(2015, 2019, by=1)))+
  geom_line(color = "#6699FF", size = 2)+
  scale_x_continuous(breaks=seq(2010, 2019, by=1))+
  labs(x = "Year", y = "Flood (Cluster %)", title = "Yearly Dietary Diversity Average")

# Look at the mean few_ipc across country over time (in seasons)
d<- as.data.frame(colMeans(out_matrix_ys))
a <- 1:24
ggplot(data = d, aes(y=colMeans(out_matrix_ys), x = a))+
  geom_line(color = "#ff5050", size = 2)+
  # scale_x_continuous(breaks=a[seq(1, length(a),12)], labels = c("2010_01", "2011_01", "2012_01", "2013_01", "2014_01","2015_01", "2016_01", "2017_01", "2018_01", "2019_01"))+
  labs(x = "Season", y = "Average Flood %", title = "Seasonal Time Series of Dietary Diversity")

# Order heat map by c_code & WCODE % flooded means to examine effects
ggplot(df) +
  geom_tile(aes(x=year_season, y=reorder(c_code, wcode, mean, order=TRUE), fill = dd10r_score_m)) +
  scale_fill_gradient(name = "% Flooded",low = "blue", high = "red") +
  labs(x = "Year-Season", y = "Cluster Code")

# Order heat map by c_code & LAT % flooded means to examine effects
ggplot(df) +
  geom_tile(aes(x=year_season, y=reorder(c_code, lat, mean, order=TRUE), fill = dd10r_score_m)) +
  scale_fill_gradient(name = "% Flooded",low = "blue", high = "red") +
  labs(x = "Year-Season", y = "Cluster Code")

# QUANTIFY TEMPORAL DEPENDENCIES

# Check Temporal Dependency
timeMean <- colMeans(out_matrix_ys[,1:24])

bacf <- acf(timeMean, lag.max=50, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept=0.20, linetype='dashed', col = 'blue')+
  geom_hline(yintercept=-0.20, linetype='dashed', col = 'blue')+
  scale_y_continuous(breaks=seq(-1.0, 1.0, by=0.2))+
  labs(y="ACF", x="Lag", title="ACF - Dietary Diversity")

bacf <- acf(diff(timeMean), lag.max=50, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept=0.20, linetype='dashed', col = 'blue')+
  geom_hline(yintercept=-0.20, linetype='dashed', col = 'blue')+
  scale_y_continuous(breaks=seq(-1.0, 1.0, by=0.2))+
  labs(y="ACF", x="Lag", title="ACF Differenced - Dietary Diversity")

bacf <- pacf(timeMean, lag.max=50, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept=0.20, linetype='dashed', col = 'blue')+
  geom_hline(yintercept=-0.20, linetype='dashed', col = 'blue')+
  scale_y_continuous(breaks=seq(-1.0, 1.0, by=0.2))+
  labs(y="PACF", x="Lag", title="PACF - Dietary Diversity")

bacf <- pacf(diff(timeMean), lag.max=50, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(yintercept=0.20, linetype='dashed', col = 'blue')+
  geom_hline(yintercept=-0.20, linetype='dashed', col = 'blue')+
  scale_y_continuous(breaks=seq(-1.0, 1.0, by=0.2))+
  labs(y="PACF", x="Lag", title="PACF Differenced - Dietary Diversity")


#### ** Conclusions ####

# - Year & season matter
# - A lag is required to interpret the causal relation between flood and diet
# - Spatial autocorrelation matters


# elev + temp_mean + evap_mean + ndvi_mean + prec_mean +
#   treatment + ramadan + preg + dd10r_score_m_BL +
#   g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
#   communication_BL + quint2_BL + woman_edu_cat__BL + 
#   mobility_BL + decision_BL + know_score_BL +
#### 3. Crude Association (LMER - control for subject code) ####

# Raw
crude_gme <- lmer(dd10r_score_m ~ perc_flooded+(1|wcode),
                  data=df)
crude_res <- getME_res(crude_gme)
plotResults(crude_res)
crude_res

# Lag
crude_gmeL <- lmer(dd10r_score_m ~ Flood_1Lag+(1|wcode),
                   data=df)
crude_resL <- getME_res(crude_gmeL)
plotResults(crude_resL)
crude_resL

#### Lag & interaction 
crude_gmeLI <- lmer(dd10r_score_m ~ Flood_1Lag*season_flood+(1|wcode),
                  data=df)
crude_resLI <- getME_res(crude_gmeLI)
plotResults(crude_resLI)

# Check residual autocorrelation
# res <- summary(crude_gme)$residuals
# n <- length(res) 
# mod2 <- lm(res[-n] ~ res[-1]) 
# summary(mod2)

#### 4. Step-wise Selection of variables (A-priori) ####

# CHECK how correlated Environmental variables are with Flooding exposure
# - Decided to only include mean values for backward selection
corr_gme <- lmer(perc_flooded ~ 
                   temp_mean + temp_min + temp_max + 
                   evap_mean + evap_min + evap_max +
                   ndvi_mean + ndvi_min + ndvi_max +
                   prec_mean + prec_min + prec_max + elev + (1|wcode),
                 data=df)
corr_res <- getME_res(corr_gme)
plotResults(corr_res)


# Outcome !!! no season included !!!
step_test <-  glm(dd10r_score_m ~ 
                    temp_mean + ndvi_mean + evap_mean + prec_mean + elev +
                    treatment + ramadan + preg + dd10r_score_m_BL + 
                    age_3_BL + g_2h_BL + fam_type_BL +
                    wi_hl_BL + wi_al_BL + wi_land_BL + num_crops_BL + hfias_BL +
                    woman_edu_cat__BL + mobility_BL + support_BL + 
                    communication_BL + decision_BL + know_score_BL +
                    dep_ratio + md_score_BL + wealth_BL + dec_BL + quint_BL + 
                    terc_BL + wealth2_BL + dec2_BL + quint2_BL + terc2_BL,
                  family = gaussian, data = df)

# Double Selection
step(step_test, direction = 'backward')

# Exposure !!! - no season included !!!
step_test <-  glm(Flood_1Lag ~ 
                    temp_mean + ndvi_mean + evap_mean + prec_mean + elev +
                    treatment + ramadan + preg + dd10r_score_m_BL + 
                    age_3_BL + g_2h_BL + fam_type_BL +
                    wi_hl_BL + wi_al_BL + wi_land_BL + num_crops_BL + hfias_BL +
                    woman_edu_cat__BL + mobility_BL + support_BL + 
                    communication_BL + decision_BL + know_score_BL +
                    dep_ratio + md_score_BL + wealth_BL + dec_BL + quint_BL + 
                    terc_BL + wealth2_BL + dec2_BL + quint2_BL + terc2_BL,
                  family = gaussian, data = df)

# Double Selection
step(step_test, direction = 'backward')


# Selected Formula: OUT
# Call:  glm(formula = dd10r_score_m ~ temp_mean + evap_mean + ndvi_mean + 
#              prec_mean + elev + treatment + ramadan + preg + dd10r_score_m_BL + 
#              dd10r_score_m_EL* + g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL + 
#              wi_al_BL* + wi_land_BL + hfias_BL + woman_edu_cat__BL* + mobility_BL* + 
#              communication_BL + decision_BL* + know_score_BL* + wealth_BL + 
#              terc_BL* + wealth2_BL + quint2_BL + terc2_BL*, family = gaussian, 
#            data = df)

# Selected Formula: EXP
# Call:  glm(formula = Flood_1Lag ~ temp_mean + evap_mean + ndvi_mean + 
#              prec_mean + elev + treatment + ramadan + preg + dd10r_score_m_BL + 
#              age_3_BL* + g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL + 
#              wi_land_BL + hfias_BL + support_BL + communication_BL + md_score_BL + 
#              wealth_BL + dec_BL* + wealth2_BL + dec2_BL* + quint2_BL*, family = gaussian, 
#            data = df)

# PRELIMINARY SELECTION (1-4: BOTH; 5: Outcome; 6: Exposure) 
# - decided to exclude overlapping wealth & EL variables to minimize co linearity in the model
# - Precipitation was also removed because it is to closely related to water inundation
# temp_mean + evap_mean + ndvi_mean + elev +
# treatment + ramadan + preg + dd10r_score_m_BL + 
# g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL + 
# wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
# dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL + 
# age_3_BL

#### Preliminary Dag (LMER - mixed effects w/ confounders)

conf_gme <- lmer(dd10r_score_m ~ Flood_1Lag*season_flood + 
                   elev + temp_mean + evap_mean + ndvi_mean +
                   treatment + ramadan + preg + dd10r_score_m_BL +
                   g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                   wi_land_BL + hfias_BL + communication_BL + quint2_BL +
                   wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                   age_3_BL + (1|wcode),
                 data=df)
conf_res <- getME_res(conf_gme)
plotResults(conf_res)
conf_res

# FINAL FORMULA
conf_gme2 <- lmer(dd10r_score_m ~ Flood_1Lag*season_flood + 
                    temp_mean + evap_mean + ndvi_mean +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL + (1|wcode),
                  data=df)
conf_res2 <- getME_res(conf_gme2)
plotResults(conf_res2)
conf_res2

# Check residual autocorrelation
res <- summary(conf_gme)$residuals
n <- length(res) 
mod2 <- lm(res[-n] ~ res[-1]) 
summary(mod2)

# Check model autocorrelation 
acf(residuals(conf_gme)) # Indicates a trend & series not stationary (violates assumption) - difference
acf(diff(residuals(conf_gme))) # Removes trend - although there is one point before 5 months (so introduce a lag variable)

# Not a huge amount of autocorrelation once confounders & wcode are accounted for
### BUT the DW Test or the Linear Regression test are not robust to anomalies in the data (GLOBAL MEASURE). 
### If you have Pulses, Seasonal Pulses , Level Shifts or Local Time Trends these tests are useless 
### as these untreated components inflate the variance of the errors thus downward biasing the tests 
### causing you ( as you have found out ) to incorrectly accept the null hypothesis of no auto-correlation. 
# https://stats.stackexchange.com/questions/14914/how-to-test-the-autocorrelation-of-the-residuals 

# = Autocorrelation analysis required

# TEST for stationary, using augmented Dickey-Fuller tests
