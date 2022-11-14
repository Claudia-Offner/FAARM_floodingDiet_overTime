#### R-INLA Results: Flooding & Dietary Diversity Over Time

# FORMAT MAIN DATA FOR PROCESSING
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

# Detatch packages & clear environment/plots
# rm(list = ls())

#### IMPORTANT - set file path to data folder location
# setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/- DD-Flooding TimeSeries - CO/4. Data/Final')

# Save data for reporting
write.csv(st_inla_res, "C:/Users/ClaudiaOffner/Downloads/st_inla.csv", row.names=FALSE)
write.csv(st_inla_Tres, "C:/Users/ClaudiaOffner/Downloads/st_inla_Tres.csv", row.names=FALSE)
write.csv(st_inla2_STRATres, "C:/Users/ClaudiaOffner/Downloads/st_inla2_STRAT.csv", row.names=FALSE)
write.csv(st_inla3_STRATres, "C:/Users/ClaudiaOffner/Downloads/st_inla3_STRAT.csv", row.names=FALSE)

# Test other lags?
# df <- df %>%
#   group_by(wcode) %>%
#   dplyr::mutate(Flood_6Lag = dplyr::lag(Flood_1Lag, n = 5, default = NA)) %>% # general time  lag of 6 by admin_code
#   as.data.frame()

#### 0. LMER (No seasonal Interactions) ####
# TEST: Do the coefficients between frequentist models add up precisely for each model?

# MODIFIED EFFECT: What is the seasonal impact of flooding on DD? (I think this makes most sense)
st_inla <- inla(dd10r_score_m ~ season_flood + Flood_1Lag + treatment + 
                         temp_mean + evap_mean + ndvi_mean + 
                         ramadan + preg + dd10r_score_m_BL +
                         g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                         communication_BL + quint2_BL + woman_edu_cat__BL + 
                         mobility_BL + decision_BL + know_score_BL + 
                         f(wcode, model = 'iid') + # control for women random effect on DD
                         # f(season_flood, model = 'iid') + # Control for seasonal effects on diet
                         # f(season_id, Flood_1Lag, model='ar1') + # Control for temporal autocorrelation
                         # f(OBJECTID_1, Flood_1Lag, model = 'besagproper', graph = W.adj.mat),
                         f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                           group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                           hyper = prec.prior),
                       family ='gaussian', data = df,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))
st_inla_res <- getINLA_res(st_inla)
st_inla_res$Variables <- c('(Intercept) Mar/Apr season', 'May/Jun season', 
                           'Jul/Aug season', 'Sep/Oct season', 'Nov/Dec season', 
                           'Jan/Feb season', 'Flooding Extent','Treatment',
                            'Temperature', 'Evapotranspiration', 'NDVI', 'Ramadan',
                            'Pregnancy', 'DD BL', 'Religion BL', 'Dependency Ratio BL',
                            'Land Owned BL', 'HFIAS BL', 'Communication BL',
                            'Wealth BL', 'Education BL', 'Mobility BL', 
                            'Decision BL', 'Knowledge BL')
plotResults(st_inla_res)
st_inla_res$OR <- exp(st_inla_res$Mean)


st_inla_T <- inla(dd10r_score_m ~ season_flood + Flood_1Lag*treatment + 
                  temp_mean + evap_mean + ndvi_mean + 
                  ramadan + preg + dd10r_score_m_BL +
                  g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                  communication_BL + quint2_BL + woman_edu_cat__BL + 
                  mobility_BL + decision_BL + know_score_BL + 
                  f(wcode, model = 'iid') + # control for women random effect on DD
                  # f(season_flood, model = 'iid') + # Control for seasonal effects on diet
                  # f(season_id, Flood_1Lag, model='ar1') + # Control for temporal autocorrelation
                  # f(OBJECTID_1, Flood_1Lag, model = 'besagproper', graph = W.adj.mat),
                  f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                    group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                    hyper = prec.prior),
                family ='gaussian', data = df,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute = TRUE))
st_inla_Tres <- getINLA_res(st_inla_T)
st_inla_Tres$Variables <- c('(Intercept) Mar/Apr season', 'May/Jun season', 
                            'Jul/Aug season', 'Sep/Oct season', 'Nov/Dec season', 
                            'Jan/Feb season', 'Flooding Extent (Control)','Treatment',
                            'Temperature', 'Evapotranspiration', 'NDVI', 'Ramadan',
                            'Pregnancy', 'DD BL', 'Religion BL', 'Dependency Ratio BL',
                            'Land Owned BL', 'HFIAS BL', 'Communication BL',
                            'Wealth BL', 'Education BL', 'Mobility BL', 
                            'Decision BL', 'Knowledge BL', 'Flooding Extent (Treatment)')
plotResults(st_inla_Tres)
st_inla_Tres$OR <- exp(st_inla_Tres$Mean)


# = Outputs between these models do not add up precisely (because they are different models)

#### 1. RINLA (2 way Interaction - by season) ####

# RANDOM SLOPE: What is the slope of flooding, by season, on DD - given its overall effect on DD?
# Not selected because seasons can be considered fixed effects so does not meet assumptions
# (they are not random, they have a degree of predictability)
# st_inla2_SLOPE <- inla(dd10r_score_m ~ season_flood + Flood_1Lag + treatment + # NEED to have flood in fixed effects
#                          temp_mean + evap_mean + ndvi_mean + 
#                          ramadan + preg + dd10r_score_m_BL +
#                          g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
#                          communication_BL + quint2_BL + woman_edu_cat__BL + 
#                          mobility_BL + decision_BL + know_score_BL + 
#                          f(season_flood, Flood_1Lag,  model = 'iid') + # Random slopes for flood by season on DD
#                          f(wcode, model = 'iid') + # control for women random effect on DD
#                          f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
#                          group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
#                          hyper = prec.prior),
#                        family ='gaussian', data = df,
#                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#                        control.predictor = list(compute = TRUE))
# st_inla2_SLOPEres <- getINLA_res(st_inla2_SLOPE)
# plotResults(st_inla2_SLOPEres)
# st_inla2_SLOPE[["summary.random"]][["season_flood"]]

# FULL INTERACTION: How does the impact of flooding on DD depend on season?
# Not selected because season is a nominal variable with no real 'zero' for reference. 
# Also does not make sense to include overall flood effects, if you have season (right?)

# st_inla2_INT <- inla(dd10r_score_m ~ season_flood*Flood_1Lag + treatment +
#                   temp_mean + evap_mean + ndvi_mean +
#                   ramadan + preg + dd10r_score_m_BL +
#                   g_2h_BL + dep_ratio + wi_land_BL + hfias_BL +
#                   communication_BL + quint2_BL + woman_edu_cat__BL +
#                   mobility_BL + decision_BL + know_score_BL +
#                   f(wcode, model = 'iid') + # control for women random effect on DD
#                   f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
#                     group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
#                     hyper = prec.prior),                
#                 family ='gaussian', data = df,
#                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#                 control.predictor = list(compute = TRUE))
# st_inla2_INTres <- getINLA_res(st_inla2_INT)
# plotResults(st_inla2_INTres)


# MODIFIED EFFECT: What is the seasonal impact of flooding on DD? (I think this makes most sense)
st_inla2_STRAT <- inla(dd10r_score_m ~ season_flood + season_flood:Flood_1Lag + treatment + 
                   temp_mean + evap_mean + ndvi_mean + 
                   ramadan + preg + dd10r_score_m_BL +
                   g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                   communication_BL + quint2_BL + woman_edu_cat__BL + 
                   mobility_BL + decision_BL + know_score_BL + 
                   f(wcode, model = 'iid') + # control for women random effect on DD
                   # f(season_flood, model = 'iid') + # Control for seasonal effects on diet
                   # f(season_id, Flood_1Lag, model='ar1') + # Control for temporal autocorrelation
                   # f(OBJECTID_1, Flood_1Lag, model = 'besagproper', graph = W.adj.mat),
                   f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                     group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                     hyper = prec.prior),
                 family ='gaussian', data = df,
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))
st_inla2_STRATres <- getINLA_res(st_inla2_STRAT)
st_inla2_STRATres$Variables <- c('(Intercept) Mar/Apr season', 'May/Jun season', 
                            'Jul/Aug season', 'Sep/Oct season', 'Nov/Dec season', 
                            'Jan/Feb season', 'Treatment',
                            'Temperature', 'Evapotranspiration', 'NDVI', 'Ramadan',
                            'Pregnancy', 'DD BL', 'Religion BL', 'Dependency Ratio BL',
                            'Land Owned BL', 'HFIAS BL', 'Communication BL',
                            'Wealth BL', 'Education BL', 'Mobility BL', 
                            'Decision BL', 'Knowledge BL', 'Mar/Apr season : Flooding Extent',
                            'May/Jun season : Flooding Extent', 'Jul/Aug season : Flooding Extent',
                            'Sep/Oct season : Flooding Extent', 'Nov/Dec season : Flooding Extent',
                            'Jan/Feb season : Flooding Extent')
plotResults(st_inla2_STRATres)
st_inla2_STRATres$OR <- exp(st_inla2_STRATres$Mean)
# st_inla2_STRAT[["summary.random"]][["OBJECTID_1"]]

# Compare
# comp <- selINLA_mod(list('st_inla2_SLOPE', 'st_inla2_STRAT', 'st_inla2_INT'))
# comp[order(comp$DIC), ]


# There is no massive/clear difference between any of these models - so we will rely on conceptual theory
# - Random Slope: rejected because season is not necessarily a random phenomena (it should be modeled as a fixed effect)
# - Interaction: rejected  because season is a nominal variable with no 'zero' reference - making interpretability near impossble
# - Modification: accepted (the results also make most sense)

#### 2. RINLA (3 way Interaction - by season & treatment) ####


# RANDOM SLOPE: What is the slope of flooding, by season and treatment, on DD 
# - given its overall effect on DD?
# # Create new variable
# df$treat_season <- paste(df$treatment, df$season_flood)
# st_inla3_SLOPE <- inla(dd10r_score_m ~ season_flood + Flood_1Lag + treatment + # NEED to have flood in fixed effects
#                          temp_mean + evap_mean + ndvi_mean + 
#                          ramadan + preg + dd10r_score_m_BL +
#                          g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
#                          communication_BL + quint2_BL + woman_edu_cat__BL + 
#                          mobility_BL + decision_BL + know_score_BL + 
#                          f(wcode, model = 'iid') + # control for women random effect on DD
#                          f(treat_season, Flood_1Lag,  model = 'iid') +
#                          f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
#                            group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
#                            hyper = prec.prior),
#                        family ='gaussian', data = df,
#                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#                        control.predictor = list(compute = TRUE))
# st_inla3_SLOPEres <- getINLA_res(st_inla3_SLOPE)
# plotResults(st_inla3_SLOPEres)
# st_inla3_SLOPE[["summary.random"]][["treat_season"]]

# FULL INTERACTION: How does the impact of flooding on DD depend on season and treatment?
# st_inla3_INT <- inla(dd10r_score_m ~ season_flood*Flood_1Lag*treatment +
#                   temp_mean + evap_mean + ndvi_mean +
#                   ramadan + preg + dd10r_score_m_BL +
#                   g_2h_BL + dep_ratio + wi_land_BL + hfias_BL +
#                   communication_BL + quint2_BL + woman_edu_cat__BL +
#                   mobility_BL + decision_BL + know_score_BL +
#                   f(wcode, model = 'iid') + # control for women random effect on DD
#                   f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
#                     group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
#                     hyper = prec.prior),
#                 family ='gaussian', data = df,
#                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#                 control.predictor = list(compute = TRUE))
# st_inla3_INTres <- getINLA_res(st_inla3_INT)
# plotResults(st_inla3_INTres)


# MODIFIED EFFECT: How does the seasonal impact of flooding depend on treatment?(I think this makes most sense)
st_inla3_STRAT <- inla(dd10r_score_m ~ season_flood + season_flood:Flood_1Lag*treatment +
                    temp_mean + evap_mean + ndvi_mean +
                    ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL + 
                    # f(season_flood, model = 'iid') + # Control for seasonal effects on diet
                    f(wcode, model = 'iid') + # control for women random effect on DD
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                      group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                      hyper = prec.prior),
                  family ='gaussian', data = df,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
st_inla3_STRATres <- getINLA_res(st_inla3_STRAT)
st_inla3_STRATres$Variables <- c('(Intercept) Mar/Apr season', 'May/Jun season', 
                                 'Jul/Aug season', 'Sep/Oct season', 'Nov/Dec season', 
                                 'Jan/Feb season', 'Treatment',
                                 'Temperature', 'Evapotranspiration', 'NDVI', 'Ramadan',
                                 'Pregnancy', 'DD BL', 'Religion BL', 'Dependency Ratio BL',
                                 'Land Owned BL', 'HFIAS BL', 'Communication BL',
                                 'Wealth BL', 'Education BL', 'Mobility BL', 
                                 'Decision BL', 'Knowledge BL', 'Mar/Apr season : Flooding Extent (Control)',
                                 'May/Jun season : Flooding Extent (Control)', 'Jul/Aug season : Flooding Extent (Control)',
                                 'Sep/Oct season : Flooding Extent (Control)', 'Nov/Dec season : Flooding Extent (Control)',
                                 'Jan/Feb season : Flooding Extent (Control)', 'Mar/Apr season : Flooding Extent (Treatment)',
                                 'May/Jun season : Flooding Extent (Treatment)', 'Jul/Aug season : Flooding Extent (Treatment)',
                                 'Sep/Oct season : Flooding Extent (Treatment)', 'Nov/Dec season : Flooding Extent (Treatment)',
                                 'Jan/Feb season : Flooding Extent (Treatment)')
plotResults(st_inla3_STRATres)
st_inla3_STRATres$OR <- exp(st_inla3_STRATres$Mean)


# Compare
# comp <- selINLA_mod(list('st_inla3_SLOPE', 'st_inla3_INT', 'st_inla3_STRAT'))
# comp[order(comp$DIC), ]

#### 3. VISUALS ####

# Plot posteriors

x <- data.frame(st_inla2_STRAT$marginals.fixed['season_floodNov/Dec:Flood_1Lag'])
y <- data.frame(st_inla2_STRAT$marginals.fixed['wi_land_BL']) #[,1]
plot(x)
plot(y)


#### COMPARE ####

# Random Slopes:
# Which time points had the best & worse DD given flooding?
# Which clusters had the best & worse DD given flooding?
st_inla2_s <- inla(dd10r_score_m ~ Flood_1Lag + treatment + season_flood +
                     temp_mean + evap_mean + ndvi_mean + 
                     ramadan + preg + dd10r_score_m_BL +
                     g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                     communication_BL + quint2_BL + woman_edu_cat__BL + 
                     mobility_BL + decision_BL + know_score_BL + 
                     f(wcode, model = 'iid') + # control for women random effect on DD
                     f(year_season, Flood_1Lag, model='ar1') + # Control for temporal autocorrelation
                     f(OBJECTID_1, Flood_1Lag, model = 'besagproper', graph = W.adj.mat),
                   family ='gaussian', data = df,
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                   control.predictor = list(compute = TRUE))
st_inla2_space<- getINLA_res(st_inla2_s)
plotResults(st_inla2_space)
st_inla2_s[["summary.random"]][["year_season"]]
st_inla2_s[["summary.random"]][["OBJECTID_1"]]

comp <- selINLA_mod(list('st_inla2_SLOPE', 'st_inla2_STRAT', 'st_inla2_INT', 
                         'st_inla3_SLOPE', 'st_inla3_INT', 'st_inla3_STRAT',
                         'st_inla2_s'))
comp[order(comp$DIC), ]
comp[order(comp$WAIC), ]
comp[order(comp$MLIK), ]


#### Quadratic effects ####
# RQ: What is the optimum magnitude of flooding in relation to DD for every season?

# Make quadratic variable
df$Flood_1Lag2 <- df$Flood_1Lag^2

# Full Interaction
quad_inla <- inla(dd10r_score_m ~ Flood_1Lag*season_flood + Flood_1Lag2*season_flood + 
                    temp_mean + evap_mean + ndvi_mean +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL + 
                    f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                group = season_id, control.group = list(model = "ar1"),
                                                hyper = prec.prior),
                  family ='gaussian', data = df,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
quad_inla_res <- getINLA_res(quad_inla)
plotResults(quad_inla_res)


# Stratified Effect (I think this would be preferred over a full interaction...)
quad_inla_STRAT <- inla(dd10r_score_m ~ season_flood:Flood_1Lag + season_flood:Flood_1Lag2 + season_flood + 
                          temp_mean + evap_mean + ndvi_mean +
                          treatment + ramadan + preg + dd10r_score_m_BL +
                          g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                          communication_BL + quint2_BL + woman_edu_cat__BL + 
                          mobility_BL + decision_BL + know_score_BL + 
                          f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                      group = season_id, control.group = list(model = "ar1"),
                                                      hyper = prec.prior),
                        family ='gaussian', data = df,
                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                        control.predictor = list(compute = TRUE))
quad_inla_res_STRAT <- getINLA_res(quad_inla_STRAT)
plotResults(quad_inla_res_STRAT)


# How would we plot these? Not sure how to make sense of them, most seem unimportant...



################################################################################


#### PLOT Predicted values
# df$st_inla_DD <- st_inla$summary.fitted.values$mean
# out_time <-cast(df, c_code~year, mean, value = "st_inla_DD", na.rm=TRUE) # in years
# df2 <- merge(bound, out_time, by='c_code') 
# df2 <- subset(df2, select=-c(lat, long))
# df3 <- df2
# df3@data$mean <- rowMeans(data.matrix(df3@data[,2:ncol(df2@data)]))
# map <- tm_basemap(leaflet::providers$Esri.WorldTopoMap) + 
#   tm_shape(df3) +
#   tm_polygons('mean', title="Dietary Diversity Level")+
#   tm_compass(position=c("left","top"))+
#   tm_scale_bar()
# lf <- tmap_leaflet(map)
# map

# Recalculated Effects
# x <- st_inla_res %>% filter(grepl('Flood', Variables))
# x[-(1),3] <- x[1,3] + x[-(1),3] # get main effects
# x$Lower_CI <- x$Mean - x$SD # get CI
# x$Upper_CI <- x$Mean + x$SD # get CI
# y <- st_inla_res %>% filter(!grepl('Flood', Variables)) %>% rbind(x) %>% arrange(Index) # merge back
# plotResults(y)
# Recode MAP_P: y[y$Lower_CI  > 1 & y$Upper_CI  < -1) | (y$Lower_CI  < 1 & y$Upper_CI  > -1] 



#### *** Plot posteriors over Space and time *** 

# Predicted Values
df$pred <- basic_inla$summary.fitted.values$mean


# GROUPING
ggplot() +
  geom_point(data=df, aes(x=dd10r_score_m, y=Flood_1Lag, colour=season_flood)) +
  # ylim(0, 0.03) +
  facet_grid(season_flood ~ .)


# Separated Spatial-Temporal Effects
inla_GST1 <- inla(dd10r_score_m ~ Flood_1Lag*season_flood + 
                    temp_mean + evap_mean + ndvi_mean +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL +
                    f(wcode, model = 'iid') + 
                    f(season_id, model='ar1', hyper=prec.prior) + 
                    f(OBJECTID_1, model='besagproper', graph=W.adj.mat, hyper=prec.prior),
                  family ='gaussian', data = df, 
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
Res_inla_GST1 <- getINLA_res(inla_GST1)
plotResults(Res_inla_GST1)

# Get posteriors for each cluster
x <- inla_GST1[["summary.random"]][["OBJECTID_1"]]
x[order(x$mean), ]

# Get posteriors for each time point
y <- inla_GST1[["summary.random"]][["season_id"]]
y[order(y$mean), ]

# plotResults(Res2[c(1:15),])
# x <- Res2[-c(16:29),]
# rownames(x) <- NULL
# x$Index <- 1:nrow(x) # set index
# 
# plotResults(x)
# Res2$ODDS <- exp(Res2$Mean)
# 
# Res2 %>% 
#   mutate_if(is.numeric, round, digits=6)
# 
# # Check residual autocorrelation
# res <- df$dd10r_score_m - mod3$summary.fitted.values[,"mean"] #residuals
# n <- length(res) 
# mod <- lm(res[-n] ~ res[-1]) 
# summary(mod)

library(reshape2)
library(tmap)

# Transform marginals and compute posterior mean marginals: List of `marginals.fitted.values`from inla model
tmarg <- function(marginals) {
  post.means <- mclapply(marginals, function (marg) {
    # Transform post. marginals
    aux <- inla.tmarginal(exp, marg)
    # Compute posterior mean
    inla.emarginal(function(x) x, aux)
  })
  
  return(as.vector(unlist(post.means)))
}

tic('Adding posterior means')
# Add posterior means to the SpatialPolygonsDataFrame
df$marg <- tmarg(mod3$marginals.fitted.values)
toc() # Approx 22 minutes

# Load shape data (as spatial polygon df)
bound <- readOGR(dsn="FAARM/96_Cluster_final.shp")
bound <- spTransform(bound, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
bound@data <- bound@data %>% dplyr::select(c(cluster_co)) %>% dplyr::rename(c_code = cluster_co)
bound@data$c_code <- as.numeric(bound@data$c_code)

# Average by cluster
agr <- df %>%
  group_by(c_code, year) %>%
  summarise_at(vars(marg), list(name = mean), na.rm = TRUE)

# Merge with Spatial Polygon Data frame
bound@data <- merge(bound@data, agr, by='c_code') 

# Map over time
tmap_mode("plot")
tm_shape(bound) +
  tm_polygons("name", title="Marginal posterior means") + # Mean WDDS
  tm_compass(position=c("left","top"))+
  tm_scale_bar()+
  tm_facets(by="year")




#### *** Model validation *** 
# 
# This is the model we will focus on.
# There is no need to run it again, but we
# do.


# Get fitted values and residuals
Fit6.a <- mod1$summary.fitted.values[,"mean"]
E6     <- df$dd10r_score_m - Fit6.a #residuals


#Homogeneity
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Fit6.a, y = E6)
abline(h = 0, v = 0)

#Normality
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
hist(E6, breaks = 25)

#Independence due to model misfit
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = df$Flood_1Lag_cub, y = E6)
abline(h = 0)

plot(x=df$season_flood, y = E6)
abline(h = 0)

boxplot(E6 ~ Flood_1Lag_cub, data = df)
abline(h = 0)


# Spatial patterns in the residuals?
# Option 1: Plot the residuls vs spatial
#           locations. Look for patterns.
# Option 2: Make a variogram of the residuals

# Option 1:
MyCex <- 3 * abs(E6) / max(E6) + 0.5
Sign <- as.numeric(E6 >=0) + 1
MyPch <- c(1, 16)[Sign]
xyplot(Northing ~ Easting,
       data = iph,
       cex = MyCex,
       pch = MyPch,
       col = 1,
       aspect = "iso",
       xlab = list(label = "Easting", cex = 1.5),
       ylab = list(label = "Northing", cex = 1.5)
)
# Better as before!!!!


# Option 2: Variogram
MyData <- data.frame(E6 = E6, 
                     Xkm = iph$Easting.km, 
                     Ykm = iph$Northing.km)
coordinates(MyData) <- c("Xkm", "Ykm")

V1 <- variogram(E6 ~ Xkm + Ykm , 
                data = MyData, 
                cressie = TRUE,
                cutoff = 100)
plot(V1, 
     xlab = list(label = "Distance", cex = 1.5),
     ylab = list(label = "Cressie's semivariance", cex = 1.5),
     col = 1, pch = 16, smooth = TRUE)     
# No spatial correlation. Odd pattern though




#### is it a data set thing? (i.e. compiled dataframe vs. raw) = YES (partially - works better with raw scores)
#### Is it a reference thing?
#### Is it a data cleaning thing?
#### Does it have to do with the flood data?
#### Is it a power thing? Does removing rows/time points help?



prec.prior <- list(prec = list(prior = "logtnormal", param = c(1, 0.01))) # the probability of ?? being greater than 1 is equal to 0.01
test <- list(mean = list(Flood_1Lag = -100,
                         season_DD = 0,
                         season_flood = 0),
             prec = list(Flood_1Lag = 0.001,
                         season_DD = 0.001,
                         season_flood = 0.001),
             mean.intercept = 0,
             prec.intercept = 0)

mod2 <- inla(dd10r_score_m ~ Flood_1Lag + season_DD + season_flood:Flood_1Lag_cub
             + treatment + ramadan # + preg + wdiet_wt 
             + dd10r_score_m_BL + g_2h_BL + dep_ratio + wi_al_BL + num_crops_BL + woman_edu_cat__BL
             + mobility_BL + communication_BL + decision_BL + know_score_BL + wealth2_BL
             + f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                           group = season_id, control.group = list(model = "ar1"),
                                           hyper = prec.prior),
             family ='gaussian', data = df,
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE), #link=1
             control.fixed = test)
Res2 <- getINLA_res(mod2)
plotResults(Res2)
Res2$ODDS <- exp(Res2$Mean)
Res2

mod1$summary.fitted.values$mean

plot(df$Flood_1Lag_cub, df$dd10r_score_m)

names(inla.models()$prior)
names(inla.models()$latent)
names(inla.models()$likelihood)


plot(mod2, plot.prior = TRUE)


mod2$summary.random

control.fixed = list(mean = list(Flood_1Lag = 0,
                                 season_DD = 0,
                                 season_flood = 0),
                     prec = list(Flood_1Lag = 0.001,
                                 season_DD = 0.001,
                                 season_flood = 0.001),
                     mean.intercept = 0,
                     prec.intercept = 0)


summary(df)


cor.test(df$prec, df$season)



