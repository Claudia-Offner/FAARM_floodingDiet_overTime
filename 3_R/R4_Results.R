#### R-INLA Results: Flooding & Dietary Diversity Over Time

# FORMAT MAIN DATA FOR PROCESSING
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

# Detatch packages & clear environment/plots
# rm(list = ls())

#### IMPORTANT - set file path to data folder location
# setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/- DD-Flooding TimeSeries - CO/4. Data/Final')

#### 0. LMER (2 way Interaction - no spatial-temporal autocorrelation) ####
# TEST: Do the coefficients between frequentist models add up precisely for each model?

# Full Interaction: How does the impact of flooding on DD depend on each season?
basic_gme <- lmer(dd10r_score_m ~ season_flood*Flood_1Lag + 
                    temp_mean + evap_mean + ndvi_mean +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL + (1|wcode),
                  data=df)
basic_gme_res <- getME_res(basic_gme)
plotResults(basic_gme_res)


# Stratified Effect: What is the impact of flooding on DD for each season?
basic_gme_STRAT <- lmer(dd10r_score_m ~ season_flood:Flood_1Lag + season_flood + # Control for season
                    temp_mean + evap_mean + ndvi_mean +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL + (1|wcode),
                  data=df)
basic_gme_res_STRAT <- getME_res(basic_gme_STRAT)
plotResults(basic_gme_res_STRAT)


# Recalculated Effects
x <- basic_gme_res %>% filter(grepl('Flood', Variables))
x[-(1),2] <- x[1,2] + x[-(1),2] # get main effects
x$Lower_CI <- x$Mean - x$SD # get CI
x$Upper_CI <- x$Mean + x$SD # get CI
y <- basic_gme_res %>% filter(!grepl('Flood', Variables)) %>% rbind(x) %>% arrange(Index) # merge back
plotResults(y)

# = Outputs between these models do not add up precisely (because they are different models)

#### 1. RINLA (2 way Interaction - with spatial-temporal autocorrelation) ####

# Full Interaction: How does the impact of flooding on DD depend on each season?
st_inla <- inla(dd10r_score_m ~ season_flood*Flood_1Lag + 
                  temp_mean + evap_mean + ndvi_mean + 
                  treatment + ramadan + preg + dd10r_score_m_BL +
                  g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                  communication_BL + quint2_BL + woman_edu_cat__BL + 
                  mobility_BL + decision_BL + know_score_BL + 
                  # f(season_flood, model = 'iid') + # RANDOM EFFECT FOR SEASON?
                  f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                              group = season_id, control.group = list(model = "ar1"),
                                              hyper = prec.prior),
                family ='gaussian', data = df,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute = TRUE))
st_inla_res <- getINLA_res(st_inla)
plotResults(st_inla_res)


# Stratified Effect: What is the impact of flooding on DD for each season? (I think this makes most sense)
st_inla2 <- inla(dd10r_score_m ~ season_flood:Flood_1Lag + season_flood + # Control for season
                   temp_mean + evap_mean + ndvi_mean + 
                   treatment + ramadan + preg + dd10r_score_m_BL +
                   g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                   communication_BL + quint2_BL + woman_edu_cat__BL + 
                   mobility_BL + decision_BL + know_score_BL + 
                   # f(season_flood, model = 'iid') + # OR RANDOM EFFECT FOR SEASON?
                   f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                               group = season_id, control.group = list(model = "ar1"),
                                               hyper = prec.prior),
                 family ='gaussian', data = df,
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))
st_inla_STRAT <- getINLA_res(st_inla2)
plotResults(st_inla_STRAT)


# Recalculated Effects
x <- st_inla_res %>% filter(grepl('Flood', Variables))
x[-(1),3] <- x[1,3] + x[-(1),3] # get main effects
x$Lower_CI <- x$Mean - x$SD # get CI
x$Upper_CI <- x$Mean + x$SD # get CI
y <- st_inla_res %>% filter(!grepl('Flood', Variables)) %>% rbind(x) %>% arrange(Index) # merge back
plotResults(y)
# Recode MAP_P: y[y$Lower_CI  > 1 & y$Upper_CI  < -1) | (y$Lower_CI  < 1 & y$Upper_CI  > -1] 


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



#### 2. RINLA (3 way Interaction - by treatment) ####

# Full Interaction: How does the impact of flooding on DD depend on each season and treatment?
st_3_inla <- inla(dd10r_score_m ~ season_flood*Flood_1Lag*treatment + 
                  temp_mean + evap_mean + ndvi_mean +
                  ramadan + preg + dd10r_score_m_BL +
                  g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                  communication_BL + quint2_BL + woman_edu_cat__BL + 
                  mobility_BL + decision_BL + know_score_BL + 
                  # f(season_flood, model = 'iid') + # RANDOM EFFECT FOR SEASON?
                  f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                              group = season_id, control.group = list(model = "ar1"),
                                              hyper = prec.prior),
                family ='gaussian', data = df,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute = TRUE))
st_3_inla_res <- getINLA_res(st_3_inla)
plotResults(st_3_inla_res)

# Stratified Effect: What is the impact of flooding on DD for each season and every treatment? (I think this makes most sense)
st_3_inla_STRAT <- inla(dd10r_score_m ~ season_flood:Flood_1Lag*treatment + season_flood + # Control for season
                    temp_mean + evap_mean + ndvi_mean +
                    ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL + 
                    # f(season_flood, model = 'iid') + # or RANDOM EFFECT FOR SEASON?
                    f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                group = season_id, control.group = list(model = "ar1"),
                                                hyper = prec.prior),
                  family ='gaussian', data = df,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
Res_st_3_inla_STRAT <- getINLA_res(st_3_inla_STRAT)
plotResults(Res_st_3_inla_STRAT)

# Recalculated Effects
x <- st_3_inla_res %>% filter(grepl('Flood', Variables))
x[-(1),3] <- x[1,3] + x[-(1),3] # get main effects
x$Lower_CI <- x$Mean - x$SD # get CI
x$Upper_CI <- x$Mean + x$SD # get CI
y <- st_3_inla_res %>% filter(!grepl('Flood', Variables)) %>% rbind(x) %>% arrange(Index) # merge back
plotResults(y)

#### 3. RINLA (Quadratic effect) ####
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


#### COMPARE ####
# Compare Metrics
# comp <- selINLA_mod(list('st_inla', 'st_3_inla', 'quad_inla'))
# comp[order(comp$MLIK), ] # The higher the better fit 
# comp[order(comp$DIC), ] # The lower the better fit 
# comp[order(comp$WAIC), ] # The lower the better fit 


################################################################################

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
