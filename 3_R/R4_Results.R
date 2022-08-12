#### R-INLA Results: Flooding & Dietary Diversity Over Time

# FORMAT MAIN DATA FOR PROCESSING
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

# Detatch packages & clear environment/plots
# rm(list = ls())

library(tictoc)
library(INLA)
library(sf)
library(rgdal)
library(dplyr)
library(spdep)
library(ggplot2)
library(bayestestR)
library(lme4)
require("ggrepel")

#### IMPORTANT - set file path to data folder location
setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/- DD-Flooding TimeSeries - CO/4. Data/Final')

# Note: The intercept term in a regression table tells us the average expected 
# value for the response variable when all of the predictor variables are equal 
# to zero. However, the regression coefficient for the intercept is only 
# meaningful if it’s reasonable that all of the predictor variables in the model 
# can actually be equal to zero.
# https://www.statology.org/how-to-interpret-regression-coefficients/

# set intercept to 0 to see Mar/Apr (interpret outputs as DD average rather than effect size?)


#### A. Crude Association (LMER - control for subject code) ####

crude_gme <- lmer(dd10r_score_m ~ season_flood*Flood_1Lag + (1|wcode),
                  data=df)
crude_res <- getME_res(crude_gme)
plotResults(crude_res)

#### B. Preliminary (LMER - with confounders) ####

basic_gme <- lmer(dd10r_score_m ~ season_flood*Flood_1Lag + 
                   temp_mean + evap_mean + ndvi_mean +
                   treatment + ramadan + preg + dd10r_score_m_BL +
                   g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                   communication_BL + quint2_BL + woman_edu_cat__BL + 
                   mobility_BL + decision_BL + know_score_BL + (1|wcode),
                 data=df)
basic_gme_res <- getME_res(basic_gme)
plotResults(basic_gme_res)

#### 1. RINLA (2 way Interaction - no spatial-temporal autocorrelation)####

basic_inla <- inla(dd10r_score_m ~ season_flood*Flood_1Lag + 
                    temp_mean + evap_mean + ndvi_mean +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL + 
                 f(wcode, model = 'iid'),
                 family ='gaussian', data = df,
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))
basic_inla_res <- getINLA_res(basic_inla)
plotResults(basic_inla_res)

# Stratified effects
basic_inla2 <- inla(dd10r_score_m ~ season_flood:Flood_1Lag + 
                     temp_mean + evap_mean + ndvi_mean +
                     treatment + ramadan + preg + dd10r_score_m_BL +
                     g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                     communication_BL + quint2_BL + woman_edu_cat__BL + 
                     mobility_BL + decision_BL + know_score_BL + 
                     f(wcode, model = 'iid'),
                   family ='gaussian', data = df,
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                   control.predictor = list(compute = TRUE))
basic_inla_STRAT <- getINLA_res(basic_inla2)
plotResults(basic_inla_STRAT)

#### 2. RINLA (2 way Interaction - with spatial-temporal autocorrelation) ####

st_inla <- inla(dd10r_score_m ~ season_flood*Flood_1Lag + 
                  temp_mean + evap_mean + ndvi_mean + 
                  treatment + ramadan + preg + dd10r_score_m_BL +
                  g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                  communication_BL + quint2_BL + woman_edu_cat__BL + 
                  mobility_BL + decision_BL + know_score_BL + 
               f(season_flood, model = 'iid') +
               f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                           group = season_id, control.group = list(model = "ar1"),
                                           hyper = prec.prior),
             family ='gaussian', data = df,
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE))
st_inla_res <- getINLA_res(st_inla)
plotResults(st_inla_res)

# Stratified effects
st_inla2 <- inla(dd10r_score_m ~ season_flood:Flood_1Lag + 
                   temp_mean + evap_mean + ndvi_mean + 
                  treatment + ramadan + preg + dd10r_score_m_BL +
                  g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                  communication_BL + quint2_BL + woman_edu_cat__BL + 
                  mobility_BL + decision_BL + know_score_BL + 
                  f(season_flood, model = 'iid') +
                  f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                              group = season_id, control.group = list(model = "ar1"),
                                              hyper = prec.prior),
                family ='gaussian', data = df,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute = TRUE))
st_inla_STRAT <- getINLA_res(st_inla2)
plotResults(st_inla_STRAT)


#### 3. RINLA (3 way - by treatment) ####

st_3_inla <- inla(dd10r_score_m ~ season_flood*Flood_1Lag*treatment + 
                  temp_mean + evap_mean + ndvi_mean +
                  ramadan + preg + dd10r_score_m_BL +
                  g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                  communication_BL + quint2_BL + woman_edu_cat__BL + 
                  mobility_BL + decision_BL + know_score_BL + 
                  f(season_flood, model = 'iid') +
                  f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                              group = season_id, control.group = list(model = "ar1"),
                                              hyper = prec.prior),
                family ='gaussian', data = df,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute = TRUE))
st_3_inla_res <- getINLA_res(st_3_inla)
plotResults(st_3_inla_res)

st_3_inla_STRAT <- inla(dd10r_score_m ~ season_flood:Flood_1Lag*treatment + 
                    temp_mean + evap_mean + ndvi_mean +
                    ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                    communication_BL + quint2_BL + woman_edu_cat__BL + 
                    mobility_BL + decision_BL + know_score_BL + 
                    f(season_flood, model = 'iid') +
                    f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                group = season_id, control.group = list(model = "ar1"),
                                                hyper = prec.prior),
                  family ='gaussian', data = df,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
Res_st_3_inla_STRAT <- getINLA_res(st_3_inla_STRAT)
plotResults(Res_st_3_inla_STRAT)

#### COMPARE ####
# Compare Metrics
# comp <- selINLA_mod(list('basic_inla', 'st_inla', 'st_3_inla'))
# comp[order(comp$MLIK), ] # The higher the better fit - Flooded_Diff
# comp[order(comp$DIC), ] # The lower the better fit - Flood_1
# comp[order(comp$WAIC), ] # The lower the better fit - Flood_1

#### Treatment subsets ####

# CONTROL
st_inlaC <- inla(dd10r_score_m ~ season_flood*Flood_1Lag + 
                   temp_mean + evap_mean + ndvi_mean +
                   ramadan + preg + dd10r_score_m_BL +
                   g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                   communication_BL + quint2_BL + woman_edu_cat__BL + 
                   mobility_BL + decision_BL + know_score_BL + 
                   f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                               group = season_id, control.group = list(model = "ar1"),
                                               hyper = prec.prior),
                 family ='gaussian', data = control,
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))
Res_st_inlaC <- getINLA_res(st_inlaC)
plotResults(Res_st_inlaC)

st_inlaC_STRAT <- inla(dd10r_score_m ~ season_flood:Flood_1Lag + 
                         temp_mean + evap_mean + ndvi_mean +
                         ramadan + preg + dd10r_score_m_BL +
                         g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                         communication_BL + quint2_BL + woman_edu_cat__BL + 
                         mobility_BL + decision_BL + know_score_BL + 
                         f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                     group = season_id, control.group = list(model = "ar1"),
                                                     hyper = prec.prior),
                       family ='gaussian', data = control,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))
Res_st_inlaC_STRAT <- getINLA_res(st_inlaC_STRAT)
plotResults(Res_st_inlaC_STRAT)

# TREAT
st_inlaT <- inla(dd10r_score_m ~ season_flood*Flood_1Lag + 
                   temp_mean + evap_mean + ndvi_mean +
                   ramadan + preg + dd10r_score_m_BL +
                   g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                   communication_BL + quint2_BL + woman_edu_cat__BL + 
                   mobility_BL + decision_BL + know_score_BL + 
                   f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                               group = season_id, control.group = list(model = "ar1"),
                                               hyper = prec.prior),
                 family ='gaussian', data = treat,
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))
Res_st_inlaT <- getINLA_res(st_inlaT)
plotResults(Res_st_inlaT)

st_inlaT_STRAT <- inla(dd10r_score_m ~ season_flood:Flood_1Lag + 
                         temp_mean + evap_mean + ndvi_mean +
                         ramadan + preg + dd10r_score_m_BL +
                         g_2h_BL + dep_ratio + wi_land_BL + hfias_BL + 
                         communication_BL + quint2_BL + woman_edu_cat__BL + 
                         mobility_BL + decision_BL + know_score_BL + 
                         f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                     group = season_id, control.group = list(model = "ar1"),
                                                     hyper = prec.prior),
                       family ='gaussian', data = treat,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))
Res_st_inlaT_STRAT <- getINLA_res(st_inlaT_STRAT)
plotResults(Res_st_inlaT_STRAT)

#### *** Plot posteriors over Space and time *** ####


plotResults(Res2_Treat_6month)


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




#### *** Model validation *** #####
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

################################################################################

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


#### *** 3.4 R-INLA Flood Metrics *** ####
# Check why this does not work for script 3...

# NOTE: Use 'Metric_test.csv' file from 0_DataFormatting.R to run this code (and check variable selection) !!!

## HERE: This model makes the most sense theoretically - outputs seem interpret able
Flood_1 <- inla(dd10r_score_m ~ Flood_1Lag + season_DD + season_flood:Flood_1Lag #Flood_1Lag
                + temp_mean + evap_mean + ndvi_mean + prec_mean + elev
                + treatment + ramadan + preg + dd10r_score_m_BL + dd10r_score_m_EL + hfias_BL
                + g_2h_BL + fam_type_BL + wi_land_BL + wi_hl_BL + wi_al_BL
                + woman_edu_cat__BL + mobility_BL + communication_BL
                + decision_BL + know_score_BL + wealth_BL
                + f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                              group = season_id, control.group = list(model = "ar1"),
                                              hyper = prec.prior),
                family ='gaussian', data = df,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute = TRUE))
Flood_1Lag <- getINLA_res(Flood_1)
plotResults(Flood_1Lag)


Flooded_diff <- inla(dd10r_score_m ~ flooded_diff_lag + season_DD + season_flood:flooded_diff_lag #Flood_1Lag   #Flood_1Lag
                     + temp_mean + evap_mean + ndvi_mean + prec_mean + elev
                     + treatment + ramadan + preg + dd10r_score_m_BL + dd10r_score_m_EL + hfias_BL
                     + g_2h_BL + fam_type_BL + wi_land_BL + wi_hl_BL + wi_al_BL
                     + woman_edu_cat__BL + mobility_BL + communication_BL
                     + decision_BL + know_score_BL + wealth_BL
                     + f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                   group = season_id, control.group = list(model = "ar1"),
                                                   hyper = prec.prior),
                     family ='gaussian', data = df,
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute = TRUE))
Flooded_diff_lag <- getINLA_res(Flooded_diff)
plotResults(Flooded_diff_lag)


Flooded_weight <- inla(dd10r_score_m ~ flooded_weight_lag + season_DD + season_flood:flooded_weight_lag #Flood_1Lag   #Flood_1Lag
                       + temp_mean + evap_mean + ndvi_mean + prec_mean + elev
                       + treatment + ramadan + preg + dd10r_score_m_BL + dd10r_score_m_EL + hfias_BL
                       + g_2h_BL + fam_type_BL + wi_land_BL + wi_hl_BL + wi_al_BL
                       + woman_edu_cat__BL + mobility_BL + communication_BL
                       + decision_BL + know_score_BL + wealth_BL
                       + f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                     group = season_id, control.group = list(model = "ar1"),
                                                     hyper = prec.prior),
                       family ='gaussian', data = df,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))
Flooded_weight_lag <- getINLA_res(Flooded_weight)
plotResults(Flooded_weight_lag)


Flooded_diff_w <- inla(dd10r_score_m ~ flooded_diff_w_lag + season_DD + season_flood:flooded_diff_w_lag #Flood_1Lag   #Flood_1Lag
                       + temp_mean + evap_mean + ndvi_mean + prec_mean + elev
                       + treatment + ramadan + preg + dd10r_score_m_BL + dd10r_score_m_EL + hfias_BL
                       + g_2h_BL + fam_type_BL + wi_land_BL + wi_hl_BL + wi_al_BL
                       + woman_edu_cat__BL + mobility_BL + communication_BL
                       + decision_BL + know_score_BL + wealth_BL
                       + f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                     group = season_id, control.group = list(model = "ar1"),
                                                     hyper = prec.prior),
                       family ='gaussian', data = df,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))
Flooded_diff_w_lag <- getINLA_res(Flooded_diff_w)
plotResults(Flooded_diff_w_lag)

#### COMPARE ####

# Compare Metrics
comp <- selINLA_mod(list('Flood_1', 'Flooded_diff', 'Flooded_weight', 'Flooded_diff_w'))
comp[order(comp$MLIK), ] # The higher the better fit - Flooded_Diff
comp[order(comp$DIC), ] # The lower the better fit - Flood_1
comp[order(comp$WAIC), ] # The lower the better fit - Flood_1

