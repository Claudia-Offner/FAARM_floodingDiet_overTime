#### R-INLA Analysis: Flooding & Dietary Diversity Over Time

# To run this code, set the work directory to folder containing the provided files & data
# setwd('C:/Users/offne/Documents/GitHub/RINLA_FloodDiet_Modelling/')
# 
# # Load Formatted RS data file
# library(here)
# suppressWarnings(source(here::here('0_DataFormatting.R')))

# Load packages
library(lme4)

## NOTES:
## - Setting priors (?)
## - GET BYM TO WORK WITH INTEGTRATED FIXED EFFECT

#### 1. General Mixed Effects Model (Gaussian) ####

#### Crude
crude_gme <- lmer(dd10r_score_m ~ Flood_1Lag+(1|wcode),
                  data=df)
crude_res <- getME_res(crude_gme)
plotResults(crude_res)

#### Controlling for select baseline variables
base_gme <- lmer(dd10r_score_m ~ Flood_1Lag +
                   temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                   treatment + ramadan + preg + dd10r_score_m_BL +
                   g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                   wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                   dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                   age_3_BL + (1|wcode),
                 data=df)
base_res <- getME_res(base_gme)
plotResults(base_res)

#### Controlling for DD season
season_gme <- lmer(dd10r_score_m ~ Flood_1Lag + season_DD + 
                     temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                     treatment + ramadan + preg + dd10r_score_m_BL +
                     g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                     wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                     dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                     age_3_BL + (1|wcode),
                   data=df)
season_res <- getME_res(season_gme)
plotResults(season_res)


#### 2. R-INLA Mixed Effects Model ####
#### Model selection criteria

#### Gaussian Mixed Effects Models w/ lag
inla_Gme <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                   temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                   treatment + ramadan + preg + dd10r_score_m_BL +
                   g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                   wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                   dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                   age_3_BL + 
                 f(wcode, model = 'iid'),
                 family ='gaussian', data = df, 
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), # compute model selection criteria
                 control.predictor = list(compute = TRUE))
Res_inla_Gme <- getINLA_res(inla_Gme) # Mixed-Effects (Gaussian w/ lag)
plotResults(Res_inla_Gme)
result <- cbind(summary(inla_Gme)$fixed[,-7])

#### !!!!! This is the formula used for model fitting !!!!!


#### 3.1. R-INLA Temporal Model ####
#### - Compare: iid, ar1, rw1

# Gaussian Temporal Models w/ lag
inla_GT.iid <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                      temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                      treatment + ramadan + preg + dd10r_score_m_BL +
                      g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                      wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                      dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                      age_3_BL + 
                    f(wcode, model = 'iid') + f(season_id, model='iid'),
                    family ='gaussian', data = df, 
                    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), 
                    control.predictor = list(compute = TRUE))
Res_inla_GT.iid <- getINLA_res(inla_GT.iid) # Temporal INLA - IID (Gaussian w/ lag)
plotResults(Res_inla_GT.iid)

inla_GT.ar <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                     temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                     treatment + ramadan + preg + dd10r_score_m_BL +
                     g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                     wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                     dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                     age_3_BL + 
                   f(wcode, model = 'iid') + f(season_id, model='ar1'),
                   family ='gaussian', data = df, 
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), 
                   control.predictor = list(compute = TRUE))
Res_inla_GT.ar <- getINLA_res(inla_GT.ar) # Temporal INLA - AR (Gaussian w/ lag)
plotResults(Res_inla_GT.ar)

inla_GT.rw <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                     temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                     treatment + ramadan + preg + dd10r_score_m_BL +
                     g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                     wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                     dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                     age_3_BL + 
                   f(wcode, model = 'iid') + f(season_id, model='rw1'),
                   family ='gaussian', data = df, 
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), 
                   control.predictor = list(compute = TRUE))
Res_inla_GT.rw <- getINLA_res(inla_GT.rw) # Temporal INLA - RW (Gaussian w/ lag)
plotResults(Res_inla_GT.rw)

#### 3.2. R-INLA Spatial Model ####
#### - Compare: iid, besag, besagproper, bym

# Gaussian Spatial Models w/ lag
inla_GS.iid <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                      temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                      treatment + ramadan + preg + dd10r_score_m_BL +
                      g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                      wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                      dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                      age_3_BL + 
                    f(wcode, model = 'iid') + f(OBJECTID_1, model = 'iid'),
                    family ='gaussian', data = df, 
                    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                    control.predictor = list(compute = TRUE))
Res_inla_GS.iid <- getINLA_res(inla_GS.iid) # Spatial INLA - IID (Gaussian w/ lag)
plotResults(Res_inla_GS.iid)

inla_GS.besag <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                        temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                        treatment + ramadan + preg + dd10r_score_m_BL +
                        g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                        wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                        dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                        age_3_BL + 
                      f(wcode, model = 'iid') + f(OBJECTID_1, model = 'besag', graph = W.adj.mat),
                      family ='gaussian', data = df, 
                      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                      control.predictor = list(compute = TRUE))
Res_inla_GS.besag <- getINLA_res(inla_GS.besag) # Spatial INLA - BESAG (Gaussian w/ lag)
plotResults(Res_inla_GS.besag)

inla_GS.besagproper <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                              temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                              treatment + ramadan + preg + dd10r_score_m_BL +
                              g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                              wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                              dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                              age_3_BL + 
                            f(wcode, model = 'iid') + f(OBJECTID_1, model = 'besagproper', graph = W.adj.mat),
                            family ='gaussian', data = df, 
                            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                            control.predictor = list(compute = TRUE))
Res_inla_GS.besagproper <- getINLA_res(inla_GS.besagproper) # Spatial INLA - BESAG PROPER (Gaussian w/ lag)
plotResults(Res_inla_GS.besagproper)

inla_GS.bym <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                      temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                      treatment + ramadan + preg + dd10r_score_m_BL +
                      g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                      wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                      dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                      age_3_BL + 
                    f(wcode, model = 'iid') + f(OBJECTID_1, model = 'bym', graph = W.adj.mat),
                    family ='gaussian', data = df, 
                    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                    control.predictor = list(compute = TRUE))
Res_inla_GS.bym <- getINLA_res(inla_GS.bym) # Spatial INLA - BYM (Gaussian w/ lag)
plotResults(Res_inla_GS.bym)


#### COMPARE ####

# Compare INLA Temporal Models
comp <- selINLA_mod(list('inla_GT.iid', 'inla_GT.ar', 'inla_GT.rw'))
comp[order(comp$WAIC), ]
# = AR1

# Compare INLA Spatial Models
comp <- selINLA_mod(list('inla_GS.iid', 'inla_GS.besag', 'inla_GS.besagproper', 'inla_GS.bym'))
comp[order(comp$WAIC), ]
# = bym/besagproper

#### 3.3. R-INLA Spatio-temporal Model ####

# Gaussian Separated Spatial-Temporal Effects
inla_GST1 <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                    temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                    wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                    dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                    age_3_BL + 
                  f(wcode, model = 'iid') + 
                  f(season_id, model='ar1', hyper=prec.prior) + 
                  f(OBJECTID_1, model='besagproper', graph=W.adj.mat, hyper=prec.prior),
                  family ='gaussian', data = df, 
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
Res_inla_GST1 <- getINLA_res(inla_GST1)
plotResults(Res_inla_GST1)

# Integrated
inla_GST2 <- inla(dd10r_score_m ~ Flood_1Lag + season_DD +
                    temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                    wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                    dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                    age_3_BL + 
                  f(wcode, model = 'iid') + 
                  f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                      group = season_id, control.group = list(model = "ar1"),
                      hyper = prec.prior),
                  family ='gaussian', data = df, 
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
Res_inla_GST2 <- getINLA_res(inla_GST2)
plotResults(Res_inla_GST2)

# Integrated & Interaction
inla_GST3 <- inla(dd10r_score_m ~ Flood_1Lag + season_DD + season_flood:Flood_1Lag + 
                    temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                    treatment + ramadan + preg + dd10r_score_m_BL +
                    g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                    wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                    dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                    age_3_BL + 
                    f(wcode, model = 'iid') + 
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                      group = season_id, control.group = list(model = "ar1"),
                      hyper = prec.prior),
                  family ='gaussian', data = df, 
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))
Res_inla_GST3 <- getINLA_res(inla_GST3)
plotResults(Res_inla_GST3)

#### COMPARE ####

# Compare ALL INLA models
comp <- selINLA_mod(list('inla_Gme', 
                         'inla_GT.iid', 'inla_GT.ar', 'inla_GT.rw',
                         'inla_GS.iid', 'inla_GS.besag', 'inla_GS.besagproper', 'inla_GS.bym',
                         'inla_GST1', 'inla_GST2', 'inla_GST3'))
comp[order(comp$WAIC), ]
comp[order(comp$DIC), ]
comp[order(comp$MLIK), ]
# Model GST3 is the best fit

#### 3.4 R-INLA Flood Metrics ####

# NOTE: Use 'Metric_test.csv' file from 0_DataFormatting.R to run this code (and check variable selection) !!!

## HERE: This model makes the most sense theoretically - outputs seem interpret able
Flood_1 <- inla(dd10r_score_m ~ Flood_1Lag + season_DD + season_flood:Flood_1Lag + 
                  temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                  treatment + ramadan + preg + dd10r_score_m_BL +
                  g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                  wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                  dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                  age_3_BL + 
                f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                              group = season_id, control.group = list(model = "ar1"),
                                              hyper = prec.prior),
                family ='gaussian', data = df,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute = TRUE))
Flood_1Lag <- getINLA_res(Flood_1)
plotResults(Flood_1Lag)



Flooded_diff <- inla(dd10r_score_m ~ flooded_diff_lag + season_DD + season_flood:flooded_diff_lag +
                       temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                       treatment + ramadan + preg + dd10r_score_m_BL +
                       g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                       wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                       dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                       age_3_BL + 
                     f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                   group = season_id, control.group = list(model = "ar1"),
                                                   hyper = prec.prior),
                     family ='gaussian', data = df,
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute = TRUE))
Flooded_diff_lag <- getINLA_res(Flooded_diff)
plotResults(Flooded_diff_lag)


Flooded_weight <- inla(dd10r_score_m ~ flooded_weight_lag + season_DD + season_flood:flooded_weight_lag +
                         temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                         treatment + ramadan + preg + dd10r_score_m_BL +
                         g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                         wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                         dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                         age_3_BL + 
                       f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                                                     group = season_id, control.group = list(model = "ar1"),
                                                     hyper = prec.prior),
                       family ='gaussian', data = df,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))
Flooded_weight_lag <- getINLA_res(Flooded_weight)
plotResults(Flooded_weight_lag)


Flooded_diff_w <- inla(dd10r_score_m ~ flooded_diff_w_lag + season_DD + season_flood:flooded_diff_w_lag +
                         temp_mean + evap_mean + ndvi_mean + prec_mean + elev +
                         treatment + ramadan + preg + dd10r_score_m_BL +
                         g_2h_BL + fam_type_BL + dep_ratio + wi_hl_BL +
                         wi_land_BL + hfias_BL + communication_BL + quint2_BL + 
                         dd10r_score_m_EL + wi_al_BL + woman_edu_cat__BL + mobility_BL + decision_BL + know_score_BL +
                         age_3_BL + 
                       f(wcode, model = 'iid') + f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
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




#### Examples #### 
#### R-INLA Temporal Models
# https://becarioprecario.bitbucket.io/inla-gitbook/ch-temporal.html#sec:autotime

#### R-INLA Spatial Model
# https://becarioprecario.bitbucket.io/inla-gitbook/ch-spatial.html

#### R-INLA Spatio-temporal Model
# https://becarioprecario.bitbucket.io/inla-gitbook/ch-temporal.html#sec:spacetime 

