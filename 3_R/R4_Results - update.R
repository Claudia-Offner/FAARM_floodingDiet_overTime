
# write.csv(inla_non_WDDSres, "C:/Users/ClaudiaOffner/Downloads/result-non.csv", row.names=FALSE)
# # write.csv(inla_treat_WDDSres, "C:/Users/ClaudiaOffner/Downloads/result-treat.csv", row.names=FALSE)
# write.csv(inla_seas_WDDSres, "C:/Users/ClaudiaOffner/Downloads/result-seas.csv", row.names=FALSE)
# write.csv(test, "C:/Users/ClaudiaOffner/Downloads/testt.csv", row.names=FALSE)

#### Variables ####
# var1 <- c('(Intercept)', 'Flood Extent', 'Treatment','DD BL', 'Ramadan', 'Religion BL', 'Wealth BL')
# var2 <- c('(Intercept) Jan/Feb season', 'Mar/Apr season', 
#           'May/Jun season', 'Jul/Aug season', 
#           'Sep/Oct season', 'Nov/Dec season', 
#           'Treatment','WDDS BL', 'Ramadan', 'Religion BL', 'Wealth BL', 
#           'Jan/Feb season : Flood Extent',
#           'Mar/Apr season : Flood Extent',
#           'May/Jun season : Flood Extent', 
#           'Jul/Aug season : Flood Extent',
#           'Sep/Oct season : Flood Extent', 
#           'Nov/Dec season : Flood Extent')
# var3 <- c('(Intercept) Jan/Feb season', 'Mar/Apr season', 
#          'May/Jun season', 'Jul/Aug season', 
#          'Sep/Oct season', 'Nov/Dec season', 
#          'Treatment','WDDS BL', 'Ramadan', 'Religion BL', 'Wealth BL', 
#          'Jan/Feb season : Flood Extent (Control)',
#          'Mar/Apr season : Flood Extent (Control)',
#          'May/Jun season : Flood Extent (Control)', 
#          'Jul/Aug season : Flood Extent (Control)',
#          'Sep/Oct season : Flood Extent (Control)', 
#          'Nov/Dec season : Flood Extent (Control)',
#          'Jan/Feb season : Flood Extent (Treatment)',
#          'Mar/Apr season : Flood Extent (Treatment)',
#          'May/Jun season : Flood Extent (Treatment)', 
#          'Jul/Aug season : Flood Extent (Treatment)',
#          'Sep/Oct season : Flood Extent (Treatment)', 
#          'Nov/Dec season : Flood Extent (Treatment)')

#### 1. Dietary Diversity ####


# Continuous: Dietary Diversity Scores (3-way interaction with season)
inla_WDDS <- inla(dd10r_score_m ~ season_flood*Flood_1Lag*treatment +
                    dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + 
                    f(wcode, model = 'iid') + # control for women random effect on DD
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                      group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                      hyper = prec.prior),
                  family ='gaussian', data = df, weights = wdiet_wt,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config=TRUE),
                  control.predictor = list(compute = TRUE))


# Continuous: Dietary Diversity Scores (3-way interaction with season)
inla_WDDS <- inla(dd10r_score_m ~ season_flood + season_flood:Flood_1Lag*treatment +
                    dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + 
                    f(wcode, model = 'iid') + # control for women random effect on DD
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                      group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                      hyper = prec.prior),
                  family ='gaussian', data = df, weights = wdiet_wt,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config=TRUE),
                  control.predictor = list(compute = TRUE))

# Quick visual
inla_WDDSres <- getINLA_res(inla_WDDS)
inla_WDDSres$Variables <- var3
plotResults(inla_WDDSres)


#### 2. Minimum Dietary Diversity  ####

# Binary: Minimum dietary diversity(no interaction)
inla_non_MDD <- inla(dd10r_min_m ~ Flood_1Lag+ treatment +
                       dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + 
                        f(wcode, model = 'iid') + # control for women random effect on DD
                        f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                          group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                          hyper = prec.prior),
                      family ='logistic', data = df, weights = wdiet_wt,
                      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                      control.predictor = list(compute = TRUE))

# # Binary: Minimum dietary diversity (2-way interaction with treatment)
# inla_treat_WDDS <- inla(dd10r_min_m ~ Flood_1Lag*treatment +
#                         dd10r_min_m_BL + ramadan + g_2h_BL + quint2_BL + 
#                         f(wcode, model = 'iid') + # control for women random effect on DD
#                         f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
#                           group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
#                           hyper = prec.prior),
#                       family ='gaussian', data = df, weights = wdiet_wt,
#                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#                       control.predictor = list(compute = TRUE))

# Binary: Minimum dietary diversity (2-way interaction with season)
inla_seas_MDD <- inla(dd10r_min_m ~ season_flood + season_flood:Flood_1Lag + treatment +
                        dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + 
                         f(wcode, model = 'iid') + # control for women random effect on DD
                         f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                           group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                           hyper = prec.prior),
                       family ='logistic', data = df, weights = wdiet_wt,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))

# Binary: Minimum dietary diversity
inla_MDD <- inla(dd10r_min_m ~ season_flood + season_flood:Flood_1Lag*treatment +
                   dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                         f(wcode, model = 'iid') + # control for women random effect on DD
                         f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                           group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                           hyper = prec.prior),
                       family ='logistic', data = df, weights = wdiet_wt,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))

# Visuals
inla_non_MDDres <- getINLA_res(inla_non_MDD)
inla_non_MDDres$Variables <- var1
inla_non_MDDres$Mean <- exp(inla_non_MDDres$Mean)
inla_non_MDDres$Lower_CI <- exp(inla_non_MDDres$Lower_CI)
inla_non_MDDres$Upper_CI <- exp(inla_non_MDDres$Upper_CI)
plotResults(inla_non_MDDres, 1)

inla_seas_MDDres <- getINLA_res(inla_seas_MDD)
inla_seas_MDDres$Variables <- var2
inla_seas_MDDres$Mean <- exp(inla_seas_MDDres$Mean)
inla_seas_MDDres$Lower_CI <- exp(inla_seas_MDDres$Lower_CI)
inla_seas_MDDres$Upper_CI <- exp(inla_seas_MDDres$Upper_CI)
plotResults(inla_seas_MDDres, 1)

inla_MDDres <- getINLA_res(inla_MDD)
inla_MDDres$Variables <- var3
inla_MDDres$Mean <- exp(inla_MDDres$Mean)
inla_MDDres$Lower_CI <- exp(inla_MDDres$Lower_CI)
inla_MDDres$Upper_CI <- exp(inla_MDDres$Upper_CI)
plotResults(inla_MDDres, 1)


#### 3-Interaction Models: Food Groups  ####

# Binary: Vitamin A rich fruit/veg
inla_VITA <- inla(dd10r_vita ~ season_flood + season_flood:Flood_1Lag*treatment +
                   dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + 
                   f(wcode, model = 'iid') + # control for women random effect on DD
                   f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                     group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                     hyper = prec.prior),
                 family ='logistic', data = df, weights = wdiet_wt,
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 control.predictor = list(compute = TRUE))

# Binary: Dairy
inla_DAIRY <- inla(dd10r_dairy ~ season_flood + season_flood:Flood_1Lag*treatment +
                    dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                    f(wcode, model = 'iid') + # control for women random effect on DD
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                      group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                      hyper = prec.prior),
                  family ='logistic', data = df, weights = wdiet_wt,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))

# Binary: Legumes
inla_LEGUME <- inla(dd10r_legume ~ season_flood + season_flood:Flood_1Lag*treatment +
                     dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                     f(wcode, model = 'iid') + # control for women random effect on DD
                     f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                       group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                       hyper = prec.prior),
                   family ='logistic', data = df, weights = wdiet_wt,
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                   control.predictor = list(compute = TRUE))

# Binary: Nuts
inla_NUTS <- inla(dd10r_nuts ~ season_flood + season_flood:Flood_1Lag*treatment +
                      dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                      f(wcode, model = 'iid') + # control for women random effect on DD
                      f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                        group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                        hyper = prec.prior),
                    family ='logistic', data = df, weights = wdiet_wt,
                    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                    control.predictor = list(compute = TRUE))

# Binary: Other fruit
inla_OTHF <- inla(dd10r_othf ~ season_flood + season_flood:Flood_1Lag*treatment +
                    dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                    f(wcode, model = 'iid') + # control for women random effect on DD
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                      group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                      hyper = prec.prior),
                  family ='logistic', data = df, weights = wdiet_wt,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))

#### Visuals ####


inla_WDDSres2 <- getINLA_res(inla_WDDS2)
inla_WDDSres2$Variables <- var
plotResults(inla_WDDSres2)

inla_MDDres <- getINLA_res(inla_MDD)
inla_MDDres$Variables <- var
inla_MDDres$Mean <- exp(inla_MDDres$Mean)
inla_MDDres$Lower_CI <- exp(inla_MDDres$Lower_CI)
inla_MDDres$Upper_CI <- exp(inla_MDDres$Upper_CI)
plotResults(inla_MDDres, 1)

inla_VITAres <- getINLA_res(inla_VITA)
inla_VITAres$Variables <- var
inla_VITAres$Mean <- exp(inla_VITAres$Mean)
inla_VITAres$Lower_CI <- exp(inla_VITAres$Lower_CI)
inla_VITAres$Upper_CI <- exp(inla_VITAres$Upper_CI)
plotResults(inla_VITAres, 1)

inla_DAIRYres <- getINLA_res(inla_DAIRY)
inla_DAIRYres$Variables <- var
inla_DAIRYres$Mean <- exp(inla_DAIRYres$Mean)
inla_DAIRYres$Lower_CI <- exp(inla_DAIRYres$Lower_CI)
inla_DAIRYres$Upper_CI <- exp(inla_DAIRYres$Upper_CI)
plotResults(inla_DAIRYres, 1)

inla_LEGUMEres <- getINLA_res(inla_LEGUME)
inla_LEGUMEres$Variables <- var
inla_LEGUMEres$Mean <- exp(inla_LEGUMEres$Mean)
inla_LEGUMEres$Lower_CI <- exp(inla_LEGUMEres$Lower_CI)
inla_LEGUMEres$Upper_CI <- exp(inla_LEGUMEres$Upper_CI)
plotResults(inla_LEGUMEres, 1)

inla_NUTSres <- getINLA_res(inla_NUTS)
inla_NUTSres$Variables <- var
inla_NUTSres$Mean <- exp(inla_NUTSres$Mean)
inla_NUTSres$Lower_CI <- exp(inla_NUTSres$Lower_CI)
inla_NUTSres$Upper_CI <- exp(inla_NUTSres$Upper_CI)
plotResults(inla_NUTSres, 1)

inla_OTHFres <- getINLA_res(inla_OTHF)
inla_OTHFres$Variables <- var
inla_OTHFres$Mean <- exp(inla_OTHFres$Mean)
inla_OTHFres$Lower_CI <- exp(inla_OTHFres$Lower_CI)
inla_OTHFres$Upper_CI <- exp(inla_OTHFres$Upper_CI)
plotResults(inla_OTHFres, 1)




