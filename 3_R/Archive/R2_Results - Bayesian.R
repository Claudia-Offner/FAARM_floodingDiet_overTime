# RUN 3-WAY INTERACTIONS (BAYESIAN)

# write.csv(inla_non_WDDSres, "C:/Users/ClaudiaOffner/Downloads/result-non.csv", row.names=FALSE)

#### Variables ####

# For previous version of partial interaction model (season:Flood*Treat)
var3 <- c('(Intercept) Jan/Feb season', 'Mar/Apr season', 'May/Jun season', 
          'Jul/Aug season', 'Sep/Oct season', 'Nov/Dec season', 
          'Treatment','WDDS BL', 'Ramadan', 'Religion BL', 'Wealth BL',
          'Jan/Feb season : Flood Extent (Control)',
          'Mar/Apr season : Flood Extent (Control)',
          'May/Jun season : Flood Extent (Control)',
          'Jul/Aug season : Flood Extent (Control)',
          'Sep/Oct season : Flood Extent (Control)',
          'Nov/Dec season : Flood Extent (Control)',
          'Jan/Feb season : Flood Extent (Treatment)',
          'Mar/Apr season : Flood Extent (Treatment)',
          'May/Jun season : Flood Extent (Treatment)',
          'Jul/Aug season : Flood Extent (Treatment)',
          'Sep/Oct season : Flood Extent (Treatment)',
          'Nov/Dec season : Flood Extent (Treatment)')

# For current trial version of full interaction model (season*Flood*Treat)
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

#### 1. Dietary Diversity Scores (continuous) ####

# CURRENT VERSION WITH FULL MAIN EFFECTS (depends on ability to calculate marginal effects for Bayesian models)
# Continuous: Dietary Diversity Scores (3-way interaction with season)

inla_WDDS <- inla(dd10r_score_m ~ Flood_1Lag*season_flood*treatment +
                    dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                    f(wcode, model = 'iid') + # control for women random effect on DD
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                      group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                      hyper = prec.prior),
                  family ='gaussian', data = df, weights = wdiet_wt,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config=TRUE),
                  control.predictor = list(compute = TRUE))

inla_WDDS$marginals.fixed
summary(inla_WDDS)


# Quick visual
inla_WDDSres <- getINLA_res(inla_WDDS)
inla_WDDSres$Variables <- var
plotResults(inla_WDDSres)
# 0.028552365 + -0.309932866 # Flood:Cont:Mar/Apr -0.2813805 [flood(Jan/Feb)  + flood:season(Mar/Apr)]
# 0.028552365 + -0.309932866 + 0.021864205 + -0.843075653 # Flood:Treat:Mar/Apr -1.102592 [flood(Jan/Feb) + treat:season(Mar/Apr) + flood:season(Mar/Apr) + flood:season:treat(Mar/Apr)]


# PREVIOUS VERSION WITHOUT SEASON:TREAT TERMS (stratified analysis)
# Continuous: Dietary Diversity Scores (3-way interaction with season)
inla_WDDS1 <- inla(dd10r_score_m ~ season_flood + Flood_1Lag + season_flood:Flood_1Lag*treatment +
                    dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                    f(wcode, model = 'iid') + # control for women random effect on DD
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                      group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                      hyper = prec.prior),
                  family ='gaussian', data = df, weights = wdiet_wt,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config=TRUE),
                  control.predictor = list(compute = TRUE))

predictions <- predict(inla_WDDS1)
print(predictions_new$mean)

# Visuals
inla_WDDSres1 <- getINLA_res(inla_WDDS1)
inla_WDDSres1$Variables <- var3
plotResults(inla_WDDSres1)

# # Manual calc. strat
# 0.058734846 + -0.743679640 # Flood:Cont:Mar/Apr -0.2813805 [season(Mar/Apr)  + flood:season(Mar/Apr)]
# 0.058734846 + -0.148450406  # Flood:Treat:Mar/Apr -1.102592 [season(Mar/Apr) + flood:season(Mar/Apr) + flood:season:treat(Mar/Apr)]


#### 2. Minimum Dietary Diversity (binary) ####

# PREVIOUS VERSION WITHOUT SEASON:TREAT TERMS (avoids multi-co linearity)
# Binary: Minimum dietary diversity (3-way interaction with season)
inla_MDD <- inla(dd10r_min_m ~ season_flood + season_flood:Flood_1Lag*treatment +
                   dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                         f(wcode, model = 'iid') + # control for women random effect on DD
                         f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                           group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                           hyper = prec.prior),
                       family ='logistic', data = df, weights = wdiet_wt,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))

# Visuals (+ convert to ORs)
inla_MDDres <- getINLA_res(inla_MDD)
inla_MDDres$Variables <- var3
inla_MDDres$Mean <- exp(inla_MDDres$Mean)
inla_MDDres$Lower_CI <- exp(inla_MDDres$Lower_CI)
inla_MDDres$Upper_CI <- exp(inla_MDDres$Upper_CI)
plotResults(inla_MDDres, 1)


#### 3. Food Group Consumption (binary) ####

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

#### 3. Food Group Visuals (+ convert to ORs) ####

inla_VITAres <- getINLA_res(inla_VITA)
inla_VITAres$Variables <- var3
inla_VITAres$Mean <- exp(inla_VITAres$Mean)
inla_VITAres$Lower_CI <- exp(inla_VITAres$Lower_CI)
inla_VITAres$Upper_CI <- exp(inla_VITAres$Upper_CI)
plotResults(inla_VITAres, 1)

inla_DAIRYres <- getINLA_res(inla_DAIRY)
inla_DAIRYres$Variables <- var3
inla_DAIRYres$Mean <- exp(inla_DAIRYres$Mean)
inla_DAIRYres$Lower_CI <- exp(inla_DAIRYres$Lower_CI)
inla_DAIRYres$Upper_CI <- exp(inla_DAIRYres$Upper_CI)
plotResults(inla_DAIRYres, 1)

inla_LEGUMEres <- getINLA_res(inla_LEGUME)
inla_LEGUMEres$Variables <- var3
inla_LEGUMEres$Mean <- exp(inla_LEGUMEres$Mean)
inla_LEGUMEres$Lower_CI <- exp(inla_LEGUMEres$Lower_CI)
inla_LEGUMEres$Upper_CI <- exp(inla_LEGUMEres$Upper_CI)
plotResults(inla_LEGUMEres, 1)

inla_NUTSres <- getINLA_res(inla_NUTS)
inla_NUTSres$Variables <- var3
inla_NUTSres$Mean <- exp(inla_NUTSres$Mean)
inla_NUTSres$Lower_CI <- exp(inla_NUTSres$Lower_CI)
inla_NUTSres$Upper_CI <- exp(inla_NUTSres$Upper_CI)
plotResults(inla_NUTSres, 1)

inla_OTHFres <- getINLA_res(inla_OTHF)
inla_OTHFres$Variables <- var3
inla_OTHFres$Mean <- exp(inla_OTHFres$Mean)
inla_OTHFres$Lower_CI <- exp(inla_OTHFres$Lower_CI)
inla_OTHFres$Upper_CI <- exp(inla_OTHFres$Upper_CI)
plotResults(inla_OTHFres, 1)




