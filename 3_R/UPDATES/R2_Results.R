

# Load the caret package
library(caret)


#### DUMMY CODE VARIABLES ####
# Create a sample data frame for dummy variables
d <- data.frame(
  season_flood = df$season_flood,
  treatment = df$treatment
)

# Set meaningful labesl to categories
d$treatment[d$treatment == 0] <- 'control'
d$treatment[d$treatment == 1] <- 'treatment'

# Create a new factor variable for combinations of Column1 and Column2
d$Int. <- interaction(d$season_flood, d$treatment)

# Convert the new factor variable to dummy variables
dummy_data <- model.matrix(~ season_flood + Int., data = d)
colnames(dummy_data) <- gsub("Int.", "", colnames(dummy_data)) # remove col characters
colnames(dummy_data) <- gsub("season_flood", "", colnames(dummy_data)) # remove col characters
colnames(dummy_data) <- gsub("/", "", colnames(dummy_data)) # remove col characters

# Combine the dummy variables with the original data frame
df <- cbind(df, dummy_data)
colnames(dummy_data)


#### Modeling ####

formula <- 'dd10r_score_m ~ season_flood + treatment + Flood_1Lag +
                dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                Flood_1Lag:JanFeb + Flood_1Lag:MarApr + Flood_1Lag:MayJun + 
                Flood_1Lag:JulAug + Flood_1Lag:SeptOct + Flood_1Lag:NovDec + 
                
            f(wcode, model = "iid") + 
            f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, 
              group = season_id, control.group = list(model = "ar1"), 
              hyper = prec.prior)'

# Continuous: Dietary Diversity Scores
inla_WDDS <- inla(dd10r_score_m ~ season_flood + treatment + 
                    dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL +
                    Flood_1Lag:JanFeb + Flood_1Lag:MarApr + Flood_1Lag:MayJun + 
                    Flood_1Lag:JulAug + Flood_1Lag:SeptOct + Flood_1Lag:NovDec +
                    Flood_1Lag:MarApr.control + Flood_1Lag:MayJun.control + 
                    Flood_1Lag:JulAug.control + Flood_1Lag:SeptOct.control + Flood_1Lag:NovDec.control + 
                    Flood_1Lag:JanFeb.treatment + Flood_1Lag:MarApr.treatment + Flood_1Lag:MayJun.treatment + 
                    Flood_1Lag:JulAug.treatment + Flood_1Lag:SeptOct.treatment + Flood_1Lag:NovDec.treatment +
                    f(wcode, model = "iid") + 
                    f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, 
                      group = season_id, control.group = list(model = "ar1"), 
                      hyper = prec.prior),
                  family ='gaussian', data = df, weights = wdiet_wt,
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE))

inla_WDDSres <- getINLA_res(inla_WDDS)
# inla_WDDSres$Variables <- var3
plotResults(inla_WDDSres)


# Continuous: Dietary Diversity Scores (2-way interaction with season)
inla_seas_WDDS <- inla(dd10r_score_m ~ season_flood*Flood_1Lag*treatment +
                         # dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + 
                         f(wcode, model = 'iid') + # control for women random effect on DD
                         f(OBJECTID_1, model = "besagproper", graph = W.adj.mat, # control for spatial effects
                           group = season_id, control.group = list(model = "ar1"), # grouped by time point on DD
                           hyper = prec.prior),
                       family ='gaussian', data = df, weights = wdiet_wt,
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE))

inla_seas_WDDSres <- getINLA_res(inla_seas_WDDS)
# inla_seas_WDDSres$Variables <- var2
plotResults(inla_seas_WDDSres)
