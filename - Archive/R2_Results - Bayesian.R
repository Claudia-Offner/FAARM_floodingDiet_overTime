# RUN 3-WAY INTERACTIONS (BAYESIAN)

# write.csv(inla_non_WDDSres, "C:/Users/ClaudiaOffner/Downloads/result-non.csv", row.names=FALSE)

#### 0. Packages & Functions ####
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
plotResults <- function(res1, x=0) {
  
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
    geom_vline(xintercept=x, color='black', linetype='dashed', alpha=.5) +
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


# Function to compare R-INLA models in table
testINLA_Interact <- function(model1, model2){
  # Make sure MODEL1 has interactions and MODEL2 is the null model
  
  # Get marginal log-likelihood
  log_ml1 <- summary(model1)$mlik[2]
  log_ml2 <- summary(model2)$mlik[2]
  
  # Get the logarithm of the Bayesian Factor
  log_BF <- log_ml1 - log_ml2
  
  # Get Bayesian Factor
  BF <- exp(0.5 * (log_ml1 - log_ml2))
  
  # Create Table
  x <- as.data.frame(rbind(cbind("Bayesian Factor", BF),
                           cbind('Log Bayesian Factor', log_BF)))
  colnames(x) = c('Measure of Evidence', "Results") # 'CPO'
  x
  
}

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




