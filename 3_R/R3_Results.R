# RUN 3-WAY INTERACTION (FREQUENTIST)

## Detatch packages & clear environment/plots
### lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
### rm(list = ls())

## IMPORTANT - set file path to data folder location
setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/4. Data')

# 0. PACKAGES & FUNCTIONS ####

# Load packages
library(openxlsx)
library(dplyr)
library(spdep)
library(nlme) 
library(lme4)
library(ggplot2)
library(margins) # https://www.rdocumentation.org/packages/margins/versions/0.3.26
library(emmeans) 

# HELPFUL RESOURCES
# https://rdrr.io/cran/emmeans/f/vignettes/basics.Rmd
# https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
# https://stats.stackexchange.com/questions/592518/average-marginal-means-with-marginaleffects?rq=1


# Function to round numeric columns of data frame
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# Funciton to calculate odds ratios from Logistic Regressions
getORs <- function(df, columns) {
  
  # OR TRANSFORMATION
  for (c in columns) {
    df[[c]] <- exp(df[[c]])
  }
  
  return(df)
}

# Function to calculate probabilities from Logistic Regressions
getPROBS <- function(df, columns) {
  
  # OR TRANSFORMATION
  for (c in columns) {
    df[[c]] <- exp(df[[c]])/ (1 + exp(df[[c]]))
  }
  
  return(df)
}

# Function to read LME results in table
getLME <- function(lme_model, var=0){
  
  # Use the summary function to get a summary of the model
  summary_table <- summary(lme_model)
  
  # Extract relevant information (coefficient estimates, standard errors, p-values, and confidence intervals)
  lme_res <- round(as.data.frame(summary_table$tTable[, c("Value", "Std.Error", "DF", "t-value", "p-value")]), 4)
  lme_res$Variable <- rownames(lme_res)
  rownames(lme_res) <- NULL
  colnames(lme_res) <- c("Estimate", "Std.Error", "df", "t-Value", "p-value", "Variable") # Rename columns
  
  # Calculate confidence intervals
  lme_res$LowerCI <- round(lme_res$Estimate - qt(0.975, summary_table$tTable[, "DF"]) * lme_res$Std.Error, 4)
  lme_res$UpperCI <- round(lme_res$Estimate + qt(0.975, summary_table$tTable[, "DF"]) * lme_res$Std.Error, 4)
  lme_res <- lme_res %>% # Re-order columns
    select(Variable, Estimate, Std.Error, LowerCI, UpperCI, `p-value`)
  
  # Plot
  lme_res$index <- 1:nrow(lme_res) # set index
  names(lme_res) <- c("Variables","Mean","SD","Lower_CI","Upper_CI", 'P_Value',"Index")
  
  # Set significance col (for plotting)
  lme_res$P_Value <- as.numeric(lme_res$P_Value)
  lme_res$Importance <- '0_None'
  lme_res$Importance[lme_res$P_Value <= 0.10 & lme_res$P_Value >= 0.05] <- '1_Weak'
  lme_res$Importance[lme_res$P_Value <= 0.05] <- '2_Strong'
  
  # Get results
  if(var != 0) {
    
    lme_res$Variables <- var
    
  }
  
  return(round_df(lme_res, 5))
  
}

# Function to read GLMM results in table
getGLMM <- function(glmm_model, var, rep=0) {
  
  # Get glmm_results
  glmm_res <- as.data.frame(summary(glmm_model)$coef)
  
  # Calcualte CIs
  colnames(glmm_res) <- c("Estimate", "Std.Error", "z-value", "p-value") # Rename columns
  glmm_res$LowerCI <- round(glmm_res[["Estimate"]] - 1.96 * glmm_res[["Std.Error"]], 4)
  glmm_res$UpperCI <- round(glmm_res[["Estimate"]] + 1.96 * glmm_res[["Std.Error"]], 4)
  glmm_res$Variable <- rownames(glmm_res)
  rownames(glmm_res) <- NULL
  glmm_res <- glmm_res %>% # Re-order columns
    select(Variable, Estimate, Std.Error, LowerCI, UpperCI, `p-value`)
  
  # Plot
  glmm_res$index <- 1:nrow(glmm_res) # set indeglmm_res
  names(glmm_res) <- c("Variables","Mean","SD","Lower_CI","Upper_CI", 'P_Value',"Index")
  
  # Set significance col (for plotting)
  glmm_res$P_Value <- as.numeric(glmm_res$P_Value)
  glmm_res$Importance <- '0_None'
  glmm_res$Importance[glmm_res$P_Value <= 0.10 & glmm_res$P_Value >= 0.05] <- '1_Weak'
  glmm_res$Importance[glmm_res$P_Value <= 0.05] <- '2_Strong'
  
  #Set variable names
  glmm_res$Variables <- var
  
  # Set representation of results
  if (rep=='OR'){
    # Convert to ORs
    glmm_res <- getORs(glmm_res, c('Mean', 'Lower_CI','Upper_CI'))
    return(round_df(glmm_res, 5))
  } else if (rep=='PROBS') {
    glmm_res <- getPROBS(glmm_res, c('Mean', 'Lower_CI','Upper_CI'))
    return(round_df(glmm_res, 5))
  } else {
    return(round_df(glmm_res, 5))
  }

}

# Function for formatting contrast and trend tables for forest plotting function (plotResults)
formatRES <- function(df) {
  for (c in names(df)) {
    
    mean <- c('1.trend', 'estimate', 'emmean')
    lci <- c('asymp.LCL', 'lower.CL')
    uci <- c('asymp.UCL', 'upper.CL')
    if (c %in% mean) {
      colnames(df)[colnames(df) %in% mean] <- "Mean"
    } 
    if (c %in% lci) {
      colnames(df)[colnames(df) %in% lci] <- "Lower_CI"
    } 
    if (c %in% uci) {
      colnames(df)[colnames(df) %in% uci] <- "Upper_CI"
    } 
    if (all('p.value' == c)) {
      colnames(df)[colnames(df) %in% 'p.value'] <- "P_Value"
    } 
    if (all('SE' == c)) {
      colnames(df)[colnames(df) %in% 'SE'] <- "SD"
    } 
  }
  
  # Plot
  df$Index <- 1:nrow(df) # set index
  df$Variables <- apply(df[, 1:3], 1, paste, collapse = ":") # set variables
  df <- df[, c("Variables", "Mean", "SD", "Lower_CI", "Upper_CI", "P_Value", "Index")]
  
  # Set significance col (for plotting)
  df$P_Value <- as.numeric(df$P_Value)
  df$Importance <- '0_None'
  df$Importance[df$P_Value <= 0.10 & df$P_Value >= 0.05] <- '1_Weak'
  df$Importance[df$P_Value <= 0.05] <- '2_Strong'
  
  return(df)
}

# Function to plot R-INLA results in forest plot; NOTE: use round_df function on df
plotResults <- function(res1, x=0, name=0) {
  
  res <- round_df(res1, 3)
  
  if (name != 0) {
    result_name <- name
  } else {
    result_name <- deparse(substitute(res1))
  }
  
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

# Model Variable Names
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

# 1. Example Interaction model for VanderWeele ####

# Example of interactionR package
# library(interactionR)
# data (OCdata)
# 
# ## fit the interaction model
# model.glm <- glm(oc ~ alc*smk, family = binomial(link = "logit"), data = OCdata)
# ## format tables
# table_object = interactionR(model.glm, exposure_names = c("alc", "smk"), ci.type = "mover", ci.level = 0.95, em = T, recode = F)
# ## display
# interactionR_table(table_object)



# 3.A INTERACTION ANALYSIS 3-way: All-Continuous Outcome ####

# Set path for saving data
path <- 'REPORTING/dd10r_score_m_3/'

# Model (Temporal - no spatial)
lme_mod <- lme(
  fixed = dd10r_score_m ~ Flood_1Lag * season_flood * treatment 
           + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL,  # Controls
  random = list(wcode = pdDiag(~1), c_code = pdDiag(~1)), #, season_id = pdDiag(~1)
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
  data = df,
  na.action = na.omit  # Handle missing data using na.omit
)

# Get results
(mod_res <- getLME(lme_mod))
summary(lme_mod)
anova(lme_mod) #  Check ANOVA for statistically strong interactions (some evidence)

## 3A. Average Marginal Effects (slopes
### EMTRENDS: Estimates the slope of coefficients, accounting for reference groups.
ame1 <- emtrends(lme_mod, pairwise ~ Flood_1Lag*season_flood*treatment, var = 1)$emtrends
(ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
# ame2 <- emtrends(lme_mod, pairwise ~ Flood_1Lag*season_flood*treatment, var = 1, at = list(Flood_1Lag = levels))$emtrends
# ame2 # NOTE that there is no difference when flood inundation changes (because flooding has a fixed slope)
# Summary of relevant contrasts, including CIs
(ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))

# # EXAMPLE MANUAL CHECKS
# -0.0946 # Flood:Cont:Jan/Feb [flood(Jan/Feb)]
# -0.0946 + -0.5741 # Flood:Cont:Mar/Apr [flood(Jan/Feb)  + flood:season(Mar/Apr)]
# -0.0946 +  0.2578 # Flood:Cont:May/Jun [flood(Jan/Feb)  + flood:season(May/Jun)]
# # etc.
# -0.0946 + -0.1219 # Flood:Treat:Jan/Feb [flood + flood:treat]
# -0.0946 + -0.1219 + -0.5741 + -0.2799 # Flood:Treat:Mar/Apr [flood(Jan/Feb) + treat:season(Mar/Apr)) + flood:season(Mar/Apr) + flood:season:treat(Mar/Apr)]
# -0.0946 + -0.1219 + 0.2578 + 0.0135 # Flood:Treat:May/Jun [flood(Jan/Feb) + treat:season(Jan/Feb)) + flood:season(May/Jun) + flood:season:treat(May/Jun)]
# # etc.


## 3B. Estimated Marginal Means (predictions
### EMMEANS: Estimates the predicted value of WDDS for difference coef values, accounting for reference groups.
# AVERAGE LEVEL OF INUNDATON
emm_1 <- emmeans(lme_mod, ~ Flood_1Lag*season_flood*treatment, at=list(Flood_1Lag = levels[2])) # 1% increase
(emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
### Summary of contrasts, including CIs
(contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
(contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
(contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
# MULTIPLE LEVELS OF INUNDATON
emm_2 <- emmeans(lme_mod, ~ Flood_1Lag*season_flood*treatment, at = list(Flood_1Lag = levels)) # levels
(emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
### Summary of contrasts, including CIs
(contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
(contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
(contr_emm2C <- summary(contrast(emm_2, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))


## Export Tables & Plots
tables <- c('mod_res', 'ame1_res', 'ame1_cont', 'emm_1_res', 
            'contr_emm1A', 'contr_emm1B', 'emm_2_res', 
            'contr_emm2A', 'contr_emm2B', 'contr_emm2C')

for(t in tables) {
  
  write.xlsx(get(t), paste0(path , t , "_m3.xlsx"), rowNames=FALSE, fileEncoding = "UTF-8")

  if(t=='mod_res'){
    suppressMessages(ggsave(paste0(path, t, "_m3_plot.pdf"), plot = plotResults(get(t))))
  } else{
    suppressMessages(ggsave(paste0(path, t, "_m3_plot.pdf"), plot = plotResults(formatRES(get(t)))))
  }
  
  print(paste0('dd10r_score_m_3-', t, ": tables and plots exported"))
}

print(plotResults(mod_res))
print("VARIABLE dd10r_score_m_3 IS COMPLETE")


# 3.B INTERACTION ANALYSIS 2-way: Season-Continuous Outcome ####

# Set path for saving data
v <- 'dd10r_score_m_2_seas'
path <- paste0('REPORTING/', v, '/')

# Model (Temporal - no spatial)
lme_mod <- lme(
  fixed = dd10r_score_m ~ Flood_1Lag * season_flood + treatment 
  + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL,  # Controls
  random = list(wcode = pdDiag(~1), c_code = pdDiag(~1)), #, season_id = pdDiag(~1)
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
  data = df,
  na.action = na.omit  # Handle missing data using na.omit
)

# Get results
(mod_res <- getLME(lme_mod))
anova(lme_mod) #  Check ANOVA for statistically strong interactions (marginal evidence)

## 3A. Average Marginal Effects (slopes)
ame1 <- emtrends(lme_mod, pairwise ~ Flood_1Lag*season_flood, var = 1)$emtrends
(ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))

## 3B. Estimated Marginal Means (predictions)
# AVERAGE LEVEL OF INUNDATON
emm_1 <- emmeans(lme_mod, ~ Flood_1Lag*season_flood, at=list(Flood_1Lag = levels[2])) # 1% increase
(emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
### Summary of contrasts, including CIs
(contr_emm1A <- summary(contrast(emm_1, "pairwise"), infer = c(TRUE, TRUE)))

# MULTIPLE LEVELS OF INUNDATON
emm_2 <- emmeans(lme_mod, ~ Flood_1Lag*season_flood, at = list(Flood_1Lag = levels)) # levels
(emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
### Summary of contrasts, including CIs
(contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
(contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("season_flood")), infer = c(TRUE, TRUE)))


## Export Tables & Plots
tables <- c('mod_res', 'ame1_res', 'emm_1_res', 
            'contr_emm1A', 'emm_2_res', 
            'contr_emm2A', 'contr_emm2B')

for(t in tables) {
  
  write.xlsx(get(t), paste0(path , t , "_m2_seas.xlsx"), rowNames=FALSE, fileEncoding = "UTF-8")
  
  if(t=='mod_res'){
    suppressMessages(ggsave(paste0(path, t, "_m2_seas_plot.pdf"), plot = plotResults(get(t))))
  } else{
    suppressMessages(ggsave(paste0(path, t, "_m2_seas_plot.pdf"), plot = plotResults(formatRES(get(t)))))
  }
  
  print(paste0(v, '-', t, ": tables and plots exported"))
}

print(plotResults(mod_res))
print(paste0("VARIABLE ", v,  " IS COMPLETE"))

# 3.C INTERACTION ANALYSIS 2-way: Treat-Continuous Outcome ####

# Set path for saving data
v <- 'dd10r_score_m_2_treat'
path <- paste0('REPORTING/', v, '/')

# Model (Temporal - no spatial)
lme_mod <- lme(
  fixed = dd10r_score_m ~ Flood_1Lag * treatment  + season_flood
  + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL,  # Controls
  random = list(wcode = pdDiag(~1), c_code = pdDiag(~1)), #, season_id = pdDiag(~1)
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
  data = df,
  na.action = na.omit  # Handle missing data using na.omit
)

# Get results
(mod_res <- getLME(lme_mod))
anova(lme_mod) #  Check ANOVA for statistically strong interactions (marginal evidence)

## 3A. Average Marginal Effects (slopes)
ame1 <- emtrends(lme_mod, pairwise ~ Flood_1Lag*treatment, var = 1)$emtrends
(ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))

## 3B. Estimated Marginal Means (predictions)
# AVERAGE LEVEL OF INUNDATON
emm_1 <- emmeans(lme_mod, ~ Flood_1Lag*treatment, at=list(Flood_1Lag = levels[2])) # 1% increase
(emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
### Summary of contrasts, including CIs
(contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))

# MULTIPLE LEVELS OF INUNDATON
emm_2 <- emmeans(lme_mod, ~ Flood_1Lag*treatment, at = list(Flood_1Lag = levels)) # levels
(emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
### Summary of contrasts, including CIs
(contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
(contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("treatment")), infer = c(TRUE, TRUE)))


## Export Tables & Plots
tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
            'contr_emm2A', 'contr_emm2B')

for(t in tables) {
  
  write.xlsx(get(t), paste0(path , t , "_m2_treat.xlsx"), rowNames=FALSE, fileEncoding = "UTF-8")
  
  if(t=='mod_res'){
    suppressMessages(ggsave(paste0(path, t, "_m2_treat_plot.pdf"), plot = plotResults(get(t))))
  } else{
    suppressMessages(ggsave(paste0(path, t, "_m2_treat_plot.pdf"), plot = plotResults(formatRES(get(t)))))
  }
  
  print(paste0(v, '-', t, ": tables and plots exported"))
}

print(plotResults(mod_res))
print(paste0("VARIABLE ", v,  " IS COMPLETE"))

# 3.D INTERACTION ANALYSIS no-way: Continuous Outcome ####

# Set path for saving data
v <- 'dd10r_score_m_0'
path <- paste0('REPORTING/', v, '/')

# Model (Temporal - no spatial)
lme_mod <- lme(
  fixed = dd10r_score_m ~ Flood_1Lag + season_flood + treatment 
  + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL,  # Controls
  random = list(wcode = pdDiag(~1), c_code = pdDiag(~1)), #, season_id = pdDiag(~1)
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
  data = df,
  na.action = na.omit  # Handle missing data using na.omit
)

# Get results
(mod_res <- getLME(lme_mod))
anova(lme_mod) #  Check ANOVA for statistically strong interactions (marginal evidence)

## 3A. Average Marginal Effects (slopes)
ame1 <- emtrends(lme_mod, pairwise ~ Flood_1Lag, var = 1)$emtrends
(ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))

## 3B. Estimated Marginal Means (predictions)
# AVERAGE LEVEL OF INUNDATON
emm_1 <- emmeans(lme_mod, ~ Flood_1Lag, at=list(Flood_1Lag = levels[2])) # 1% increase
(emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))

# MULTIPLE LEVELS OF INUNDATON
emm_2 <- emmeans(lme_mod, ~ Flood_1Lag, at = list(Flood_1Lag = levels)) # levels
(emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
(contr_emm2A <- summary(contrast(emm_2, "pairwise"), infer = c(TRUE, TRUE)))


## Export Tables & Plots
tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'emm_2_res', 'contr_emm2A')

for(t in tables) {
  
  write.xlsx(get(t), paste0(path , t , "_m0.xlsx"), rowNames=FALSE, fileEncoding = "UTF-8")
  
  if(t=='mod_res'){
    suppressMessages(ggsave(paste0(path, t, "_m0_plot.pdf"), plot = plotResults(get(t))))
  } else{
    suppressMessages(ggsave(paste0(path, t, "_m0_plot.pdf"), plot = plotResults(formatRES(get(t)))))
  }
  
  print(paste0(v, '-', t, ": tables and plots exported"))
}

print(plotResults(mod_res))
print(paste0("VARIABLE ", v,  " IS COMPLETE"))

# # 4.INTERACTION ANALYSIS: Binary Outcomes ####
# # https://stackoverflow.com/questions/63360751/specifying-random-effects-for-repeated-measures-in-logistic-mixed-model-in-r-lm
# 
# variables <- c("dd10r_min_m", "dd10r_vita", "dd10r_legume", "dd10r_nuts", 
#                "dd10r_dairy", "dd10r_flesh", "dd10r_eggs", "dd10r_dglv", 
#                "dd10r_othf", "dd10r_othv")
# 
# for (v in variables) {
#   
#   # Set path for saving data
#   path <- paste0('REPORTING/', v, '/')
#   
#   # Model (Temporal - no spatial)
#   glmm_mod <- glmer(
#     get(v) ~ Flood_1Lag * season_flood * treatment
#     + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL 
#     + (1 | wcode) + (1 | c_code) + (1 | season_id),
#     data = df,
#     family = binomial(),
#     weights = wdiet_wt
#   )
#   
#   # RESULTS
#   mod_res <- getGLMM(glmm_mod, var, 'OR')
# 
#     # MARGINAL EFFECTS
#   ame1 <- emtrends(glmm_mod, pairwise ~ Flood_1Lag*season_flood*treatment, var = 1)$emtrends
#   (ame1_res <- getORs(summary(ame1,  infer = c(TRUE, TRUE)), c('1.trend', 'asymp.LCL', 'asymp.UCL')))
#   # Summary of relevant contrasts, including CIs
#   ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE))
#   (ame1_cont <- getORs(ame1_cont, c('estimate', 'asymp.LCL', 'asymp.UCL')))
#   
#   # MARGINAL MEANS
#   emm_1 <- emmeans(glmm_mod, ~ Flood_1Lag*season_flood*treatment)
#   (emm_1_res <- getORs(summary(emm_1, infer = c(TRUE, TRUE)), c('emmean', 'asymp.LCL', 'asymp.UCL')))
#   ### Summary of contrasts, including CIs
#   contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE))
#   (contr_emm1A <- getORs(contr_emm1A, c('estimate', 'asymp.LCL', 'asymp.UCL')))
#   contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE))
#   (contr_emm1B <- getORs(contr_emm1B, c('estimate', 'asymp.LCL', 'asymp.UCL')))
#   contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE))
#   (contr_emm1C <- getORs(contr_emm1C, c('estimate', 'asymp.LCL', 'asymp.UCL')))
#   
#   #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
#   emm_2 <- emmeans(glmm_mod, ~ Flood_1Lag*season_flood*treatment, at = list(Flood_1Lag = levels)) # levels
#   (emm_2_res <- getORs(summary(emm_2, infer = c(TRUE, TRUE)), c('emmean', 'asymp.LCL', 'asymp.UCL')))
#   ### Summary of contrasts, including CIs
#   (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
#   (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
#   (contr_emm2C <- summary(contrast(emm_2, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
# 
#   # Export tables and plots
#   tables <- c('mod_res', 'ame1_res', 'ame1_cont', 'emm_1_res', 
#               'contr_emm1A', 'contr_emm1B', 'emm_2_res', 
#               'contr_emm2A', 'contr_emm2B', 'contr_emm2C')
#   
#   for(t in tables) {
#     
#     write.csv(get(t), paste0(path , t , ".csv"), row.names=FALSE, fileEncoding = "UTF-8")
#     
#     if(t=='mod_res'){
#       suppressMessages(ggsave(paste0(path, t, "_plot.pdf"), plot = plotResults(get(t), 1)))
#     } else{
#       suppressMessages(ggsave(paste0(path, t, "_plot.pdf"), plot = plotResults(formatRES(get(t)), 1)))
#     }
#     
#     print(paste0(v, "-", t, ": tables and plots exported"))
#   }
#   
#   print(plotResults(mod_res, 1, v))
#   print(paste0("VARIABLE ", v, " IS COMPLETE"))
#   
# }
# 
# warnings

