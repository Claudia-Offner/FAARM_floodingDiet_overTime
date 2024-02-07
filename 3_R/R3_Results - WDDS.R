# RUN 3-WAY INTERACTION (FREQUENTIST- Continuous)

## IMPORTANT - set file folder to data folder location
setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/4. Data/REPORTING/Model Outputs - WDDS/')
# Suppress warnings
options(warn = -1) 

# 0. PACKAGES & FUNCTIONS ####

# HELPFUL RESOURCES
# https://rdrr.io/cran/emmeans/f/vignettes/basics.Rmd
# https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
# https://stats.stackexchange.com/questions/592518/average-marginal-means-with-marginaleffects?rq=1
# https://www.rdocumentation.org/packages/margins/versions/0.3.26

extract_LME <- function(variable, folder) {
  
  if (folder == 'm_3/'){
    
    interaction <- '~ Flood_1Lag * season_flood * treatment'
    formula <- as.formula(paste0(variable, interaction, " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL"))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1), c_code = pdDiag(~1), season_id = pdDiag(~1)), #, season_id = pdDiag(~1)
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
      data = df,
      na.action = na.omit  # Handle missing data using na.omit
    )
    
    # Get results
    (mod_res <- getLME(lme_mod))
    (anova(lme_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes)
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # AVERAGE LEVEL OF INUNDATON
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    # MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_1Lag = levels)) # levels 
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm2C <- summary(contrast(emm_2, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'ame1_cont', 'emm_1_res', 
                'contr_emm1A', 'contr_emm1B', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B', 'contr_emm2C')
    
  } 
  else if (folder == 'm_2_seas/'){
    
    interaction <- '~ Flood_1Lag * season_flood'
    formula <- as.formula(paste0(variable, interaction, " + treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL"))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1), c_code = pdDiag(~1), season_id = pdDiag(~1)), #, season_id = pdDiag(~1)
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
      data = df,
      na.action = na.omit  # Handle missing data 
    )
    
    # Get results
    (mod_res <- getLME(lme_mod))
    (anova(lme_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes)
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # AVERAGE LEVEL OF INUNDATON
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise"), infer = c(TRUE, TRUE)))
    # MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_1Lag = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("season_flood")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_2_treat/'){
    
    interaction <- '~ Flood_1Lag * treatment'
    formula <- as.formula(paste0(variable, interaction, "+ season_flood + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL"))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1), c_code = pdDiag(~1), season_id = pdDiag(~1)), #, season_id = pdDiag(~1)
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
      data = df,
      na.action = na.omit  # Handle missing data
    )
    
    # Get results
    (mod_res <- getLME(lme_mod))
    (anova(lme_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes)
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # AVERAGE LEVEL OF INUNDATON
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    # MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_1Lag = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("treatment")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_0/'){
    
    interaction <- '~ Flood_1Lag '
    formula <- as.formula(paste0(variable, interaction, " + season_flood + treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL"))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1), c_code = pdDiag(~1), season_id = pdDiag(~1)), #, season_id = pdDiag(~1)
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
      data = df,
      na.action = na.omit  # Handle missing data using na.omit
    )
    
    # Get results
    (mod_res <- getLME(lme_mod))
    (anova(lme_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes)
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # AVERAGE LEVEL OF INUNDATON
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    # MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_1Lag = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise"), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'emm_2_res', 'contr_emm2A')
    
  }
  
  ## Export Tables & Plots
  for(t in tables) {
    
    write.xlsx(get(t), paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
    
    if(t=='mod_res'){
      suppressMessages(ggsave(paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(get(t))))
    } else{
      suppressMessages(ggsave(paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(formatRES(get(t)))))
    }
    
    # print(paste0(variable, '-', t, ": tables and plots exported"))
    print(get(t))
    assign(t, get(t), envir = .GlobalEnv)
    
  }
  
  print(plotResults(mod_res))
  print(paste0("VARIABLE ", variable, "/", folder, "IS COMPLETE"))
  
}

# 1. Run models for continuous variables ####

models <- c('m_3/', 'm_2_seas/', 'm_2_treat/', 'm_0/')

# Run every model
for (m in models) {
  extract_LME('dd10r_score_m', m)
}



# Example Interaction Model - with notes ####

formula <- as.formula("dd10r_score_m ~ Flood_1Lag * season_flood * treatment 
                      + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL")

lme_mod <- lme(
  fixed = formula,  # Controls
  random = list(wcode = pdDiag(~1), c_code = pdDiag(~1), season_id = pdDiag(~1)), #, season_id = pdDiag(~1)
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  # correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
  data = df,
  na.action = na.omit  # Handle missing data using na.omit
)


# Get results
(mod_res <- getLME(lme_mod))
#  Check ANOVA for statistically strong interactions (some evidence)
anova(lme_mod) 

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
  
  name <- paste0('dd10r_score_m_', t)
 
  write.xlsx(get(t), paste0(folder , name, '_m3.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
  
  if(t=='mod_res'){
    suppressMessages(ggsave(paste0(folder, name, '_m3_plot.pdf'), plot = plotResults(get(t))))
  } else{
    suppressMessages(ggsave(paste0(folder, name, '_m3_plot.pdf'), plot = plotResults(formatRES(get(t)))))
  }
  
  print(paste0(v, '-', folder, t, ": tables and plots exported"))
}

print(plotResults(mod_res))
print( paste0("VARIABLE",v, '-', folder, "IS COMPLETE"))


# Example Interaction Model - VanderWeele ####

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



