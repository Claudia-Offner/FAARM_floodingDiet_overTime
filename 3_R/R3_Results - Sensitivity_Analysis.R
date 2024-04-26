# RUN 3-WAY INTERACTION (Flood_1Lag - continuous exposure)
# NB: This file runs models F1 and F2 (season) for the control group ONLY

## IMPORTANT - set path to data folder location
path <- 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/4. Data/REPORTING/Sens_Anlys/'

# Subset DF for control
df_FULL <- df
df_CON <-df[df$treatment==0,]


# 0. PACKAGES & FUNCTIONS ####

# Store times taken to run all models for each variable
times <- data.frame(Variable = character(0), Time = numeric(0))

# Identify levels for predictor (based on center & scale)
levels <- c((0 - mean_value)/0.01,
            (0.01 - mean_value)/0.01, 
            (0.05 - mean_value)/0.01,
            (0.1 - mean_value)/0.01,
            (0.2 - mean_value)/0.01)

# Folder locations for each model run
models <- c('m_2_seas/', 'm_0/')

# FUNCTION to run models that extract results for continuous outcomes
## Adapted, so only relevant models included, without treatment
extract_LME <- function(variable, folder, dataset, treat='y') {
  
  
  if (folder == 'm_2_seas/'){
    
    interaction <- '~ Flood_1Lag * season_flood'
    covar <- ' + treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL'
    if (treat != 'y'){
      covar <- gsub("\\+ treatment ", "", covar)
    }
    formula <- as.formula(paste0(variable, interaction, covar))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      data = dataset,
      na.action = na.omit  # Handle missing data 
    )
    
    # Get results
    (mod_res <- getLME(lme_mod))
    (anova(lme_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes)
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)),
                     at=list(Flood_1Lag = levels[2]), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise"), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_1Lag = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("season_flood")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_0/'){
    
    interaction <- '~ Flood_1Lag'
    covar <- ' + treatment + season_flood + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL'
    if (treat != 'y'){
      covar <- gsub("\\+ treatment ", "", covar)
    }
    formula <- as.formula(paste0(variable, interaction, covar))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      data = dataset,
      na.action = na.omit  # Handle missing data using na.omit
    )
    
    # Get results
    (mod_res <- getLME(lme_mod))
    (anova(lme_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes)
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)),
                     at=list(Flood_1Lag = levels[2]), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_1Lag = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise"), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'emm_2_res', 'contr_emm2A')
    
  }
  
  ## Export Tables & Plots
  # assign('lme_mod', lme_mod, envir = .GlobalEnv)
  
  for(t in tables) {
    
    write.xlsx(get(t), paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
    
    if(t=='mod_res'){
      suppressMessages(ggsave(paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(get(t))))
    } else{
      suppressMessages(ggsave(paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(formatRES(get(t)))))
    }
    
    # print(paste0(variable, '-', t, ": tables and plots exported"))
    print(get(t))
    # assign(t, get(t), envir = .GlobalEnv)
    
  }
  
  print(plotResults(mod_res))
  print(paste0("VARIABLE ", variable, "/", folder, "IS COMPLETE"))
  
}

# FUNCTION to run models that extract results for binary outcomes
## Adapted, so only relevant models included, without treatment
extract_GLMER <- function(variable, folder, dataset, treat='y') {
  
  
  if (folder == 'm_2_seas/'){
    
    interaction <- '~ Flood_1Lag * season_flood'
    covar <- ' + treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 + wcode|season_id) + (1 | c_code)'
    if (treat != 'y'){
      covar <- gsub("\\+ treatment ", "", covar)
    }
    formula <- as.formula(paste0(variable, interaction, covar))
    glmm_mod <- glmer(
    formula=formula,
    weights = wdiet_wt,
    data = dataset,
    family = binomial(link="logit"), # extracts binomial regression on logit scale
    control=glmerControl(optimizer="bobyqa") # Removes non-convergence warnings
    )
    
    # Get results - as Odds Ratios
    (mod_res <- getGLMM(glmm_mod, 0, 'OR'))
    (anova(glmm_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes) - as Probabilities
    ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', interaction)), 
                     at=list(Flood_1Lag = levels[2]), var = 1,
                     trans='response')$emtrends # probabilities
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions) - as Probabilities
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(glmm_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2]),
                     trans = "response") # probabilities
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise"), infer = c(TRUE, TRUE)))
    #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_1Lag = levels),
                     trans = "response") # probabilities
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("season_flood")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_0/'){
    
    interaction <- '~ Flood_1Lag'
    covar <- ' + treatment + season_flood + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 + wcode|season_id) + (1 | c_code)'
    if (treat != 'y'){
      covar <- gsub("\\+ treatment ", "", covar)
    }
    formula <- as.formula(paste0(variable, interaction, covar))
    glmm_mod <- glmer(
    formula=formula,
    weights = wdiet_wt,
    data = dataset,
    family = binomial(link="logit"), # extracts binomial regression on logit scale
    control=glmerControl(optimizer="bobyqa") # Removes non-convergence warnings
    )
    
    # Get results - as Odds Ratios
    (mod_res <- getGLMM(glmm_mod, 0, 'OR'))
    (anova(glmm_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes) - as Probabilities
    ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', interaction)), 
                     at=list(Flood_1Lag = levels[2]), var = 1,
                     trans='response')$emtrends # probabilities
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions) - as Probabilities
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(glmm_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2]),
                     trans = "response") # probabilities
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_1Lag = levels),
                     trans = "response") # probabilities
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise"), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'emm_2_res', 'contr_emm2A')
    
  }
  
  ## Export Tables & Plots
  assign('glmm_mod', glmm_mod, envir = .GlobalEnv)
  
  path <- paste0(variable, "/", folder)
  for(t in tables) {
    assign(t, get(t, envir = environment()), envir = .GlobalEnv)
    write.xlsx(get(t, envir = environment()), paste0(path, sub('dd10r_', '', variable), '_', toupper(t) , '_', sub('/', '', folder), '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
    
    if(t=='mod_res'){
      suppressMessages(ggsave(paste0(path , sub('dd10r_', '', variable), '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(get(t, envir = environment()), 1)))
    } else{
      suppressMessages(ggsave(paste0(path , sub('dd10r_', '', variable), '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(formatRES(get(t, envir = environment())), 1)))
    }
    
    # print(paste0(variable, '-', t, ": tables and plots exported"))
    print(get(t))
    
  }
  
  print(plotResults(mod_res, 1))
  print(paste0("VARIABLE ", variable, "/", folder, "IS COMPLETE"))
  
}


# 1. Run models for continuous variables ####

# SET PATH & VARIABLES
setwd(paste0(path, 'Model Outputs - WDDS/'))
cont_variables <- c('dd10r_score_m')

# EXTRACT RESULTS
for (v in cont_variables){
  # For every continuous variable
  start_time <- Sys.time()
  for (m in models) {
    # Run every model
    extract_LME(v, m, df_CON, treat='n')
  }
  end_time <- Sys.time()
  # Print timings
  total_time <- end_time - start_time
  times <- rbind(times, data.frame(Variable = v, Time = total_time))
  print(paste0('TIME TO RUN ', v, ' MODELS: ', total_time))
}


# 2. Run models for binary variables ####

# SET PATH & VARIABLES
setwd(paste0(path, 'Model Outputs - Food Groups/'))
bin_variables <- c("dd10r_othv", "dd10r_othf", "dd10r_dglv",
                   "dd10r_vita", "dd10r_legume", "dd10r_nuts",
                   "dd10r_dairy", "dd10r_eggs", "dd10r_flesh",
                   "dd10r_min_m", "dd10r_starch")


# EXTRACT RESULTS
for (v in bin_variables){
  # For every binary variable
  start_time <- Sys.time()
  for (m in models) {
    # Run every model
    extract_GLMER(v, m, df_CON, treat='n')
  }
  end_time <- Sys.time()
  # Print timings
  total_time <- end_time - start_time
  times <- rbind(times, data.frame(Variable = v, Time = total_time))
  print(paste0('TIME TO RUN ', v, ' MODELS: ', total_time))
}

# COMPARISON OF RUNNING TIMES
setwd(path)
(times)
write.xlsx(times, 'times.xlsx', rowNames=FALSE, fileEncoding = "UTF-8")

