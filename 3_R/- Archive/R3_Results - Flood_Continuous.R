# RUN 3-WAY INTERACTION (Flood_1Lag - continuous exposure)
# NB: This predictor centered and scaled to represent a 1% increase in cluster flooded (see R0_DataFormating.R)

## IMPORTANT - set path to data folder location
path <- 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/4. Data/REPORTING/Flood_1Lag/'

# !!!! TEST !!!!
# interaction <- '~ (Flood_1Lag + I(Flood_1Lag^2)) * season_flood * treatment'
# https://stats.stackexchange.com/questions/287006/interpreting-interactions-in-a-linear-model-vs-quadratic-model

# 0. PACKAGES & FUNCTIONS ####

# HELPFUL RESOURCES
# https://rdrr.io/cran/emmeans/f/vignettes/basics.Rmd
# https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
# https://stats.stackexchange.com/questions/592518/average-marginal-means-with-marginaleffects?rq=1
# https://www.rdocumentation.org/packages/margins/versions/0.3.26
# Binary Outcomes: https://shouldbewriting.netlify.app/posts/2020-04-13-estimating-and-testing-glms-with-emmeans/#fnref2

# Store times taken to run all models for each variable
times <- data.frame(Variable = character(0), Time = numeric(0))

# Folder locations for each model run
models <- c('m_3/', 'm_2_seas/', 'm_2_treat/', 'm_0/')

# Function to run models that extract results for continuous outcomes


extract_LME <- function(variable, folder) {
  
  if (folder == 'm_3/'){
    
    interaction <- '~ Flood_1Lag * season_flood * treatment'
    formula <- as.formula(paste0(variable, interaction, " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL"))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      data = df,
      na.action = na.omit  # Handle missing data using na.omit
    )
    
    # Get results
    (mod_res <- getLME(lme_mod))
    (anova(lme_mod)) # Check interaction significance 
    
    ## 3A. Average Marginal Effects (slopes)
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)),
                     at=list(Flood_1Lag = levels[2]), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
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
      random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      data = df,
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
  else if (folder == 'm_2_treat/'){
    
    interaction <- '~ Flood_1Lag * treatment'
    formula <- as.formula(paste0(variable, interaction, " + season_flood + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL"))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      data = df,
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
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_1Lag = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("treatment")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_0/'){
    
    interaction <- '~ Flood_1Lag'
    formula <- as.formula(paste0(variable, interaction, " + season_flood + treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL"))
    lme_mod <- lme(
      fixed = formula,  # Controls
      random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      data = df,
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

# Function to run models that extract results for binary outcomes
extract_GLMER <- function(variable, folder) {
  
  if (folder == 'm_3/'){
    
    interaction <- '~ Flood_1Lag * season_flood * treatment'
    covar <- " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 + wcode|season_id) + (1 | c_code)"
    formula <- as.formula(paste0(variable, interaction, covar))
    glmm_mod <- glmer(
      formula=formula,
      weights = wdiet_wt,
      data = df,
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
    (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions) - as Probabilities
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(glmm_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2]),
                     trans = "response") # probabilities
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_1Lag = levels),
                     trans = "response") # probabilities
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
    covar <- " + treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 + wcode|season_id) + (1 | c_code)"
    formula <- as.formula(paste0(variable, interaction, covar))
    glmm_mod <- glmer(
      formula=formula,
      weights = wdiet_wt,
      data = df,
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
  else if (folder == 'm_2_treat/'){
    
    interaction <- '~ Flood_1Lag * treatment'
    covar <- " + season_flood + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 + wcode|season_id) + (1 | c_code)"
    formula <- as.formula(paste0(variable, interaction, covar))
    glmm_mod <- glmer(
      formula=formula,
      weights = wdiet_wt,
      data = df,
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
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_1Lag = levels),
                     trans = "response") # probabilities
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("treatment")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_0/'){
    
    interaction <- '~ Flood_1Lag'
    covar <- " + season_flood + treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 + wcode|season_id) + (1 | c_code)"
    formula <- as.formula(paste0(variable, interaction, covar))
    glmm_mod <- glmer(
      formula=formula,
      weights = wdiet_wt,
      data = df,
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
    write.xlsx(get(t, envir = environment()), paste0(path, variable, '_', toupper(t) , '_', sub('/', '', folder), '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
    
    if(t=='mod_res'){
      suppressMessages(ggsave(paste0(path , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(get(t, envir = environment()), 1)))
    } else{
      suppressMessages(ggsave(paste0(path , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(formatRES(get(t, envir = environment())), 1)))
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
    extract_LME(v, m)
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
    extract_GLMER(v, m)
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

# # Notes: Interaction model with continuous outcome  ####
# 
# formula <- as.formula("dd10r_score_m ~ Flood_1Lag * season_flood * treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL")
# 
# lme_mod <- lme(
#   fixed = formula,  # Controls
#   random = list(wcode = pdDiag(~1), c_code = pdDiag(~1), season_id = pdDiag(~1)),
#   weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
#   # correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
#   data = df,
#   na.action = na.omit  # Handle missing data using na.omit
# )
# 
# # Get results
# (mod_res <- getLME(lme_mod))
# (anova(lme_mod)) # Check interaction significance
# 
# ## 3A. Average Marginal Effects (slopes)
# ## EMTRENDS: Estimates the slope of coefficients, accounting for reference groups.
# ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', '~ Flood_1Lag * season_flood * treatment')),
#                  at=list(Flood_1Lag = levels[2]), var = 1)$emtrends
# (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
# ### Summary of contrasts, including CIs
# (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
# 
# # # EXAMPLE MANUAL CHECKS
# # -0.0946 # Flood:Cont:Jan/Feb [flood(Jan/Feb)]
# # -0.0946 + -0.5741 # Flood:Cont:Mar/Apr [flood(Jan/Feb)  + flood:season(Mar/Apr)]
# # -0.0946 +  0.2578 # Flood:Cont:May/Jun [flood(Jan/Feb)  + flood:season(May/Jun)]
# # # etc.
# # -0.0946 + -0.1219 # Flood:Treat:Jan/Feb [flood + flood:treat]
# # -0.0946 + -0.1219 + -0.5741 + -0.2799 # Flood:Treat:Mar/Apr [flood(Jan/Feb) + treat:season(Mar/Apr)) + flood:season(Mar/Apr) + flood:season:treat(Mar/Apr)]
# # -0.0946 + -0.1219 + 0.2578 + 0.0135 # Flood:Treat:May/Jun [flood(Jan/Feb) + treat:season(Jan/Feb)) + flood:season(May/Jun) + flood:season:treat(May/Jun)]
# # # etc.
# 
# ## 3B. Estimated Marginal Means (predictions)
# ## EMMEANS: Estimates the predicted value of WDDS for difference coef values, accounting for reference groups.
# # MARGINAL MEANS - 1% increase
# emm_1 <- emmeans(lme_mod, as.formula('~ Flood_1Lag * season_flood * treatment'), at=list(Flood_1Lag = levels[2])) # 1% increase
# (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
# ### Summary of contrasts, including CIs
# (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
# (contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
# (contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
# # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
# emm_2 <- emmeans(lme_mod, as.formula('~ Flood_1Lag * season_flood * treatment'), at = list(Flood_1Lag = levels)) # levels 
# (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
# ### Summary of contrasts, including CIs
# (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
# (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
# (contr_emm2C <- summary(contrast(emm_2, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
# 
# ## Export Tables & Plots
# tables <- c('mod_res', 'ame1_res', 'ame1_cont', 'emm_1_res', 
#             'contr_emm1A', 'contr_emm1B', 'emm_2_res', 
#             'contr_emm2A', 'contr_emm2B', 'contr_emm2C')
# 
# folder <- 'm_3'
# variable <- 'dd10r_score_m'
# 
# for(t in tables) {
#   
#   write.xlsx(get(t), paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
#   
#   if(t=='mod_res'){
#     suppressMessages(ggsave(paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(get(t))))
#   } else{
#     suppressMessages(ggsave(paste0(folder , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(formatRES(get(t)))))
#   }
#   
#   # print(paste0(variable, '-', t, ": tables and plots exported"))
#   print(get(t))
#   assign(t, get(t), envir = .GlobalEnv)
#   
# }
# 
# print(plotResults(mod_res))
# print(paste0("VARIABLE ", variable, "/", folder, "IS COMPLETE"))

# 
# interaction <- '~ Flood_1Lag * season_flood * treatment'
# formula <- as.formula(paste0('dd10r_score_m', interaction,
#                              " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 | wcode) + (1 | c_code) + (1 | season_id)"))
# glmm_mod <- glmer(
#   formula=formula,
#   weights = wdiet_wt,
#   data = df,
#   family = gaussian(link = "identity")) # Removes non-convergence warnings
# 
# (mod_res <- getGLMM(glmm_mod))
# summary(glmm_mod, infer = c(TRUE, TRUE))
# ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', '~ Flood_1Lag * season_flood * treatment')),
#                  at=list(Flood_1Lag = levels[2]), var = 1, pbkrtest.limit = 21561)$emtrends
# print(ame1)
# library(lmerTest)
#


# # Notes: Interaction model with binary outcome  ####
# 
# # Run model
# interaction <- '~ Flood_1Lag * season_flood * treatment'
# formula <- as.formula(paste0('dd10r_min_m', interaction,
#                              " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 + wcode|season_id) + (1 | c_code)"))
# glmm_mod <- glmer(
#   formula=formula,
#   weights = wdiet_wt,
#   data = df,
#   family = binomial(link="logit"), # extracts binomial regression on logit scale
#   control=glmerControl(optimizer="bobyqa") # Removes non-convergence warnings
# )
# 
# # Get results - as Odds Ratios
# (mod_res <- getGLMM(glmm_mod, 0, 'OR'))
# 
# ## 3A. Average Marginal Effects (slopes) - as Probabilities
# ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', interaction)),
#                  at=list(Flood_1Lag = levels[2]), var = 1,
#                  trans='response')$emtrends # probabilities
# (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
# (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
# 
# ## 3B. Estimated Marginal Means (predictions) - as Probabilities
# # MARGINAL MEANS - 1% increase
# emm_1 <- emmeans(glmm_mod, as.formula(interaction), at=list(Flood_1Lag = levels[2]),
#                  trans = "response") # probabilities
# (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
# (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
# (contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
# (contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
# #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
# emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_1Lag = levels),
#                  trans = "response") # probabilities
# (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
# (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
# (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
# (contr_emm2C <- summary(contrast(emm_2, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
# 
# variable <- 'dd10r_min_m'
# folder <- 'm_3/'
# 
# tables <- c('mod_res', 'ame1_res', 'ame1_cont', 'emm_1_res',
#             'contr_emm1A', 'contr_emm1B', 'emm_2_res',
#             'contr_emm2A', 'contr_emm2B', 'contr_emm2C')
# 
# path <- paste0(variable, "/", folder)
# 
# for(t in tables) {
# 
#   write.xlsx(get(t), paste0(path, variable, '_', toupper(t) , '_', sub('/', '', folder), '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
# 
#   if(t=='mod_res'){
#     suppressMessages(ggsave(paste0(path , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(get(t), 1)))
#   } else{
#     suppressMessages(ggsave(paste0(path , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(formatRES(get(t)), 1)))
#   }
# 
#   # print(paste0(variable, '-', t, ": tables and plots exported"))
#   print(get(t))
# 
# }
# formula <- as.formula("dd10r_min_m ~ Flood_1Lag * season_flood * treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL")
# 
# 
# library(aomisc)
# hillfunc <- function(Time, a, b, c) {
#   1 - ( ( (1-a) * Time^b) / ( 10^c + (Time)^b ) )
# }
# gd <- groupedData(dd10r_min_m~season_id|wcode, data=df)
# 
# nlme_mod <- nlme(
#   model=dd10r_score_m ~ hillfunc(Flood_1Lag, season_flood, treatment, dd10r_score_m_BL, ramadan, g_2h_BL, quint2_BL),
#   fixed=Flood_1Lag * season_flood * treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL~1,
#   # random=pdDiag(list(c_code~1)),
#   # fixed = list(dd10r_score_m_BL = pdDiag(~1), ramadan = pdDiag(~1), g_2h_BL = pdDiag(~1), quint2_BL = pdDiag(~1)),
#   # random = wcode ~ 1, #list(wcode = pdDiag(~1), c_code = pdDiag(~1), season_id = pdDiag(~1))
#   # weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
#   # correlation = corAR1(form = ~ season_id ),  # Adding temporal autocorrelation
#   data = gd,
#   na.action = na.omit,  # Handle missing data using na.omit
#   start = c(Flood_1Lag=0, season_flood=0, treatment=0, dd10r_score_m_BL=1, ramadan=1, g_2h_BL=1, quint2_BL=1))
