# RUN 3-WAY INTERACTION (Flood_SThresh - categorical exposure)
# NB: This predictor was created based on seasonal means & SDs (see R0_DataFormating.R)

## IMPORTANT - set path to data folder location
path <- 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/4. Data/REPORTING/Flood_SThresh/'


# 0. PACKAGES & FUNCTIONS ####

# HELPFUL RESOURCES
# https://rdrr.io/cran/emmeans/f/vignettes/basics.Rmd
# https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
# https://stats.stackexchange.com/questions/592518/average-marginal-means-with-marginaleffects?rq=1
# https://www.rdocumentation.org/packages/margins/versions/0.3.26
# Binary Outcomes: https://shouldbewriting.netlify.app/posts/2020-04-13-estimating-and-testing-glms-with-emmeans/#fnref2

# Store times taken to run all models for each variable
times <- data.frame(Variable = character(0), Time = numeric(0))

# Identify levels for predictor
levels <- c(0, 1, 2, 3, 4)

# Folder locations for each model run
models <- c('m_3/', 'm_2_seas/', 'm_2_treat/', 'm_0/')

# Function to run models that extract results for continuous outcomes
extract_LME <- function(variable, folder) {
  
  if (folder == 'm_3/'){
    
    interaction <- '~ Flood_SThresh * season_flood * treatment'
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
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)),
                     at=list(Flood_SThresh = levels[3]), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_SThresh", "season_flood")), infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_SThresh = levels[3])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_SThresh", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_SThresh", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_SThresh = levels)) # levels 
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_SThresh", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("Flood_SThresh", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm2C <- summary(contrast(emm_2, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'ame1_cont', 'emm_1_res', 
                'contr_emm1A', 'contr_emm1B', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B', 'contr_emm2C')
    
  } 
  else if (folder == 'm_2_seas/'){
    
    interaction <- '~ Flood_SThresh * season_flood'
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
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)),
                     at=list(Flood_SThresh = levels[3]), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_SThresh = levels[3])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise"), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_SThresh = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_SThresh")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("season_flood")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_2_treat/'){
    
    interaction <- '~ Flood_SThresh * treatment'
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
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)),
                     at=list(Flood_SThresh = levels[3]), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_SThresh = levels[3])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_SThresh")), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_SThresh = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_SThresh")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("treatment")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_0/'){
    
    interaction <- '~ Flood_SThresh '
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
    ame1 <- emtrends(lme_mod, as.formula(paste0('pairwise ', interaction)),
                     at=list(Flood_SThresh = levels[3]), var = 1)$emtrends
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(lme_mod, as.formula(interaction), at=list(Flood_SThresh = levels[3])) # 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(lme_mod, as.formula(interaction), at = list(Flood_SThresh = levels)) # levels
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise"), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'emm_2_res', 'contr_emm2A')
    
  }
  
  ## Export Tables & Plots
  assign('lme_mod', lme_mod, envir = .GlobalEnv)
  
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

# Function to run models that extract results for binary outcomes
extract_GLMER <- function(variable, folder) {
  
  if (folder == 'm_3/'){
    
    interaction <- '~ Flood_SThresh * season_flood * treatment'
    formula <- as.formula(paste0('dd10r_min_m', interaction, 
                                 " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 | wcode) + (1 | c_code) + (1 | season_id)"))
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
                     at=list(Flood_SThresh = levels[3]), var = 1,
                     trans='response')$emtrends # probabilities
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_SThresh", "season_flood")), infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions) - as Probabilities
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(glmm_mod, as.formula(interaction), at=list(Flood_SThresh = levels[3]),
                     trans = "response") # probabilities
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_SThresh", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_SThresh", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_SThresh = levels),
                     trans = "response") # probabilities
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_SThresh", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("Flood_SThresh", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm2C <- summary(contrast(emm_2, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    
    
    tables <- c('mod_res', 'ame1_res', 'ame1_cont', 'emm_1_res', 
                'contr_emm1A', 'contr_emm1B', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B', 'contr_emm2C')
    
  } 
  else if (folder == 'm_2_seas/'){
    
    interaction <- '~ Flood_SThresh * season_flood'
    formula <- as.formula(paste0('dd10r_min_m', interaction, 
                                 " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 | wcode) + (1 | c_code) + (1 | season_id)"))
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
                     at=list(Flood_SThresh = levels[3]), var = 1,
                     trans='response')$emtrends # probabilities
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions) - as Probabilities
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(glmm_mod, as.formula(interaction), at=list(Flood_SThresh = levels[3]),
                     trans = "response") # probabilities
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise"), infer = c(TRUE, TRUE)))
    #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_SThresh = levels),
                     trans = "response") # probabilities
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_SThresh")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("season_flood")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_2_treat/'){
    
    interaction <- '~ Flood_SThresh * treatment'
    formula <- as.formula(paste0('dd10r_min_m', interaction, 
                                 " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 | wcode) + (1 | c_code) + (1 | season_id)"))
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
                     at=list(Flood_SThresh = levels[3]), var = 1,
                     trans='response')$emtrends # probabilities
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions) - as Probabilities
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(glmm_mod, as.formula(interaction), at=list(Flood_SThresh = levels[3]),
                     trans = "response") # probabilities
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_SThresh")), infer = c(TRUE, TRUE)))
    #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_SThresh = levels),
                     trans = "response") # probabilities
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_SThresh")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("treatment")), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'contr_emm1A', 'emm_2_res', 
                'contr_emm2A', 'contr_emm2B')
    
  } 
  else if (folder == 'm_0/'){
    
    interaction <- '~ Flood_SThresh '
    formula <- as.formula(paste0('dd10r_min_m', interaction, 
                                 " + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL + (1 | wcode) + (1 | c_code) + (1 | season_id)"))
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
                     at=list(Flood_SThresh = levels[3]), var = 1,
                     trans='response')$emtrends # probabilities
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    
    ## 3B. Estimated Marginal Means (predictions) - as Probabilities
    # MARGINAL MEANS - 1% increase
    emm_1 <- emmeans(glmm_mod, as.formula(interaction), at=list(Flood_SThresh = levels[3]),
                     trans = "response") # probabilities
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    #  MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    emm_2 <- emmeans(glmm_mod, as.formula(interaction), at = list(Flood_SThresh = levels),
                     trans = "response") # probabilities
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise"), infer = c(TRUE, TRUE)))
    
    tables <- c('mod_res', 'ame1_res', 'emm_1_res', 'emm_2_res', 'contr_emm2A')
    
  }
  
  ## Export Tables & Plots
  assign('glmm_mod', glmm_mod, envir = .GlobalEnv)
  
  path <- paste0(variable, "/", folder)
  for(t in tables) {
    
    write.xlsx(get(t), paste0(path, variable, '_', toupper(t) , '_', sub('/', '', folder), '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
    
    if(t=='mod_res'){
      suppressMessages(ggsave(paste0(path , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(get(t), 1)))
    } else{
      suppressMessages(ggsave(paste0(path , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(formatRES(get(t)), 1)))
    }
    
    # print(paste0(variable, '-', t, ": tables and plots exported"))
    print(get(t))
    assign(t, get(t), envir = .GlobalEnv)
    
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
bin_variables <- c("dd10r_vita", "dd10r_legume", "dd10r_nuts", #"dd10r_min_m",
                   "dd10r_dairy", "dd10r_flesh", "dd10r_eggs", "dd10r_dglv",
                   "dd10r_othf", "dd10r_othv")

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

# Notes: Ordinal Exposure ####

## Only difference from continuous script is that Flood_1Lag is now  
## Flood_SThresh and levels have been changed to match that variable
