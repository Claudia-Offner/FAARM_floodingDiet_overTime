# RUN 3-WAY INTERACTION (FREQUENTIST- Binary)

## IMPORTANT - set file path to data folder location
setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/4. Data/REPORTING/Model Outputs - Food Groups/')
# Suppress warnings
options(warn = -1) 
options(scipen=999) # Turn off scientific notation

# Emmeans probabilities vs risk ratios
# https://shouldbewriting.netlify.app/posts/2020-04-13-estimating-and-testing-glms-with-emmeans/#fnref2

# 0. PACKAGES & FUNCTIONS ####

# HELPFUL RESOURCES
# https://rdrr.io/cran/emmeans/f/vignettes/basics.Rmd
# https://cran.r-project.org/web/packages/emmeans/vignettes/AQuickStart.html
# https://stats.stackexchange.com/questions/592518/average-marginal-means-with-marginaleffects?rq=1
# https://www.rdocumentation.org/packages/margins/versions/0.3.26

extract_GLMER <- function(variable, folder) {
  
  if (folder == 'm_3/'){
    
    interaction <- '~ Flood_1Lag * season_flood * treatment'
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
                     at=list(Flood_1Lag = levels[2]), var = 'Flood_1Lag',
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
                     at=list(Flood_1Lag = levels[2]), var = 'Flood_1Lag',
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
                     at=list(Flood_1Lag = levels[2]), var = 'Flood_1Lag',
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
    
    interaction <- '~ Flood_1Lag '
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
                     at=list(Flood_1Lag = levels[2]), var = 'Flood_1Lag',
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
  path <- paste0(variable, "/", folder)
  for(t in tables) {
    
    write.xlsx(get(t), paste0(path, variable, '_', toupper(t) , '_', sub('/', '', folder), '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
    
    if(t=='mod_res'){
      suppressMessages(ggsave(paste0(path , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(get(t), 1)))
    } else{
      suppressMessages(ggsave(paste0(path , variable, '_', toupper(t) , '_', sub('/', '', folder), '_plot.pdf'), plot = plotResults(formatRES(get(t)))))
    }
    
    # print(paste0(variable, '-', t, ": tables and plots exported"))
    print(get(t))
    assign(t, get(t), envir = .GlobalEnv)
    
  }
  
  print(plotResults(mod_res, 1))
  print(paste0("VARIABLE ", variable, "/", folder, "IS COMPLETE"))
  
}

# 1. Run models for binary variables ####

variables <- c("dd10r_min_m", "dd10r_vita", "dd10r_legume", "dd10r_nuts",
               "dd10r_dairy", "dd10r_flesh", "dd10r_eggs", "dd10r_dglv",
               "dd10r_othf", "dd10r_othv")

models <- c('m_3/', 'm_2_seas/', 'm_2_treat/', 'm_0/')

# For every binary variable
for (v in variables){
  # Run every model
  for (m in models) {
    
    extract_GLMER(v, m)
    
  }
}


# Example Interaction Model - with notes ####

# NB: Update all ame codes to be set to 1% changes


# Run model                
interaction <- '~ Flood_1Lag * season_flood * treatment'
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

## 3A. Average Marginal Effects (slopes) - as Probabilities
ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', interaction)), 
                 at=list(Flood_1Lag = levels[2]), var = 'Flood_1Lag',
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




