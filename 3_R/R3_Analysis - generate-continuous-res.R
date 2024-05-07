################################################################################
#### GENERATE RESULTS (CONTINUOUS EXPOSURE)  #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/3. Analysis/I. Results')

#### IMPORTANT - Run R0_Data_formatting first

times <- data.frame(Variable = character(0), Time = numeric(0))
outcomes_cont <- c("dd10r_score_m")
outcomes_bin <- c("dd10r_min_m", "dd10r_flesh", "dd10r_dairy", "dd10r_eggs",
                  "dd10r_dglv", "dd10r_vita", "dd10r_othv", "dd10r_othf",
                  "dd10r_legume", "dd10r_nuts") 

# FUNCTIONS
get_results <- function(model, outcome, dtype, interaction) {
 
  # Get calculations based to outcome data type
  if(dtype=='cont'){
    (mod_res <- getLME(model))
    # Extract as estimates
    ame1 <- emtrends(model, as.formula(paste0('pairwise ', interaction)), at=list(Flood_1Lag = levels[2]), var = 1)$emtrends
    emm_1 <- emmeans(model, as.formula(interaction), at=list(Flood_1Lag = levels[2])) # 1% increase
    emm_2 <- emmeans(model, as.formula(interaction), at = list(Flood_1Lag = levels)) # levels 
    print(plotResults(mod_res))
    
  } else if (dtype=='bin'){
    (mod_res <- getGLMM(model, 0, 'PROBS'))
    # Extract as probabilities (i.e. trans = "response")
    ame1 <- emtrends(model, as.formula(paste0('pairwise ', interaction)), at=list(Flood_1Lag = levels[2]), var = 1, trans='response')$emtrends
    emm_1 <- emmeans(model, as.formula(interaction), at=list(Flood_1Lag = levels[2]), trans = "response") # 1% increase
    emm_2 <- emmeans(model, as.formula(interaction), at = list(Flood_1Lag = levels), trans = "response") # levels 
    print(plotResults(mod_res, 1))
  }
  
  # Extract tables based on interaction combo
  if(interaction=='~ Flood_1Lag * season_flood * treatment'){
    
    ## A. Average Marginal Effects (slopes)
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    ## B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm1B <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm1C <- summary(contrast(emm_1, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag", "treatment")), infer = c(TRUE, TRUE)))
    (contr_emm2C <- summary(contrast(emm_2, "pairwise", by = c("season_flood", "treatment")), infer = c(TRUE, TRUE)))
    # List tables
    tables <- c('mod_res', 'ame1_res', 'ame1_cont', 
                'emm_1_res', 'contr_emm1A', 'contr_emm1B', 
                'emm_2_res', 'contr_emm2A', 'contr_emm2B', 'contr_emm2C')
    
  } else if (interaction=='~ Flood_1Lag * season_flood') {
    
    ## A. Average Marginal Effects (slopes)
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    ## B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise"), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("season_flood")), infer = c(TRUE, TRUE)))
    # List tables
    tables <- c('mod_res', 'ame1_res', 
                'emm_1_res', 'contr_emm1A', 
                'emm_2_res', 'contr_emm2A', 'contr_emm2B')
    
  } else if (interaction=='~ Flood_1Lag * treatment') {
    
    ## A. Average Marginal Effects (slopes)
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    ## B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    (contr_emm1A <- summary(contrast(emm_1, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise", by = c("Flood_1Lag")), infer = c(TRUE, TRUE)))
    (contr_emm2B <- summary(contrast(emm_2, "pairwise", by = c("treatment")), infer = c(TRUE, TRUE)))
    # List tables
    tables <- c('mod_res', 'ame1_res', 
                'emm_1_res', 'contr_emm1A', 
                'emm_2_res', 'contr_emm2A', 'contr_emm2B')
    
  } else if (interaction=='~ Flood_1Lag') {
    
    ## A. Average Marginal Effects (slopes)
    (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
    ## B. Estimated Marginal Means (predictions)
    # MARGINAL MEANS - 1% increase
    (emm_1_res <- summary(emm_1, infer = c(TRUE, TRUE)))
    # MARGINAL MEANS - MULTIPLE LEVELS OF INUNDATON
    (emm_2_res <- summary(emm_2, infer = c(TRUE, TRUE)))
    (contr_emm2A <- summary(contrast(emm_2, "pairwise"), infer = c(TRUE, TRUE)))
    # List tables
    tables <- c('mod_res', 'ame1_res', 
                'emm_1_res', 
                'emm_2_res', 'contr_emm2A')
    
  }
  
  ## Export Tables & Plots
  folder <- paste0(outcome, '/', gsub("^-", "", gsub("[[:space:]~*]+", "-", interaction)), '/')
  for(t in tables) {
    
    write.xlsx(get(t), paste0(folder, t, '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
    print(get(t))
    
  }
  
  print(paste0("VARIABLE ", folder, "IS COMPLETE"))
  
}

run_model <- function(outcome, type) {
  # Set the main formula
  fixed <- ' ~ Flood_1Lag * season_flood * treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL'
  
  # Run model depending on dtype
  if(type=='cont'){
    
    model <- lme(
      fixed = as.formula(paste0(outcome, fixed)),
      random = list(wcode = (~1|season_id), c_code = (~1)), # Random effects
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      na.action = na.omit,  # Handle missing data using na.omit
      data = df
    )
    
  } else if (type=='bin'){
    
    model <- glmer(
      formula=as.formula(paste0(outcome, fixed, '+ (1 + wcode|season_id) + (1 | c_code)')),
      weights = wdiet_wt,
      data = df,
      family = binomial(link="logit"), # extracts binomial regression on logit scale
      control=glmerControl(optimizer="bobyqa") # Removes non-convergence warnings
    )
    
  }
  
  # Get results for interaction combos
  get_results(model, outcome, type, '~ Flood_1Lag * season_flood * treatment') # 3-way
  get_results(model, outcome, type, '~ Flood_1Lag * season_flood') # 2-way-seas
  get_results(model, outcome, type, '~ Flood_1Lag * treatment') # 2-way-treat
  get_results(model, outcome, type, '~ Flood_1Lag') # none
  
  
}


# GENERATE RESULTS FOR ALL OUTCOMES

for (c in outcomes_cont){
  
  # Run model
  start_time <- Sys.time()
  run_model(c, 'cont')
  end_time <- Sys.time()
  
  # Print timings
  total_time <- end_time - start_time
  times <- rbind(times, data.frame(Variable = c, Time = total_time))
  print(paste0('TIME TO RUN ', c, ' MODEL: ', total_time))
}

for (b in outcomes_bin) {
  
  # Run model
  start_time <- Sys.time()
  run_model(b, 'bin')
  end_time <- Sys.time()
  
  # Print timings
  total_time <- end_time - start_time
  times <- rbind(times, data.frame(Variable = b, Time = total_time))
  print(paste0('TIME TO RUN ', b, ' MODEL: ', total_time))  
}

print(times)
write.xlsx(times, 'times.xlsx', rowNames=FALSE, fileEncoding = "UTF-8")


