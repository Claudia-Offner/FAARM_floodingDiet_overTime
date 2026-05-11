### ------------------------------------------------------------------------ ### 
### Generate main results
### ------------------------------------------------------------------------ ### 

#### DEPENDENCIES ####

# Function to export estimates, marginal effects and marginal means
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
    (mod_res <- getGLMM(model, 0, 'OR'))
    # Extract as probabilities (i.e. trans = "response")
    ame1 <- emtrends(model, as.formula(paste0('pairwise ', interaction)), at=list(Flood_1Lag = levels[2]), var = 1, trans='response')$emtrends
    emm_1 <- emmeans(model, as.formula(interaction), at=list(Flood_1Lag = levels[2]), trans = "response") # 1% increase
    emm_2 <- emmeans(model, as.formula(interaction), at = list(Flood_1Lag = levels), trans = "response") # levels 
    print(plotResults(mod_res, 1))
  }
  
  # Extract tables based on interaction combo
  if(interaction=='~ Flood_1Lag * season_flood * treatment'){
    
    # Anova test
    (anov <- car::Anova(model, type=3))

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
    tables <- c('anov', 'mod_res', 'ame1_res', 'ame1_cont', 
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
  
  # Check if folder exists
  check_folder_loc(folder)
  
  for(t in tables) {
    
    write.xlsx(get(t), paste0(folder, t, '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
    print(get(t))
    
  }
  
  print(paste0("VARIABLE ", folder, "IS COMPLETE"))
  
}

# Function to run binary/continuous models, depending on outcome
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

#### MAIN CODE (<30 hour) ####

# 
# library(car)
# 
# # After running your emmeans/contrasts to know the coefficient names,
# # check exact names first:
# names(fixef(model))
# 
# linearHypothesis(model, 
#                  hypothesis.matrix = c(
#                    "Flood_1Lag:season_floodNov/Dec:treatment",
#                    "Flood_1Lag:season_floodNov/Dec"
#                  ))

# Load data
load(paste0('main_data.RData'))

# Relevant exposure
df$Flood_1Lag <- df$Flood_SThresh
levels <- flood_cat_levels

# Check and set result location
folder <- paste0(git_path, '/Main Results/')
setwd(folder)
getwd()

# Set model timer
times <- data.frame(Variable = character(0), Time = numeric(0))

# 1. Extract and export results for continuous outcomes ####

for (c in outcomes_cont){
  
  # Run model
  start_time <- Sys.time()
  run_model(outcome=c, type='cont')
  end_time <- Sys.time()
  
  # Print timings
  total_time <- end_time - start_time
  times <- rbind(times, data.frame(Variable = c, Time = total_time))
  print(paste0('TIME TO RUN ', c, ' MODEL: ', total_time))
}

# 2. Extract and export results for binary outcomes ####

for (b in outcomes_bin) {

  # Run model
  start_time <- Sys.time()
  run_model(b, 'bin')
  end_time <- Sys.time()
  
  # Print timings
  total_time <- end_time - start_time
  times <- rbind(times, data.frame(Variable = c, Time = total_time))
  print(paste0('TIME TO RUN ', b, ' MODEL: ', total_time))

}



