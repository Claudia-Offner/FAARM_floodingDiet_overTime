################################################################################
#### GENERATE MANUSCRIPT TABLES  #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/3. Analysis/')


outcomes_cont <- "dd10r_score_m"
outcomes_bin <- c("dd10r_min_m", "dd10r_flesh", "dd10r_dairy", "dd10r_eggs",
                  "dd10r_dglv", "dd10r_vita", "dd10r_othv", "dd10r_othf",
                  "dd10r_legume", "dd10r_nuts")

# Function to format columns
format_cols <- function(outcome, var, est, l_ci, u_ci, p) {
  
  ### Format CI & p (2 decimal places)
  ci <- paste0(sprintf("%.2f", l_ci), ',', sprintf("%.2f", u_ci))
  p <- paste0('(', sprintf("%.2f", p), ')')
  if (outcome=='dd10r_score_m'){
    (master <- data.frame(variables=var, COEF=sprintf("%.2f", est), CI=ci, P=p ))
  } else {
    (master <- data.frame(variables=var, PROB=sprintf("%.2f", est), CI=ci, P=p ))
    
  }
  # Prefix with outcome
  colnames(master) <- paste0(outcome, '_', colnames(master)) 
  
  return(master)
}

# Function to extract and format predicted values
get_emm <- function (outcome, folder,val, l_ci, u_ci, p_vl) {
  
  # Get relevant data
  res <- read.xlsx(paste0('- Results/', outcome, folder, 'emm_2_res.xlsx'))
  res <- res[-grep(c('8.54', '18.54'), as.character(res$Flood_1Lag)), ]
  row.names(res) <- NULL
  # Set variable, depending on folder
  if (folder=='/Flood_1Lag-season_flood-treatment/'){
    (var <- paste(round(res$Flood_1Lag, 2), res$season_flood, res$treatment, sep = "-"))
  } else if (folder=='/Flood_1Lag-season_flood/'){
    (var <- paste(round(res$Flood_1Lag, 2), res$season_flood, sep = "-"))
  } else if (folder=='/Flood_1Lag-treatment/'){
    (var <- paste(round(res$Flood_1Lag, 2), res$treatment, sep = "-"))
  } else if (folder=='/Flood_1Lag/'){
    (var <- round(res$Flood_1Lag, 2))
  }
  # Format data
  (mean <- format_cols(outcome, var, res[[val]], res[[l_ci]], res[[u_ci]], round(res[[p_vl]], 2)))
  (mean <- mean[, -which(names(mean) == paste0(outcome, '_P'))]) # remove p-values
  
  # Split data, depending on treatment
  if (grepl("treatment", folder)){
    half_rows <- nrow(mean) / 2
    df_top <- mean[1:half_rows, ]
    df_bottom <- mean[(half_rows + 1):nrow(mean), ]
    mean <- cbind(df_top, df_bottom)
    
    return(mean)
    
  } else {
    
    return(mean)
    
  }
  
}

# Function to extract and format difference tests
get_contr <- function (outcome, folder,l_ci, u_ci, p_vl){
  
  # Get data
  if (folder=='/Flood_1Lag-season_flood/'){
    file <- 'contr_emm2b.xlsx'
  } else {
    file <- 'contr_emm2a.xlsx'
  }
  res <- read.xlsx(paste0('- Results/', outcome, folder, file))
  
  # Remove irrelevant levels/cols
  if (grepl('treatment', folder)) {
    col <- res$Flood_1Lag
  } else {
    col <- res$contrast
  }
  res <- res[-grep(c('8.54', '18.54'), as.character(col)), ]
  row.names(res) <- NULL
  
  # Set variable, depending on folder
  if (folder=='/Flood_1Lag-season_flood-treatment/'){
    (var <- paste(round(res$Flood_1Lag, 2), res$season_flood, res$contrast, sep = "-"))
  } else if (folder=='/Flood_1Lag-season_flood/'){
    (var <- paste(res$season_flood, res$contrast, sep = "-"))
  } else if (folder=='/Flood_1Lag-treatment/'){
    (var <- paste(round(res$Flood_1Lag, 2), res$contrast, sep = "-"))
  } else if (folder=='/Flood_1Lag/'){
    (var <- res$contrast)
  }
  
  # Format
  res[, c("estimate", l_ci, u_ci)] <- res[, c("estimate", l_ci, u_ci)] * -1
  (diff <- format_cols(outcome, var, res[['estimate']], res[[l_ci]], res[[u_ci]], round(res[[p_vl]], 2)))

  # Line up columns
  if (!grepl("treatment", folder)){
    
      # Make every third row empty from the second row
      (diff <- rbind(NA, diff))
      (diff[seq(1, nrow(diff), by = 3), ] <- NA)
      (diff <- diff[-nrow(diff), ]) # remove last row
    
      return(diff)
    
    } else {
      
      return(diff)
      
    }
}

# Function to get absolute difference tables, formatted according to excel structure
# NB: Tables will be automatically saved to location
abs_diff_table <- function(outcome, dtype){
  
  if (dtype=='cont'){
    
    # set variable names
    l_ci <- 'lower.CL'
    u_ci <- 'upper.CL'
    p_vl <- 'p.value'
    val <- 'emmean'
    
  } else if (dtype=='bin'){
    
    # set variable names
    l_ci <- 'asymp.LCL'
    u_ci <- 'asymp.UCL'
    p_vl <- 'p.value'
    val <- 'prob'
    
  }
  
  f <- '/Flood_1Lag-season_flood-treatment/'
  (Int_3 <- cbind(get_emm(outcome, f, val, l_ci, u_ci, p_vl), get_contr(outcome, f, l_ci, u_ci, p_vl)))
  f <- '/Flood_1Lag-season_flood/'
  (Int_2_s <- cbind(get_emm(outcome, f, val, l_ci, u_ci, p_vl), get_contr(outcome, f, l_ci, u_ci, p_vl)))
  f <- '/Flood_1Lag-treatment/'
  (Int_2_t <- cbind(get_emm(outcome, f, val, l_ci, u_ci, p_vl), get_contr(outcome, f, l_ci, u_ci, p_vl)))
  f <- '/Flood_1Lag/'
  (Int_0 <- cbind(get_emm(outcome, f, val, l_ci, u_ci, p_vl), get_contr(outcome, f, l_ci, u_ci, p_vl)))
  
  # COMBINE ALL MEANS & TESTS
  (top <- cbind(Int_0, Int_2_t))
  (bottom <- cbind(Int_2_s, Int_3))
  table <- rbind(top, bottom)
  
  # Export
  write.xlsx(table,  paste0('Tables/absolute_diff_', outcome, '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
  
  return(table)
  
}


# DESCRIPTIVE STATISTICS: Baseline characteristics of women, by trial-arm ####

# DESCRIPTIVE STATISTICS: Baseline characteristics of women across survey rounds, by trial-arm ####

# MAIN EFFECTS: Regression results for impact of 1% flood coverage on dietary outcomes ####

# Create main frame with continuous outcome
### Get data
(path <- paste0('- Results/', outcomes_cont, '/Flood_1Lag-season_flood-treatment/mod_res.xlsx'))
(mod_res <- read.xlsx(path))
### Format CI & p (2 decimal places)
master <- format_cols(outcomes_cont, mod_res$Variables, mod_res$Estimate, mod_res$Lower_CI, mod_res$Upper_CI, mod_res$P_Value)
names(master)[names(master) == "dd10r_score_m_variables"] <- "variables"  

### Append each binary outcome to the main frame
for (b in outcomes_bin) {
  
  # Get data
  (path <- paste0('- Results/', b, '/Flood_1Lag-season_flood-treatment/mod_res.xlsx'))
  mod_res <- read.xlsx(path)
  # Format CI & p (2 decimal places)
  new <- format_cols(b, mod_res$Variables, mod_res$Estimate, mod_res$Lower_CI, mod_res$Upper_CI, mod_res$P_Value)
  # Append columns to master df
  master <- cbind(master, new)
  
}
print(master)

# Re-arrange the row orders
rows_to_move <- c(1, 3:7, 2, 13:17, 8, 19:23, 18, 24:28,9:12)
(master <- rbind(master[rows_to_move, ], master[-rows_to_move, ]))
rownames(master) <- NULL

# Export
write.xlsx(master,  'Tables/main_effects.xlsx', rowNames=FALSE, fileEncoding = "UTF-8")


# MARGINAL EFFECTS: Relative effects of 1% flood coverage on dietary outcomes ####

# GET 3 WAY INTERACTION
f <- '/Flood_1Lag-season_flood-treatment/'
(path <- paste0('- Results/', outcomes_cont, f, 'ame1_res.xlsx'))
(res <- read.xlsx(path))
(var <- paste(round(res$Flood_1Lag, 2), res$season_flood, res$treatment, sep = "-"))
(Int_3 <- format_cols(outcomes_cont, var, res[['1.trend']], res[['lower.CL']], res[['upper.CL']], res[['p.value']]))
# For every binary outcome
for (b in outcomes_bin) {
  (path <- paste0('- Results/', b, f, 'ame1_res.xlsx'))
  (res <- read.xlsx(path))
  (new <- format_cols(b, var, res[['1.trend']], res[['asymp.LCL']], res[['asymp.UCL']], res[['p.value']]))
  # Append columns to master df
  (Int_3 <- cbind(Int_3, new))
}

# GET 2 WAY INTERACTION (SEASON)
f <- '/Flood_1Lag-season_flood/'
(path <- paste0('- Results/', outcomes_cont, f, 'ame1_res.xlsx'))
(res <- read.xlsx(path))
(var <- paste(round(res$Flood_1Lag, 2), res$season_flood, sep = "-"))
(Int_2_S <- format_cols(outcomes_cont, var, res[['1.trend']], res[['lower.CL']], res[['upper.CL']], res[['p.value']]))
# For every binary outcome
for (b in outcomes_bin) {
  (path <- paste0('- Results/', b, f, 'ame1_res.xlsx'))
  (res <- read.xlsx(path))
  (new <- format_cols(b, var, res[['1.trend']], res[['asymp.LCL']], res[['asymp.UCL']], res[['p.value']]))
  # Append columns to master df
  (Int_2_S <- cbind(Int_2_S, new))
}


# GET 2 WAY INTERACTION (TREATMENT)
f <- '/Flood_1Lag-treatment/'
(path <- paste0('- Results/', outcomes_cont, f, 'ame1_res.xlsx'))
(res <- read.xlsx(path))
(var <- paste(round(res$Flood_1Lag, 2), res$treatment, sep = "-"))
(Int_2_T <- format_cols(outcomes_cont, var, res[['1.trend']], res[['lower.CL']], res[['upper.CL']], res[['p.value']]))
# For every binary outcome
for (b in outcomes_bin) {
  (path <- paste0('- Results/', b, f, 'ame1_res.xlsx'))
  (res <- read.xlsx(path))
  (new <- format_cols(b, var, res[['1.trend']], res[['asymp.LCL']], res[['asymp.UCL']], res[['p.value']]))
  # Append columns to master df
  (Int_2_T <- cbind(Int_2_T, new))
}

# GET OVERALL (NO INTERACTION)
f <- '/Flood_1Lag/'
(path <- paste0('- Results/', outcomes_cont, f, 'ame1_res.xlsx'))
(res <- read.xlsx(path))
(var <- round(res$Flood_1Lag, 2))
(Int_0 <- format_cols(outcomes_cont, var, res[['1.trend']], res[['lower.CL']], res[['upper.CL']], res[['p.value']]))
# For every binary outcome
for (b in outcomes_bin) {
  (path <- paste0('- Results/', b, f, 'ame1_res.xlsx'))
  (res <- read.xlsx(path))
  (new <- format_cols(b, var, res[['1.trend']], res[['asymp.LCL']], res[['asymp.UCL']], res[['p.value']]))
  # Append columns to master df
  (Int_0 <- cbind(Int_0, new))
}

# Combine tables
master <- rbind(Int_0, Int_2_T, Int_2_S, Int_3)
write.xlsx(master,  'Tables/marginal_effects.xlsx', rowNames=FALSE, fileEncoding = "UTF-8")

# PREDICTED MEASURES: Absolute measures of 1% flood coverage on dietary outcomes, with difference tests ####

# Table 1 (Presentation)

### Get continuous outcomes
abs_diff_table(outcomes_cont, dtype='cont')

### Get binary outcomes
for (b in outcomes_bin) {
  abs_diff_table(b, dtype='bin')
}


# Table 2 (Processing for Figures) ####


