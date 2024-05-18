################################################################################
#### GENERATE MANUSCRIPT TABLES  #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/3. Analysis/')

#### IMPORTANT - Run R0_Data_formatting first

# PACKAGES ####

# Required packages & library
packages <- c('openxlsx', 'zoo', 'cli', 'tidyr', 'dplyr', 'reshape2')
library <- 'C:/Users/offne/Documents/R/win-library/FAARM/' # set path

#### Load packages from library
(.libPaths(library)) # Set library directory
for (p in packages){
  library(p, character.only = TRUE, lib.loc = library)
}


# FUNCTIONS ####

outcomes_cont <- "dd10r_score_m"
outcomes_bin <- c("dd10r_min_m", "dd10r_flesh", "dd10r_dairy", "dd10r_eggs",
                  "dd10r_dglv", "dd10r_vita", "dd10r_othv", "dd10r_othf",
                  "dd10r_legume", "dd10r_nuts")

## DESCRIPTIVE FUNCTIONS
# Function to extract descriptive info for continuous variable (mean, CI, p)
summary_cont <- function(df, g1, group=1, col) {
  # Extract descriptive info for continuous variable (mean, CI, p)
  # Data is grouped by treatment and year_season
  ## df: data frame 
  ## g1: main grouping for long format
  ## group: sub-group of treatment
  ## col: variable to run descriptive stats on
  
  df %>%
    group_by(get(g1), treatment) %>%
    filter(treatment==group) %>%
    summarise(mean = t.test(get(col))$estimate,
              lower_CI = t.test(get(col))$conf.int[1],
              upper_CI = t.test(get(col))$conf.int[2],
              p_value = t.test(get(col))$p.value)
  
  
}

# Function to extract descriptive info for binary variable (proportion, CI, p)
summary_bin <- function(df, g1, group=1, col) {
  # Extract descriptive info for binary variable (proportion, CI, p)
  # Data is grouped by treatment and year_season
  ## df: data frame
  ## g1: main grouping for long format
  ## group: sub-group of treatment
  ## col: variable to run descriptive stats on
  
  df %>%
    group_by(get(g1), treatment) %>%
    filter(treatment==group) %>%
    summarise(count = sum(!is.na(get(col))),
              occur = sum(get(col) == 1, na.rm = TRUE), #sum(!is.na(get(col))),
              prop = (binom.test(occur, count, conf.level = 0.95)$estimate)*100,
              lower_CI = (binom.test(occur, count, conf.level = 0.95)$conf.int[1])*100,
              upper_CI = (binom.test(occur, count, conf.level = 0.95)$conf.int[2])*100,
              p_value = binom.test(occur, count, conf.level = 0.95)$p.value)
  
}

# Function to extract descriptive info for cat variable (proportion, CI, p)
t1_summary_cat <- function(df, col) {
  # Extract descriptive info for cat variable (proportion, CI, p)
  # Assuming df is your data frame
  result <- df %>%
    group_by(treatment, get(col)) %>%
    summarise(
      count = n()
    ) %>%
    group_by(treatment) %>%
    mutate(
      total_count = sum(count),
      proportion = count / total_count * 100
    ) %>%
    select(total_count, treatment, `get(col)`, proportion)
  
  return(result)  
}

# Function to extract descriptive info for continuous variable 
t1_summary_cont <- function(df, col) {
  # Extract descriptive info for continuous variable
  # Assuming df is your data frame
  result <- df %>%
    group_by(treatment) %>%
    summarise(
      count = n(),
      mean_value = mean(get(col), na.rm = TRUE),
      sd_value = sd(get(col), na.rm = TRUE),
      min_value = min(get(col), na.rm = TRUE),
      max_value = max(get(col), na.rm = TRUE)
    )
  
  return(result)  
}

## ANALYSIS FUNCTIONS
# Function to extract the string including 3 characters before and after the '/' sign
extract_string <- function(string) {
  # Find the position of the '/' sign
  slash_position <- regexpr("/", string)
  # If there's no '/', return NA
  if (slash_position == -1) {
    return(NA)
  }
  # Extract the substring
  extracted_string <- substring(string, slash_position - 3, slash_position + 3)
  return(extracted_string)
}

# Function to format columns
format_cols <- function(outcome, var, est, l_ci, u_ci, p, num=2) {
  
  ### Format CI & p (2 decimal places)
  ci <- paste0(sprintf(paste0("%.", as.character(num), "f"), l_ci), ',', sprintf(paste0("%.", as.character(num), "f"), u_ci))
  p <- paste0('(', sprintf(paste0("%.", as.character(num), "f"), p), ')')
  if (outcome=='dd10r_score_m'){
    (master <- data.frame(variables=var, COEF=sprintf(paste0("%.", as.character(num), "f"), est), CI=ci, P=p ))
  } else {
    (master <- data.frame(variables=var, PROB=sprintf(paste0("%.", as.character(num), "f"), est), CI=ci, P=p ))
    
  }
  # Prefix with outcome
  colnames(master) <- paste0(outcome, '_', colnames(master)) 
  
  return(master)
}

# Function to extract and format predicted values
get_emm <- function (outcome, folder,val, l_ci, u_ci, p_vl) {
  
  # Get relevant data
  res <- read.xlsx(paste0('I. Results/', outcome, folder, 'emm_2_res.xlsx'))
  # res <- res[-grep(c('8.54', '18.54'), as.character(res$Flood_1Lag)), ]
  # row.names(res) <- NULL
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
  res <- read.xlsx(paste0('I. Results/', outcome, folder, file))
  
  # Remove irrelevant levels/cols
  if (grepl('treatment', folder)) {
    col <- res$Flood_1Lag
    # res <- res[!grepl("^0", as.character(col)), ]
    # row.names(res) <- NULL
  } else {
    col <- res$contrast
    res <- res[grep("^0", as.character(col)), ]
    row.names(res) <- NULL
  }

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
      
    diff <- diff %>% 
        group_by(grp = (row_number()-4) %/% 3) %>% # add NA's every 3rd row after row 4
      group_modify(~ add_row(.x, !!paste0(outcome, "_variables") := rep(NA, 1))) %>% 
      ungroup() %>% 
        select(-grp)
      diff <- rbind(NA, diff)
      diff <- diff[-nrow(diff), ] # remove last row

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
  (Int_3 <- cbind(get_emm(outcome, f, val, l_ci, u_ci, p_vl), get_contr(outcome, f, u_ci, l_ci, p_vl)))
  f <- '/Flood_1Lag-season_flood/'
  (Int_2_s <- cbind(get_emm(outcome, f, val, l_ci, u_ci, p_vl), get_contr(outcome, f, u_ci, l_ci,  p_vl)))
  f <- '/Flood_1Lag-treatment/'
  (Int_2_t <- cbind(get_emm(outcome, f, val, l_ci, u_ci, p_vl), get_contr(outcome, f, u_ci, l_ci,  p_vl)))
  f <- '/Flood_1Lag/'
  (Int_0 <- cbind(get_emm(outcome, f, val, l_ci, u_ci, p_vl), get_contr(outcome, f, u_ci, l_ci,  p_vl)))
  
  # COMBINE ALL MEANS & TESTS
  (top <- cbind(Int_0, Int_2_t))
  (bottom <- cbind(Int_2_s, Int_3))
  table <- rbind(top, bottom)
  
  # Export
  write.xlsx(table,  paste0('II. Tables/absolute_diff_', outcome, '.xlsx'), rowNames=FALSE, fileEncoding = "UTF-8")
  
  return(table)
  
}

# Function to format relative difference outputs 
rel_diff_form <- function(outcome, name, dtype, model, folder){
  
  if (dtype=='cont'){
    
    # set variable names
    l_ci <- 'lower.CL'
    u_ci <- 'upper.CL'
    
  } else if (dtype=='bin'){
    
    # set variable names
    l_ci <- 'asymp.LCL'
    u_ci <- 'asymp.UCL'
    
  }
  
  (res <- read.xlsx(paste0('I. Results/', outcome, folder, 'ame1_res.xlsx')))
  
  if (folder=='/Flood_1Lag-season_flood-treatment/') {
    
    res$treatment <- ifelse(res$treatment == 0, "Control", 'HFP')
    res$group <- paste0(res$season_flood, ':', res$treatment)
    
  } else if (folder=='/Flood_1Lag-season_flood/'){ #there is no treatment
    
    res$treatment <- 'Overall'
    res$group <- res$season_flood      
    
  } else if (folder=='/Flood_1Lag-treatment/') { # there is no season
    
    res$treatment <- ifelse(res$treatment == 0, "Control", 'HFP')
    res$season_flood <- 'Overall'
    res$group <- res$treatment      
    
    
  } else {
    
    res$treatment <- 'Overall'
    res$season_flood <- 'Overall'
    res$group <- 'Overall'    
    
  }
  
  res$model <- model
  res$outcome <- name
  res <- res[, c('outcome', 'model', 'season_flood', 'treatment', 'group', '1.trend', l_ci, u_ci, 'p.value')]
  colnames(res) <- c('Outcome', 'Model', 'Seas', 'Treat', 'Group', 'Diff', 'Lower.CI', 'Upper.CI', 'P')
  
  return(res)
  
}

# Function to get all difference outputs for figure creations
rel_diff_fig <- function(outcome, name, dtype){
  
  f1 <- rel_diff_form(outcome, name, dtype, 'F1', '/Flood_1Lag/')
  f2 <- rel_diff_form(outcome, name, dtype, 'F2', '/Flood_1Lag-treatment/')
  f3 <- rel_diff_form(outcome, name, dtype, 'F3', '/Flood_1Lag-season_flood/')
  f4 <- rel_diff_form(outcome, name, dtype, 'F4', '/Flood_1Lag-season_flood-treatment/')
  
  return (rbind(f1, f2, f3, f4))
  
}

# Function to get all absolute value outputs for figure creations
abs_val_fig <- function (outcome, name, dtype, folder, fig=0) {
  
  # outcome <- 'dd10r_score_m'
  # name <- 'WDDS'
  # dtype <- 'cont'
  # folder <- f
  # p_vl <- 'p.value' 
  
  if (dtype=='cont'){
    
    # set variable names
    l_ci <- 'lower.CL'
    u_ci <- 'upper.CL'
    val <- 'emmean'
    
  } else if (dtype=='bin'){
    
    # set variable names
    l_ci <- 'asymp.LCL'
    u_ci <- 'asymp.UCL'
    val <- 'prob'
  }
  
  # Get means
  (res <- get_emm(outcome, folder, val, l_ci, u_ci, 'p.value'))

  if (grepl('treatment', folder)){
    
    (res <- data.frame(pivot_longer(res, cols = everything(), names_to = ".value", names_pattern = paste0(outcome,"_(.*)"))))
    res$treat <- c('Control', 'HFP')
    res$increase <- c(0, 0, 1, 1, 2, 2, 3, 3)
    
  } else{
    res$increase <- c(0, 1, 2, 3)
  }
  
  colnames(res)[1:3] <- c('variables', 'value', 'CI')
  res$season <- sapply(res$variables , extract_string)
  res$group <- name

  # Get sig tests
  contr <- data.frame(get_contr(outcome, folder, l_ci, u_ci, 'p.value'))
  contr$sig <- as.numeric(gsub("[()]", "", contr[, grepl("_P$", names(contr))]))
  contr$sig <- na.locf(contr$sig, na.rm = FALSE, fromLast = TRUE) # Backfill the column
  contr$sig <- ifelse(contr$sig > 0.05, "p>0.05", 'p<0.05')
  colnames(contr)[1:3] <- c('variables', 'value', 'CI')
  
  # Store difference tests differently for each folder
  if (grepl('treatment', folder)){
    res$sig <- 'None'
    contr <- contr[,-4]
    contr$treat <- 'HFP-Control'
    contr$increase <- c(0, 1, 2, 3)
    contr$season <- sapply(contr$variables , extract_string)
    contr$group <- name
    res <- rbind(res, contr)
  } else{

    res$sig <- contr$sig
  }
  
  res$value <- as.numeric(res$value)
  # Replace names
  if (grepl('treatment', folder)){
    res <- res[, c('group', 'season', 'treat', 'increase', 'value', 'sig')]
    res <- res[order(res$treat, res$season),]
  } else {
    res <- res[, c('season', 'group', 'increase', 'value', 'sig')]
  }
  
  return(res)
}


# DESCRIPTIVE DATA CLEANING ####
# Get observations for women over BL 
dates <- c('2015-1', '2015-2', '2015-3', '2015-4')
w_BL <- df_BL %>% filter(year_season %in% dates & !is.na(dd_elig))
w_BL <- w_BL[w_BL$dd10r_othv != 88, ] # Remove 88 values from other veg
nrow(w_BL)
# Get observations for women over surveillance 
w_S <- df %>% filter(!is.na(dd_elig))
nrow(w_S)
# Combine BL & surveillance frame to extract descriptive stats
w_desc <- bind_rows(w_BL, w_S, .id = "Source")
nrow(w_desc) # number of observations
length(unique(w_desc$wcode)) # number of women


# For each BL woman, aggregate multiple BL seasons into a single observation
# NB: Starches are included for descriptive, but NOT analysis
diets_BL <- data.frame(wcode=unique(w_BL$wcode), treatment=w_BL$treatment)
for (i in c(outcomes_bin, 'dd10r_starch', outcomes_cont)){
  if (i %in% outcomes_cont) { # Calculate the mean of continuous data
    x <- df_BL %>%
      group_by(wcode) %>%   # Group the data by ID
      filter(year_season %in% dates) %>%
      summarise(mean = mean(get(i), na.rm = TRUE)) %>%
      rename_with(.fn = ~ i, .cols = mean)
  } else { # Calculate the mode of binary data
    x <- df_BL %>%
      group_by(wcode) %>%   # Group the data by ID
      filter(year_season %in% dates) %>%
      summarise(mode = as.integer(names(which.max(table(get(i)))))) %>%
      rename_with(.fn = ~ i, .cols = mode)
  }
  x <- as.data.frame(x)
  # Make sure that the wcodes match up
  # (NB: continuous variables contain BL for new women collected in year 2)
  if (count(diets_BL) != count(x)) {
    # Assuming df1 and df2 are your two data frames
    common_identifier <- intersect(x$wcode, diets_BL$wcode)
    x <- x %>% filter(wcode %in% common_identifier)
    diets_BL[[i]] <- x[[i]]
  } else {
    diets_BL[[i]] <- x[[i]]
  }
}

diets_BL$year_season <-  'Baseline'
# Create df with BL and surveillance
df_BL_S <- bind_rows(diets_BL, w_S, .id = "Source")

# DESCRIPTIVE STATISTICS: Baseline characteristics of women, by trial-arm ####

# Get descriptive on characteristics
cont <- c("age_3_BL", "wi_land_BL", "hh1hh_mem_EL") 
cat <- c("woman_edu_cat__BL", 'quint2_BL', 'g_2h_BL') 

c <- outcomes_cont
master <- t1_summary_cont(w_BL, c)
master[, sapply(master, is.numeric)] <- lapply(master[, sapply(master, is.numeric)], function(x) round(x, digit=1)) # round decimals to 3
master[[c]] <- paste0(master$mean_value, " ± ", master$sd_value)  # Drop irrelevant columns
master <- master[, c(1,7)]  # Select relevant columns

# Export descriptive stats for continuous data
for (c in cont) {
  desc <- t1_summary_cont(w_BL, c)
  desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], function(x) round(x, digit=1)) # round decimals to 3
  desc[[c]] <- paste0(desc$mean_value, " ± ", desc$sd_value)  # Drop irrelevant columns
  desc <- desc[, 7]  # Select relevant columns
  master <- cbind(master, desc)
}

# Export descriptive stats for cat data
for (c in c(cat, 'dd10r_starch', outcomes_bin)) {
  desc <- t1_summary_cat(w_BL, c)
  desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], function(x) round(x, digit=1)) # round decimals to 3
  
  if(all(unique(desc$`get(col)`) %in% c(0, 1))) {
    desc <- desc[desc$`get(col)` != 0, ] # Select relevant rows
    master[[c]] <- desc$proportion
  } else {
    desc <- desc %>% 
      spread(key = `get(col)`, value = proportion) %>% # Transpose variables with values NOT 0
      arrange_at(c('treatment'))
    colnames(desc) <- paste0(c, '__', colnames(desc)) 
    desc <- desc[, -c(1,2)]  # Drop irrelevant columns
    master <- cbind(master, desc)
  }
}
master <- data.frame(t(master)) # transpose
colnames(master) <- c('Control', 'Treatment')
# Export
write.xlsx(master, "II. Tables/desc_trial.xlsx", rowNames=TRUE, fileEncoding = "UTF-8") # export to xlsx

# DESCRIPTIVE STATISTICS: Baseline characteristics of women, by trial-arm, across survey rounds ####

# Export descriptive stats for WDDS 
c <- outcomes_cont
tre <- summary_cont(df_BL_S, g1='year_season', group=1, c) # treat
con <- summary_cont(df_BL_S, g1='year_season', group=0, c) # control
m <- rbind(tre, con) # combine treat & control by row
master <- format_cols(c, m$`get(g1)`, m$mean, m$lower_CI, m$upper_CI, m$p_value,num=1)
master <- master[, -c(4)]  # Drop irrelevant columns
master <- cbind(master[, 1], m$treatment, master[, -1])
names(master)[1:2] <- c("Round", 'Treatment')

# Export descriptive stats for flooding 
c <- 'perc_flooded_c'
tre <- summary_cont(df, g1='year_season', group=1, c) # treat
con <- summary_cont(df, g1='year_season', group=0, c) # control
m <- rbind(tre, con) # combine treat & control by row
m[, c('mean', 'lower_CI', 'upper_CI')] <- m[, c('mean', 'lower_CI', 'upper_CI')] * 100
desc <- format_cols(c, m$`get(g1)`, m$mean, m$lower_CI, m$upper_CI, m$p_value,num=2)
desc <- desc[, -c(1, 4)]  # Drop irrelevant columns
# Add baseline NA's for flood
# desc <- rbind(c(NA, NA), desc)
desc <- rbind(desc[1:24, ], c(NA, NA), desc[25:nrow(desc), ], c(NA, NA))
rownames(desc) <- NULL
# Append to master
master <- cbind(master, desc)

# Export descriptive stats for binary outcomes (+starches)
for (b in c('dd10r_min_m', "dd10r_starch", outcomes_bin[outcomes_bin != 'dd10r_min_m'])) {
  tre <- summary_bin(df_BL_S, g1='year_season', group=1, b) # treat
  con <- summary_bin(df_BL_S, g1='year_season', group=0, b) # control
  (m <- rbind(tre, con)) # combine treat & control by row
  desc <- format_cols(b, m$`get(g1)`, m$prop, m$lower_CI, m$upper_CI, m$p_value, num=0)
  desc <- desc[, -c(1, 4)]  # Drop irrelevant columns
  master <- cbind(master, desc)
}

# Convert the column to a factor with the desired order of levels
master$Round <- factor(master$Round, levels = c('Baseline', sort(unique(master$Round[master$Round != 'Baseline']))))
master <- master %>% arrange_at(c('Treatment', 'Round'))
# Export
write.xlsx(master, "II. Tables/desc_trial_rounds.xlsx", rowNames=FALSE, fileEncoding = "UTF-8") # export to xlsx


# MAIN EFFECTS: Regression results for impact of 1% flood coverage on dietary outcomes ####

# Create main frame with continuous outcome
### Get data
(path <- paste0('I. Results/', outcomes_cont, '/Flood_1Lag-season_flood-treatment/mod_res.xlsx'))
(mod_res <- read.xlsx(path))
### Format CI & p (2 decimal places)
master <- format_cols(outcomes_cont, mod_res$Variables, mod_res$Estimate, mod_res$Lower_CI, mod_res$Upper_CI, mod_res$P_Value)
names(master)[names(master) == "dd10r_score_m_variables"] <- "variables"  

### Append each binary outcome to the main frame
for (b in outcomes_bin) {
  
  # Get data
  (path <- paste0('I. Results/', b, '/Flood_1Lag-season_flood-treatment/mod_res.xlsx'))
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
write.xlsx(master,  'II. Tables/main_effects.xlsx', rowNames=FALSE, fileEncoding = "UTF-8")


# MARGINAL EFFECTS: Relative effects of 1% flood coverage on dietary outcomes ####

# GET 3 WAY INTERACTION
f <- '/Flood_1Lag-season_flood-treatment/'
(path <- paste0('I. Results/', outcomes_cont, f, 'ame1_res.xlsx'))
(res <- read.xlsx(path))
(var <- paste(round(res$Flood_1Lag, 2), res$season_flood, res$treatment, sep = "-"))
(Int_3 <- format_cols(outcomes_cont, var, res[['1.trend']], res[['lower.CL']], res[['upper.CL']], res[['p.value']]))
# For every binary outcome
for (b in outcomes_bin) {
  (path <- paste0('I. Results/', b, f, 'ame1_res.xlsx'))
  (res <- read.xlsx(path))
  (new <- format_cols(b, var, res[['1.trend']], res[['asymp.LCL']], res[['asymp.UCL']], res[['p.value']]))
  # Append columns to master df
  (Int_3 <- cbind(Int_3, new))
}

# GET 2 WAY INTERACTION (SEASON)
f <- '/Flood_1Lag-season_flood/'
(path <- paste0('I. Results/', outcomes_cont, f, 'ame1_res.xlsx'))
(res <- read.xlsx(path))
(var <- paste(round(res$Flood_1Lag, 2), res$season_flood, sep = "-"))
(Int_2_S <- format_cols(outcomes_cont, var, res[['1.trend']], res[['lower.CL']], res[['upper.CL']], res[['p.value']]))
# For every binary outcome
for (b in outcomes_bin) {
  (path <- paste0('I. Results/', b, f, 'ame1_res.xlsx'))
  (res <- read.xlsx(path))
  (new <- format_cols(b, var, res[['1.trend']], res[['asymp.LCL']], res[['asymp.UCL']], res[['p.value']]))
  # Append columns to master df
  (Int_2_S <- cbind(Int_2_S, new))
}


# GET 2 WAY INTERACTION (TREATMENT)
f <- '/Flood_1Lag-treatment/'
(path <- paste0('I. Results/', outcomes_cont, f, 'ame1_res.xlsx'))
(res <- read.xlsx(path))
(var <- paste(round(res$Flood_1Lag, 2), res$treatment, sep = "-"))
(Int_2_T <- format_cols(outcomes_cont, var, res[['1.trend']], res[['lower.CL']], res[['upper.CL']], res[['p.value']]))
# For every binary outcome
for (b in outcomes_bin) {
  (path <- paste0('I. Results/', b, f, 'ame1_res.xlsx'))
  (res <- read.xlsx(path))
  (new <- format_cols(b, var, res[['1.trend']], res[['asymp.LCL']], res[['asymp.UCL']], res[['p.value']]))
  # Append columns to master df
  (Int_2_T <- cbind(Int_2_T, new))
}

# GET OVERALL (NO INTERACTION)
f <- '/Flood_1Lag/'
(path <- paste0('I. Results/', outcomes_cont, f, 'ame1_res.xlsx'))
(res <- read.xlsx(path))
(var <- round(res$Flood_1Lag, 2))
(Int_0 <- format_cols(outcomes_cont, var, res[['1.trend']], res[['lower.CL']], res[['upper.CL']], res[['p.value']]))
# For every binary outcome
for (b in outcomes_bin) {
  (path <- paste0('I. Results/', b, f, 'ame1_res.xlsx'))
  (res <- read.xlsx(path))
  (new <- format_cols(b, var, res[['1.trend']], res[['asymp.LCL']], res[['asymp.UCL']], res[['p.value']]))
  # Append columns to master df
  (Int_0 <- cbind(Int_0, new))
}

# Combine tables
master <- rbind(Int_0, Int_2_T, Int_2_S, Int_3)
write.xlsx(master,  'II. Tables/marginal_effects.xlsx', rowNames=FALSE, fileEncoding = "UTF-8")

# PREDICTED VALUES: Absolute measures of 1% flood coverage on dietary outcomes, with difference tests ####

# Table 1 (Presentation)

### Get continuous outcomes
abs_diff_table(outcomes_cont, dtype='cont')

### Get binary outcomes
for (b in outcomes_bin) {
  abs_diff_table(b, dtype='bin')
}


# Table Processing for figures ####

# Create R_Rel-Diff
table1 <- rbind(rel_diff_fig('dd10r_score_m', 'WDDS', 'cont'),
               rel_diff_fig('dd10r_min_m', 'MDD', 'bin'),
               rel_diff_fig('dd10r_flesh', 'Flesh foods', 'bin'),
               rel_diff_fig('dd10r_dairy', 'Dairy', 'bin'),
               rel_diff_fig('dd10r_eggs', 'Eggs', 'bin'),
               rel_diff_fig('dd10r_dglv', 'Dark green leafy vegetables', 'bin'),
               rel_diff_fig('dd10r_vita', 'Vitamin-A rich foods', 'bin'),
               rel_diff_fig('dd10r_othv', 'Other vegetables', 'bin'),
               rel_diff_fig('dd10r_othf', 'Other fruits', 'bin'),
               rel_diff_fig('dd10r_legume', 'Legumes', 'bin'),
               rel_diff_fig('dd10r_nuts', 'Nuts/seeds', 'bin'))

# Create R_Abs-Flood-Levels
f <- '/Flood_1Lag-season_flood/'
table2 <- rbind(abs_val_fig('dd10r_score_m', "Dietary diversity scores*", 'cont', f), 
               abs_val_fig('dd10r_min_m', "Minimum dietary diversity", 'bin', f),
               abs_val_fig('dd10r_flesh', 'Flesh foods', 'bin', f),
               abs_val_fig('dd10r_dairy', 'Dairy', 'bin', f),
               abs_val_fig('dd10r_eggs', 'Eggs', 'bin', f),
               abs_val_fig('dd10r_dglv', 'Dark green leafy vegetables', 'bin', f),
               abs_val_fig('dd10r_vita', 'Vitamin-A rich foods', 'bin', f),
               abs_val_fig('dd10r_othv', 'Other vegetables', 'bin', f),
               abs_val_fig('dd10r_othf', 'Other fruits', 'bin', f),
               abs_val_fig('dd10r_legume', 'Legumes', 'bin', f),
               abs_val_fig('dd10r_nuts', 'Nuts/seeds', 'bin', f))


# Create R-Abs-Flood-Treat_Levels
f <- '/Flood_1Lag-season_flood-treatment/'
table3 <- rbind(abs_val_fig('dd10r_score_m', 'WDDS', 'cont', f), 
               abs_val_fig('dd10r_min_m', 'MDD', 'bin', f),
               abs_val_fig('dd10r_flesh', 'Flesh foods', 'bin', f),
               abs_val_fig('dd10r_dairy', 'Dairy', 'bin', f),
               abs_val_fig('dd10r_eggs', 'Eggs', 'bin', f),
               abs_val_fig('dd10r_dglv', 'Dark green leafy vegetables', 'bin', f),
               abs_val_fig('dd10r_vita', 'Vitamin-A rich foods', 'bin', f),
               abs_val_fig('dd10r_othv', 'Other vegetables', 'bin', f),
               abs_val_fig('dd10r_othf', 'Other fruits', 'bin', f),
               abs_val_fig('dd10r_legume', 'Legumes', 'bin', f),
               abs_val_fig('dd10r_nuts', 'Nuts/seeds', 'bin', f))


# Save the dataframes to an Excel file with different sheet names
write.xlsx(list(R_Rel_Diff=table1, R_Abs_Flood_Levels=table2, R_Abs_Flood_Treat_Levels=table3), "III. Figures/Visuals.xlsx")
