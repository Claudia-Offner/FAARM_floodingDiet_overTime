### ------------------------------------------------------------------------ ### 
### Generate tables
### ------------------------------------------------------------------------ ### 

# # Clear environment
# rm(list = ls())
# 
# ### IMPORTANT - set file paths to folder locations
# git_path  <- 'C:/Users/claer14/Documents/GitHub/FAARM_floodingDiet_overTime/3_R'
# setwd(git_path)
# 
# #### DEPENDENCIES ####
# source('R0_Dependencies.R')

# DESC Function to extract descriptive info for continuous variable (mean, CI, p)
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

# DESC Function to extract descriptive info for binary variable (proportion, CI, p)
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

# DESC Function to extract descriptive info for cat variable (proportion, CI, p)
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

# DESC Function to extract descriptive info for continuous variable 
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

# MAIN Function to extract the string including 3 characters before and after the '/' sign
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

# MAIN Function to format columns
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

# MAIN Function to extract and format predicted values
get_emm <- function (outcome, folder,val, l_ci, u_ci, p_vl) {
  
  # Get relevant data
  res <- read.xlsx(paste0('Main Results/', outcome, folder, 'emm_2_res.xlsx'))
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

# MAIN Function to extract and format difference tests
get_contr <- function (outcome, folder,l_ci, u_ci, p_vl){
  
  # Get data
  if (folder=='/Flood_1Lag-season_flood/'){
    file <- 'contr_emm2b.xlsx'
  } else {
    file <- 'contr_emm2a.xlsx'
  }
  res <- read.xlsx(paste0('Main Results/', outcome, folder, file))
  
  # Remove irrelevant levels/cols
  if (grepl('treatment', folder)) {
    col <- res$Flood_1Lag
  } else {
    col <- res$contrast
    col <- sub("Flood_1Lag", "", col)
    col <- sub("Flood_1Lag", "", col)
    res <- res[grep("^0", as.character(col)), ]
    res$contrast <- sub("Flood_1Lag", "", res$contrast)
    res$contrast <- sub("Flood_1Lag", "", res$contrast)
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

# MAIN Function to get absolute difference tables, formatted according to excel structure
# NB: Tables will be automatically saved to location
abs_diff_table <- function(outcome, dtype) {
  
  # Variable names by dtype
  vars <- if (dtype == 'cont') list(val='emmean',  l_ci='lower.CL',  u_ci='upper.CL',  p_vl='p.value')
  else                  list(val='prob',    l_ci='asymp.LCL', u_ci='asymp.UCL', p_vl='p.value')
  
  # Get emmeans + contrasts for a given interaction path
  get_int <- function(f) cbind(
    get_emm(outcome, f, vars$val, vars$l_ci, vars$u_ci, vars$p_vl),
    get_contr(outcome, f, vars$u_ci, vars$l_ci, vars$p_vl)
  )
  
  # Build and label table
  table <- rbind(
    cbind(get_int('/Flood_1Lag/'), get_int('/Flood_1Lag-treatment/')),
    cbind(get_int('/Flood_1Lag-season_flood/'), get_int('/Flood_1Lag-season_flood-treatment/'))
  )
  colnames(table) <- paste0(
    c(rep("Overall",3), rep("Flood Level",4), rep("Control",3), rep("Intervention",3), rep("HFP",4)),
    "_", sub("^.*_", "", colnames(table))
  )
  colnames(table)[1] <- 'Levels'
  table <- table %>% select(-ends_with("_variables"))
  
  # Replace flood level labels and mark all rows for indenting
  table$Levels <- ifelse(
    table$Levels %in% names(flood_nm), flood_nm[table$Levels],
    paste0(flood_nm[sub("-.*", "", table$Levels)], "-", sub(".*-", "", table$Levels))
  )
  table$indent <- TRUE
  
  # Season header rows
  seasons <- unique(sub(".*-", "", table$Levels[grepl("-", table$Levels)]))
  season_blocks <- do.call(rbind, lapply(seasons, function(s) {
    header <- replace(table[1,], TRUE, NA); header$Levels <- s; header$indent <- FALSE
    rows <- table[grepl(paste0("-", s, "$"), table$Levels), ]
    rows$Levels <- sub("-.*$", "", rows$Levels)
    rbind(header, rows)
  }))
  
  # Label rows
  make_label <- function(txt) { r <- replace(table[1,], TRUE, NA); r$Levels <- txt; r$indent <- FALSE; r }
  
  # Combine
  result <- rbind(make_label("Flood impacts overall"), table[1:4,], 
                  make_label("Flood impacts by season"), season_blocks)
  rownames(result) <- NULL
  
  # Flextable
  col_names   <- names(result)[-c(1, ncol(result))]
  groups      <- unique(sub("_.*$", "", col_names))
  header_wids <- c(1, sapply(groups, function(g) sum(startsWith(col_names, g))))
  display_nms <- gsub("\\.[0-9]+$", "", c("Levels", make.unique(sub("^.*_", "", col_names))))
  
  flextable(result %>% select(-indent)) %>%
    set_header_labels(values = setNames(display_nms, col_names)) %>%
    add_header_row(values = c("", groups), colwidths = header_wids) %>%
    align(part = "header", align = "center") %>%
    padding(i = which(result$indent), j = 1, padding.left = 20) %>%
    vline(j = cumsum(header_wids)[-length(header_wids)], part = "all") %>%
    theme_booktabs() %>%
    bold(part = "header") %>%
    bold(i = c(1, 6), part = "body") %>%
    italic(i = c(1, 6), part = "body") %>%
    autofit()
  
}

# Function to get all difference outputs for figure creations
rel_diff_fig <- function(outcome, dtype){
  
  # Function to format relative difference outputs 
  rel_diff_form <- function(outcome, dtype, model, folder){
    
    if (dtype=='cont'){
      
      # set variable names
      l_ci <- 'lower.CL'
      u_ci <- 'upper.CL'
      
    } else if (dtype=='bin'){
      
      # set variable names
      l_ci <- 'asymp.LCL'
      u_ci <- 'asymp.UCL'
      
    }
    
    (res <- read.xlsx(paste0('Main Results/', outcome, folder, 'ame1_res.xlsx')))
    
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
    res$outcome <- outcome
    res <- res[, c('outcome', 'model', 'season_flood', 'treatment', 'group', '1.trend', l_ci, u_ci, 'p.value')]
    colnames(res) <- c('Outcome', 'Model', 'Seas', 'Treat', 'Group', 'Diff', 'Lower.CI', 'Upper.CI', 'P')
    
    return(res)
    
  }
  
  f1 <- rel_diff_form(outcome, dtype, 'F1', '/Flood_1Lag/')
  f2 <- rel_diff_form(outcome, dtype, 'F2', '/Flood_1Lag-treatment/')
  f3 <- rel_diff_form(outcome, dtype, 'F3', '/Flood_1Lag-season_flood/')
  f4 <- rel_diff_form(outcome, dtype, 'F4', '/Flood_1Lag-season_flood-treatment/')
  
  return (rbind(f1, f2, f3, f4))
  
}

# Function to get all absolute value outputs for figure creations
abs_val_fig <- function (outcome, dtype, folder, fig=0) {
  
  # outcome <- 'dd10r_min_m'
  # name <- 'WDDS'
  # dtype <- 'bin'
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
  res$group <- outcome
  
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
    contr$group <- outcome
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

#### MAIN CODE ####

# Load data
load(paste0('main_data.RData'))

# Check and set result location
folder <- paste0(git_path, '/Tables/')
check_folder_loc(folder)
folder <- paste0(git_path, '/Figures/')
check_folder_loc(folder)


# Flood level replacement names
flood_nm <- c('0' = 'No change in flooding',
              '1' = '1 SD above average',
              '2' = '2 SD above average',
              '3' = '>2 SD above average')


# Data cleaning ####

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

# MT1: Baseline characteristics of women, by trial-arm (desc_trial) ####

# Summarise continuous outcomes
summarise_cont <- function(df, cols) {
  do.call(cbind, lapply(cols, function(c) {
    desc <- t1_summary_cont(df, c)
    desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], round, digits=1)
    setNames(data.frame(paste0(desc$mean_value, " ± ", desc$sd_value)), c)
  }))
}

# Summarise categorical outcomes
summarise_cat <- function(df, cols) {
  do.call(cbind, lapply(cols, function(c) {
    desc <- t1_summary_cat(df, c)
    desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], round, digits=1)
    if (all(unique(desc$`get(col)`) %in% c(0, 1))) {
      setNames(data.frame(desc$proportion[desc$`get(col)` != 0]), c)
    } else {
      out <- spread(desc, key=`get(col)`, value=proportion)
      out <- out[, -c(1,2)]
      colnames(out) <- paste0(c, '__', colnames(out))
      out
    }
  }))
}

# Get descriptive on characteristics
cont <- c("age_3_BL", "wi_land_BL", "hh1hh_mem_EL", 'dd10r_score_m') 
cat <- c("woman_edu_cat__BL", 'quint2_BL', 'g_2h_BL') 

# Build master table
master <- cbind(
  summarise_cont(w_BL, c(cont)),
  summarise_cat(w_BL, c(cat, 'dd10r_starch', outcomes_bin))
)
master <- data.frame(t(master))
colnames(master) <- c('Control', 'Treatment')

# Reorder and clean
desired_order <- c('age_3_BL',
                   paste0('woman_edu_cat__BL__', 0:4),
                   paste0('quint2_BL__', 1:5),
                   paste0('g_2h_BL__', 1:2),
                   'hh1hh_mem_EL', 'wi_land_BL',
                   'dd10r_starch', "dd10r_flesh", "dd10r_dairy", "dd10r_eggs",
                   "dd10r_dglv",  "dd10r_vita",  "dd10r_othv",  "dd10r_othf",
                   "dd10r_legume","dd10r_nuts",  'dd10r_min_m', 'dd10r_score_m')
master <- master[match(desired_order, rownames(master)), ]
MT1_desc_trial <- replace_matches(master, nm)

# Restructure rows with group headers
make_label  <- function(txt, df) { r <- df[1,]; r[1,] <- NA; rownames(r) <- txt; r }
split_group <- function(df, prefix) {
  rows <- df[grepl(paste0("^", prefix, ":"), rownames(df)), ]
  rownames(rows) <- sub(paste0(prefix, ": "), "", rownames(rows))
  rbind(make_label(prefix, df), rows)
}

MT1_desc_trial <- rbind(
  make_label("Individual Characteristics", MT1_desc_trial),
  MT1_desc_trial["Age", , drop=FALSE],
  split_group(MT1_desc_trial, "Education"),
  split_group(MT1_desc_trial, "Wealth"),
  split_group(MT1_desc_trial, "Religion"),
  MT1_desc_trial[c("Household members", "Land owned"), , drop=FALSE],
  make_label("Women's Dietary Characteristics", MT1_desc_trial),
  make_label("Food Groups", MT1_desc_trial),
  MT1_desc_trial[c("Starchy staples", "Flesh foods", "Dairy", "Eggs",
                   "DGLV", "Vit. A-rich foods", "Other vegetables",
                   "Other fruits", "Legumes", "Nuts/seeds", "MDD", "DDS"), , drop=FALSE]
)

# Flextable formatting
label_rows  <- which(rownames(MT1_desc_trial) %in% c("Individual Characteristics", "Women's Dietary Characteristics"))
cat_headers <- which(rownames(MT1_desc_trial) %in% c("Age", "Education", "Wealth", "Religion", "Household members", "Land owned", "Food Groups", "MDD", "DDS"))
cat_rows    <- which(grepl("^(None|Partial primary|Complete primary|Partial secondary|Complete secondary|Poorest|Lower|Middle|Upper|Wealthiest|Muslim|Hindu|Starchy staples|Flesh foods|Dairy|Eggs|DGLV|Vit\\. A-rich foods|Other vegetables|Other fruits|Legumes|Nuts/seeds)", rownames(MT1_desc_trial)))

ft <- flextable(tibble::rownames_to_column(MT1_desc_trial, " ")) %>%
  bold(i = label_rows,  j = 1, part = "body") %>%
  italic(i = cat_headers, j = 1, part = "body") %>%
  padding(i = cat_rows,   j = 1, padding.left = 20) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  autofit()

# Export
wb <- openxlsx2::wb_workbook() %>% openxlsx2::wb_add_worksheet('MT1_desc_trial')
wb <- wb_add_flextable(wb, sheet='MT1_desc_trial', ft, start_col=1, start_row=1)
openxlsx2::wb_save(wb, 'Tables/MT1_desc_trial.xlsx')

# Optional: Baseline characteristics of women, by trial-arm, across survey rounds (desc_trial_rounds) ####

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
c <- 'Flood_1Lag'
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
ST_desc_trial_rounds <- master

# Save raw unformatted file
write.xlsx(ST_desc_trial_rounds, paste0("Tables/Visuals_ST_rounds.xlsx"), rowNames=TRUE, fileEncoding = "UTF-8")

# Get the base names (everything before _COEF, _PROB, _CI)
coef_cols <- grep("_COEF$", names(ST_desc_trial_rounds), value = TRUE)
prob_cols  <- grep("_PROB$", names(ST_desc_trial_rounds), value = TRUE)

# Merge COEF+CI pairs
for (col in coef_cols) {
  base <- sub("_COEF$", "", col)
  ci_col <- paste0(base, "_CI")
  ST_desc_trial_rounds[[base]] <- paste0(ST_desc_trial_rounds[[col]], " (", ST_desc_trial_rounds[[ci_col]], ")")
  ST_desc_trial_rounds[[col]] <- NULL
  ST_desc_trial_rounds[[ci_col]] <- NULL
}

# Merge PROB+CI pairs
for (col in prob_cols) {
  base <- sub("_PROB$", "", col)
  ci_col <- paste0(base, "_CI")
  ST_desc_trial_rounds[[base]] <- paste0(ST_desc_trial_rounds[[col]], " (", ST_desc_trial_rounds[[ci_col]], ")")
  ST_desc_trial_rounds[[col]] <- NULL
  ST_desc_trial_rounds[[ci_col]] <- NULL
}

ST_desc_trial_rounds <- replace_matches(ST_desc_trial_rounds, nm)
ST_desc_trial_rounds <- ST_desc_trial_rounds[, c(1, 2, 6:ncol(ST_desc_trial_rounds), 3, 5, 4)]


# Split by treatment
control   <- ST_desc_trial_rounds %>% filter(Treatment == 0) %>% select(-Treatment)
treatment <- ST_desc_trial_rounds %>% filter(Treatment == 1) %>% select(-Treatment)

# Rename cols
colnames(control)[-1]   <- paste0(colnames(control)[-1],   "_Control")
colnames(treatment)[-1] <- paste0(colnames(treatment)[-1], "_Intervention")

# Merge side by side
result <- left_join(control, treatment, by = "Round")
# Interleave columns: DDS_Control, DDS_Intervention, Flooding_Control, Flooding_Intervention...
col_names   <- names(result)[-1]
groups      <- unique(sub("_Control$|_Intervention$", "", col_names))
interleaved <- as.vector(rbind(
  paste0(groups, "_Control"),
  paste0(groups, "_Intervention")
))
result <- result[, c("Round", interleaved)]

# Flextable with grouped headers (now grouped by variable, not treatment)
col_names   <- names(result)[-1]
groups      <- unique(sub("_Control$|_Intervention$", "", col_names))
header_wids <- c(1, sapply(groups, function(g) sum(grepl(paste0("^", g, "_(Control|Intervention)$"), col_names))))
display_nms <- gsub("\\.[0-9]+$", "", c("Round", make.unique(sub("^.*_", "", col_names))))

ft <- flextable(result) %>%
  set_header_labels(values = setNames(display_nms, names(result))) %>%
  add_header_row(values = c("", groups), colwidths = header_wids) %>%
  align(part = "header", align = "center") %>%
  vline(j = cumsum(header_wids)[-length(header_wids)], part = "all") %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  autofit()

# Export
wb <- openxlsx2::wb_workbook() %>% openxlsx2::wb_add_worksheet('ST_desc_trial_rounds')
wb <- wb_add_flextable(wb, sheet='ST_desc_trial_rounds', ft, start_col=1, start_row=1)
openxlsx2::wb_save(wb, 'Tables/ST_desc_trial_rounds.xlsx')

# ST2: Interaction tests for significance (anova) ####

# Create main frame with continuous outcome
### Get data
(path <- paste0('Main Results/', outcomes_cont, '/Flood_1Lag-season_flood-treatment/anov.xlsx'))
master <- read.xlsx(path)

### Format 2 decimal places
master$Chisq <- sprintf("%.1f", as.numeric(master$Chisq))
master$P[master$`Pr(>Chisq)` < 0.001] <- "<0.001"  
master$`Pr(>Chisq)` <- sprintf("%.2f", master$`Pr(>Chisq)`)
master$`Pr(>Chisq)`[master$P == '<0.001'] <- "<0.001"
master$P <- NULL 
colnames(master) <- paste0(outcomes_cont, '_', colnames(master)) 
# Merge relevant cols
res <- data.frame()
row_df <- setNames(data.frame(paste0(master[[2]], " (", master[[4]], ")")), outcomes_cont)
res <- rbind(res, row_df)

### Append each binary outcome to the main frame
for (b in outcomes_bin) {
  
  # Get data
  (path <- paste0('Main Results/', b, '/Flood_1Lag-season_flood-treatment/anov.xlsx'))
  new <- read.xlsx(path)
  # Format 2 decimal places
  new$Chisq <- sprintf("%.1f", as.numeric(new$Chisq))
  new$P[new$`Pr(>Chisq)` < 0.001] <- "<0.001"  
  new$`Pr(>Chisq)` <- sprintf("%.2f", new$`Pr(>Chisq)`)
  new$`Pr(>Chisq)`[new$P == '<0.001'] <- "<0.001"
  new$P <- NULL
  colnames(new) <- paste0(b, '_', colnames(new)) 
  # Append columns to master df
  row_df <- setNames(data.frame(paste0(new[[2]], " (", new[[4]], ")")), b)
  res <- cbind(res, row_df)  
}

# Select results for reporting
rownames(res) <- c("(Intercept)", "Flood_1Lag", "season_flood", "treatment", 
                      "WDDS_BL", "ramadan", "g_2h_BL", "quint2_BL", 
                      "Flood_1Lag:season_flood", "Flood_1Lag:treatment", 
                      "season_flood:treatment", "Flood_1Lag:season_flood:treatment")
res <- as.data.frame(t(res)) %>% 
  select('Flood_1Lag', 'season_flood', 'treatment', 'Flood_1Lag:treatment', 
         'Flood_1Lag:season_flood', 'season_flood:treatment', 
         'Flood_1Lag:season_flood:treatment')

# Fix names
ST2_anova <- replace_matches(res, nm)
names(ST2_anova) <- gsub("Treatment", 'HFP', names(ST2_anova))

# Flex table format
ft <- flextable(tibble::rownames_to_column(ST2_anova, " ")) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  autofit()

# Export
wb <- openxlsx2::wb_workbook() %>% openxlsx2::wb_add_worksheet('ST2_anova')
wb <- wb_add_flextable(wb, sheet='ST2_anova', ft, start_col=1, start_row=1)
openxlsx2::wb_save(wb, 'Tables/ST2_anova.xlsx')


# ST3: Marginal effects of 1% flood coverage on dietary outcomes (marginal_effects) ####

# Insert rows and rename existing variables
restructure_levels <- function(df) {
  
  make_label <- function(txt) { r <- replace(df[1,], TRUE, NA); r$Variable <- txt; r$indent <- FALSE; r }
  make_rows  <- function(pattern, label_fn) {
    rows <- df[grepl(pattern, df$Variable), ]
    rows$Variable <- label_fn(rows$Variable)
    rows$indent <- TRUE
    rows
  }
  
  # Overall (just "1")
  overall <- make_label("Flood* impact overall")
  overall_row <- df[df$Variable == "1", ]; overall_row$indent <- TRUE
  overall_row[1,1] <- NA
  # By HFP ("1-0", "1-1")
  by_hfp <- make_label("Flood* impacts by HFP")
  hfp_rows <- make_rows("^1-[01]$", function(x) trt_nm[sub("1-", "", x)])
  
  # By season ("1-Jan/Feb" etc.)
  by_season <- make_label("Flood* impacts by season")
  season_rows <- make_rows(paste0("^1-(", paste(seasons, collapse="|"), ")$"),
                           function(x) sub("1-", "", x))
  
  # By season & HFP ("1-Jan/Feb-0", "1-Jan/Feb-1" etc.)
  by_season_hfp <- make_label("Flood* impacts by season & HFP")
  season_hfp_rows <- do.call(rbind, lapply(c('0','1'), function(t) {
    rows <- make_rows(paste0("^1-(", paste(seasons, collapse="|"), ")-", t, "$"),
                      function(x) paste0(trt_nm[t], ": ", sub("^1-(.+)-[01]$", "\\1", x)))
    rows
  }))
  
  result <- rbind(overall, overall_row,
                  by_hfp, hfp_rows,
                  by_season, season_rows,
                  by_season_hfp, season_hfp_rows)
  rownames(result) <- NULL
  result
}

# Helper to load and format one outcome for a given interaction path
load_int <- function(outcome, f, is_cont) {
  path <- paste0('Main Results/', outcome, f, 'ame1_res.xlsx')
  res  <- read.xlsx(path)
  ci   <- if (is_cont) list(l='lower.CL',   u='upper.CL')   else list(l='asymp.LCL', u='asymp.UCL')
  list(var=res, trend=res[['1.trend']], l=res[[ci$l]], u=res[[ci$u]], p=res[['p.value']])
}

# Helper to build one interaction block across all outcomes
build_block <- function(f, var_fn) {
  get_cols <- function(o) {
    is_cont <- o == outcomes_cont
    d <- load_int(o, f, is_cont)
    format_cols(o, var_fn(d$var), d$trend, d$l, d$u, d$p)
  }
  Reduce(cbind, lapply(c(outcomes_cont, outcomes_bin), get_cols))
}

# Build all interaction blocks
ST3_marginal_effects <- rbind(
  build_block('/Flood_1Lag/',                function(r) round(r$Flood_1Lag, 2)),
  build_block('/Flood_1Lag-treatment/',      function(r) paste(round(r$Flood_1Lag, 2), r$treatment,   sep="-")),
  build_block('/Flood_1Lag-season_flood/',   function(r) paste(round(r$Flood_1Lag, 2), r$season_flood, sep="-")),
  build_block('/Flood_1Lag-season_flood-treatment/', function(r) paste(round(r$Flood_1Lag, 2), r$season_flood, r$treatment, sep="-"))
)
rownames(ST3_marginal_effects) <- ST3_marginal_effects[[paste0(outcomes_cont, '_variables')]]

# Clean and prep
ST3_marginal_effects      <- ST3_marginal_effects %>% select(-ends_with("_variables"), -any_of("var"))
col_names_orig <- colnames(ST3_marginal_effects)
outcomes_all   <- unique(str_remove(col_names_orig, "_(COEF|PROB|CI|P)$"))
header_values  <- c("", sapply(outcomes_all, function(o) nm[o]))
header_widths  <- c(1,  sapply(outcomes_all, function(o) sum(str_detect(col_names_orig, paste0("^", o, "_")))))

colnames(ST3_marginal_effects) <- make.unique(str_replace(col_names_orig, "^.*_(COEF|PROB|CI|P)$", "\\1"))
clean_labels <- setNames(gsub("\\.[0-9]+$", "", colnames(ST3_marginal_effects)), colnames(ST3_marginal_effects))

ST3_marginal_effects <- tibble::rownames_to_column(ST3_marginal_effects, var="Variable")
ST3_marginal_effects$indent <- FALSE

# Restructure and build flextable
trt_nm  <- c('0'='Control', '1'='HFP')
seasons <- c("Jan/Feb", "Mar/Apr", "May/Jun", "Jul/Aug", "Sep/Oct", "Nov/Dec")
result     <- restructure_levels(ST3_marginal_effects)
label_rows <- which(grepl("^Flood\\*", result$Variable))

ft <- flextable(result %>% select(-indent)) %>%
  set_header_labels(values = clean_labels) %>%
  add_header_row(values = header_values, colwidths = header_widths) %>%
  align(part = "header", align = "center") %>%
  padding(i = which(result$indent), j = 1, padding.left = 20) %>%
  vline(j = cumsum(header_widths)[-length(header_widths)] + 1, part = "all") %>%
  theme_booktabs() %>% bold(part = "header") %>%
  bold(i = label_rows, part = "body") %>%
  italic(i = label_rows, part = "body") %>%
  autofit()

# Export
wb <- openxlsx2::wb_workbook() %>% openxlsx2::wb_add_worksheet('ST3_marginal_effects')
wb <- wb_add_flextable(wb, sheet='ST3_marginal_effects', ft, start_col=1, start_row=1)
openxlsx2::wb_save(wb, 'Tables/ST3_marginal_effects.xlsx')




# ST4: Marginal means of 1% flood coverage on dietary outcomes, with difference tests ####

st4 <- list()

### Get continuous outcomes
st4[[outcomes_cont]] <- abs_diff_table(outcome=outcomes_cont, dtype='cont')

### Get binary outcomes
for (b in outcomes_bin) {
  st4[[b]] <- abs_diff_table(outcome=b, dtype='bin')
}

# Store outputs in multiple excel sheets
wb <- openxlsx2::wb_workbook()
for (outcome in names(st4)) {
  
  ft <- st4[[outcome]]
  sheet_name <- nm[outcome]
  
  if(sheet_name=='Nuts/seeds') { sheet_name <- 'Nuts & seeds'}
  
  wb <- wb %>%
    openxlsx2::wb_add_worksheet(sheet_name)
  
  wb <- wb_add_flextable(wb, sheet = sheet_name, ft, start_col = 1, start_row = 1)
}

# Save
openxlsx2::wb_save(wb, 'Tables/ST4_absolute_diff.xlsx')

# ST5: Main effects for impact of 1% flood coverage on dietary outcomes (main_effects) ####

# Create main frame with continuous outcome
### Get data
(path <- paste0('Main Results/', outcomes_cont, '/Flood_1Lag-season_flood-treatment/mod_res.xlsx'))
(mod_res <- read.xlsx(path))
### Format CI & p (2 decimal places)
master <- format_cols(outcomes_cont, mod_res$Variables, mod_res$Estimate, mod_res$Lower_CI, mod_res$Upper_CI, mod_res$P_Value)
names(master)[names(master) == "dd10r_score_m_variables"] <- "variables"  

### Append each binary outcome to the main frame
for (b in outcomes_bin) {
  
  # Get data
  (path <- paste0('Main Results/', b, '/Flood_1Lag-season_flood-treatment/mod_res.xlsx'))
  mod_res <- read.xlsx(path)
  # Format CI & p (2 decimal places)
  new <- format_cols(b, mod_res$Variables, mod_res$Estimate, mod_res$Lower_CI, mod_res$Upper_CI, mod_res$P_Value)
  # Append columns to master df
  master <- cbind(master, new)
  
}

# Re-arrange the row orders
rows_to_move <- c(1, 3:7, 2, 13:17, 8, 19:23, 18, 24:28, 9:12)
(master <- rbind(master[rows_to_move, ], master[-rows_to_move, ]))
rownames(master) <- NULL
ST5_main_effects <- replace_matches(master, nm) %>% select(-ends_with("variables"))
# Set variable name
ST5_main_effects <- cbind(
  Term = c('(Intercept)', 
           'Season (Mar/Apr)', 'Season (May/Jun)', 'Season (Jul/Aug)',
           'Season (Sep/Oct)', 'Season (Nov/Dec)', 
           'Flooding : Season (Jan/Feb)', 
           'Flooding : Season (Mar/Apr)',
           'Flooding : Season (May/Jun)',
           'Flooding : Season (Jul/Aug)',
           'Flooding : Season (Sep/Oct)',
           'Flooding : Season (Nov/Dec)',
           'Season (Jan/Feb) : HFP',
           'Season (Mar/Apr) : HFP',
           'Season (May/Jun) : HFP',
           'Season (Jul/Aug) : HFP',
           'Season (Sep/Oct) : HFP',
           'Season (Nov/Dec) : HFP',
           'Flooding : Jan/Feb season : HFP',
           'Flooding : Season (Mar/Apr) : HFP',
           'Flooding : Season (May/Jun) : HFP',
           'Flooding : Season (Jul/Aug) : HFP',
           'Flooding : Season (Sep/Oct) : HFP',
           'Flooding : Season (Nov/Dec) : HFP',
           'Baseline DDS', 'Ramadan', 'Religion', 'Wealth'),
  ST5_main_effects
)

# Build header values and widths from column names
col_names_orig <- colnames(ST5_main_effects)[-1]  # exclude Term
outcomes_all   <- unique(sub(" (COEF|PROB|CI|P)$", "", col_names_orig))
header_values  <- c("", outcomes_all)
header_widths  <- c(1, sapply(outcomes_all, function(o) sum(startsWith(col_names_orig, o))))

# Clean lower header (remove outcome prefix)
clean_cols  <- make.unique(sub("^.* (COEF|PROB|CI|P)$", "\\1", col_names_orig))
clean_labels <- setNames(gsub("\\.[0-9]+$", "", c("Term", clean_cols)), colnames(ST5_main_effects))

ft <- flextable(ST5_main_effects) %>%
  set_header_labels(values = clean_labels) %>%
  add_header_row(values = header_values, colwidths = header_widths) %>%
  align(part = "header", align = "center") %>%
  vline(j = cumsum(header_widths)[-length(header_widths)], part = "all") %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  autofit()

# Export
wb <- openxlsx2::wb_workbook() %>% openxlsx2::wb_add_worksheet('ST5_main_effects')
wb <- wb_add_flextable(wb, sheet='ST5_main_effects', ft, start_col=1, start_row=1)
openxlsx2::wb_save(wb, 'Tables/ST5_main_effects.xlsx')

# Tables for figures ####

# Create R_Rel-Diff
table1 <- rbind(rel_diff_fig('dd10r_score_m', 'cont'),
                rel_diff_fig('dd10r_min_m', 'bin'),
                rel_diff_fig('dd10r_flesh', 'bin'),
                rel_diff_fig('dd10r_dairy', 'bin'),
                rel_diff_fig('dd10r_eggs', 'bin'),
                rel_diff_fig('dd10r_dglv', 'bin'),
                rel_diff_fig('dd10r_vita', 'bin'),
                rel_diff_fig('dd10r_othv', 'bin'),
                rel_diff_fig('dd10r_othf', 'bin'),
                rel_diff_fig('dd10r_legume', 'bin'),
                rel_diff_fig('dd10r_nuts', 'bin'))

# Create R_Abs-Flood-Levels
f <- '/Flood_1Lag-season_flood/'
table2 <- rbind(abs_val_fig('dd10r_score_m',  'cont', f), 
                abs_val_fig('dd10r_min_m', 'bin', f),
                abs_val_fig('dd10r_flesh', 'bin', f),
                abs_val_fig('dd10r_dairy', 'bin', f),
                abs_val_fig('dd10r_eggs', 'bin', f),
                abs_val_fig('dd10r_dglv', 'bin', f),
                abs_val_fig('dd10r_vita', 'bin', f),
                abs_val_fig('dd10r_othv', 'bin', f),
                abs_val_fig('dd10r_othf', 'bin', f),
                abs_val_fig('dd10r_legume', 'bin', f),
                abs_val_fig('dd10r_nuts', 'bin', f))


# Create R-Abs-Flood-Treat_Levels
f <- '/Flood_1Lag-season_flood-treatment/'
table3 <- rbind(abs_val_fig('dd10r_score_m', 'cont', f), 
                abs_val_fig('dd10r_min_m', 'bin', f),
                abs_val_fig('dd10r_flesh', 'bin', f),
                abs_val_fig('dd10r_dairy', 'bin', f),
                abs_val_fig('dd10r_eggs', 'bin', f),
                abs_val_fig('dd10r_dglv', 'bin', f),
                abs_val_fig('dd10r_vita', 'bin', f),
                abs_val_fig('dd10r_othv', 'bin', f),
                abs_val_fig('dd10r_othf', 'bin', f),
                abs_val_fig('dd10r_legume', 'bin', f),
                abs_val_fig('dd10r_nuts', 'bin', f))

# Rename variables
table1 <- replace_matches(table1, nm)
table2 <- replace_matches(table2, nm)
table3 <- replace_matches(table3, nm)

# Save the data frames to an Excel file with different sheet names
write.xlsx(list(R_Rel_Diff=table1, R_Abs_Flood_Levels=table2, R_Abs_Flood_Treat_Levels=table3), "Tables/Visuals.xlsx")

#### EXPORT ####

# # Export
# tables <- c('MT1_desc_trial', 'ST_desc_trial_rounds', 'ST2_anova', 'ST3_marginal_effects', 'ST5_main_effects')
# for (t in tables){
#   write.xlsx(get(t), paste0("Tables/", t, ".xlsx"), rowNames=TRUE, fileEncoding = "UTF-8")
# }

