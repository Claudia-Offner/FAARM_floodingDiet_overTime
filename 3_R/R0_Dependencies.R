### ------------------------------------------------------------------------ ### 
### FAARM - Dependencies
### ------------------------------------------------------------------------ ### 

# Suppress warnings & turn off scientific notation
options(warn=-1)
options(scipen=999)
set.seed(123)

#### PACKAGES ####
library(openxlsx); library(zoo); library(reshape); library(reshape2); 
library(spdep); library(nlme); library(lme4); library(emmeans); library(tidyr)
library(dplyr); library(ggplot2); library(cli); library(stringr); library(ggmap);
library(patchwork); library(gridExtra); library(ggh4x); library(ggtext); 
library(cowplot)

#### FUNCTIONS ####

# Function to replace exact matches in df from a dictionary
replace_matches <- function(df, dict, exact = TRUE) {
  
  # Replace column names
  colnames(df) <- sapply(colnames(df), function(col) {
    for (key in names(dict)) {
      # Replace prefix if found anywhere in the column name
      if (grepl(key, col, fixed = TRUE)) {
        col <- gsub(key, dict[[key]], col, fixed = TRUE)
      }
    }
    col <- gsub('_', ' ', col, fixed = TRUE)
    col
  })
  
  # Replace row names
  if (!is.null(rownames(df))) {
    rownames(df) <- sapply(rownames(df), function(row) {
      for (key in names(dict)) {
        # Replace prefix if found anywhere in the column name
        if (grepl(key, row, fixed = TRUE)) {
          row <- gsub(key, dict[[key]], row, fixed = TRUE)
        }
      }
      row <- gsub('_', ' ', row, fixed = TRUE)
      row
    })
  }
  
  # Replace cell contents
  df[] <- lapply(df, function(col) {
    if (is.factor(col)) col <- as.character(col)
    
    if (is.character(col)) {
      if (exact) {
        # Exact matches
        col <- ifelse(col %in% names(dict), dict[col], col)
      } else {
        # Partial matches (replace any substring)
        for (key in names(dict)) {
          col <- gsub(key, dict[[key]], col, fixed = TRUE)
        }
      }
    }
    col
  })
  
  return(df)
}

# Function to check folder locations and create if necessary
check_folder_loc <- function(folder) {
  if (!dir.exists(folder)) {
    message("Folder does not exist. Creating location.")
    dir.create(folder, recursive = TRUE)
    return(invisible(TRUE))
  } else {
    answer <- readline(
      "Folder already exists. Overwrite files in this folder? (y/n): "
    )
    if (tolower(answer) != "y") {
      message("Export cancelled by user.")
      return(invisible(FALSE))
    }
  }
  invisible(TRUE)
}

# Function to round numeric columns of data frame
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}

# Function to calculate odds ratios from Logistic Regressions
getORs <- function(df, columns) {
  
  # OR TRANSFORMATION
  for (c in columns) {
    df[[c]] <- exp(df[[c]])
  }
  
  return(df)
}

# Function to calculate probabilities from Logistic Regressions
getPROBS <- function(df, columns) {
  
  # OR TRANSFORMATION
  for (c in columns) {
    df[[c]] <- exp(df[[c]])/ (1 + exp(df[[c]]))
  }
  
  return(df)
}

# Function to read GLMER results in table
getGLMM <- function(glmm_model, var=0, rep=0) {
  
  # Get glmm_results
  glmm_res <- as.data.frame(summary(glmm_model)$coef)
  
  # Calcualte CIs
  colnames(glmm_res) <- c("Estimate", "Std.Error", "z-value", "p-value") # Rename columns
  glmm_res$LowerCI <- round(glmm_res[["Estimate"]] - 1.96 * glmm_res[["Std.Error"]], 4)
  glmm_res$UpperCI <- round(glmm_res[["Estimate"]] + 1.96 * glmm_res[["Std.Error"]], 4)
  glmm_res$Variable <- rownames(glmm_res)
  rownames(glmm_res) <- NULL
  glmm_res <- glmm_res %>% # Re-order columns
    select(Variable, Estimate, Std.Error, LowerCI, UpperCI, `p-value`)
  
  # Plot
  glmm_res$index <- 1:nrow(glmm_res) # set indeglmm_res
  names(glmm_res) <- c("Variables","Estimate","SD","Lower_CI","Upper_CI", 'P_Value',"Index")
  
  # Set significance col (for plotting)
  glmm_res$P_Value <- as.numeric(glmm_res$P_Value)
  glmm_res$Importance <- '0_None'
  glmm_res$Importance[glmm_res$P_Value <= 0.10 & glmm_res$P_Value >= 0.05] <- '1_Weak'
  glmm_res$Importance[glmm_res$P_Value <= 0.05] <- '2_Strong'
  
  # Set variable names
  if (var != 0) {
    glmm_res$Variables <- var
  } 
  
  # Set representation of results
  if (rep=='OR'){
    # Convert to ORs
    glmm_res <- getORs(glmm_res, c('Estimate', 'Lower_CI','Upper_CI'))
    return(round_df(glmm_res, 5))
  } else if (rep=='PROBS') {
    glmm_res <- getPROBS(glmm_res, c('Estimate', 'Lower_CI','Upper_CI'))
    return(round_df(glmm_res, 5))
  } else {
    return(round_df(glmm_res, 5))
  }
  
}

# Function to read LME results in table
getLME <- function(lme_model, var=0){
  
  # Use the summary function to get a summary of the model
  summary_table <- summary(lme_model)
  
  # Extract relevant information (coefficient estimates, standard errors, p-values, and confidence intervals)
  lme_res <- round(as.data.frame(summary_table$tTable[, c("Value", "Std.Error", "DF", "t-value", "p-value")]), 4)
  lme_res$Variable <- rownames(lme_res)
  rownames(lme_res) <- NULL
  colnames(lme_res) <- c("Estimate", "Std.Error", "df", "t-Value", "p-value", "Variable") # Rename columns
  
  # Calculate confidence intervals
  lme_res$LowerCI <- round(lme_res$Estimate - qt(0.975, summary_table$tTable[, "DF"]) * lme_res$Std.Error, 4)
  lme_res$UpperCI <- round(lme_res$Estimate + qt(0.975, summary_table$tTable[, "DF"]) * lme_res$Std.Error, 4)
  lme_res <- lme_res %>% # Re-order columns
    select(Variable, Estimate, Std.Error, LowerCI, UpperCI, `p-value`)
  
  # Plot
  lme_res$index <- 1:nrow(lme_res) # set index
  names(lme_res) <- c("Variables","Estimate","SD","Lower_CI","Upper_CI", 'P_Value',"Index")
  
  # Set significance col (for plotting)
  lme_res$P_Value <- as.numeric(lme_res$P_Value)
  lme_res$Importance <- '0_None'
  lme_res$Importance[lme_res$P_Value <= 0.10 & lme_res$P_Value >= 0.05] <- '1_Weak'
  lme_res$Importance[lme_res$P_Value <= 0.05] <- '2_Strong'
  
  # Get results
  if(var != 0) {
    
    lme_res$Variables <- var
    
  }
  
  return(round_df(lme_res, 5))
  
}

# Function for formatting contrast and trend tables for forest plotting function
formatRES <- function(df) {
  for (c in names(df)) {
    
    est <- c('estimate', '1.trend', 'emmean', 'prob')
    lci <- c('asymp.LCL', 'lower.CL')
    uci <- c('asymp.UCL', 'upper.CL')
    if (c %in% est) {
      colnames(df)[colnames(df) %in% est] <- "Estimate"
    } 
    if (c %in% lci) {
      colnames(df)[colnames(df) %in% lci] <- "Lower_CI"
    } 
    if (c %in% uci) {
      colnames(df)[colnames(df) %in% uci] <- "Upper_CI"
    } 
    if (all('p.value' == c)) {
      colnames(df)[colnames(df) %in% 'p.value'] <- "P_Value"
    } 
    if (all('SE' == c)) {
      colnames(df)[colnames(df) %in% 'SE'] <- "SD"
    } 
  }
  
  # Plot
  df$Index <- 1:nrow(df) # set index
  df$Variables <- apply(df[, 1:3], 1, paste, collapse = ":") # set variables
  df <- df[, c("Variables", "Estimate", "SD", "Lower_CI", "Upper_CI", "P_Value", "Index")]
  
  # Set significance col (for plotting)
  df$P_Value <- as.numeric(df$P_Value)
  df$Importance <- '0_None'
  df$Importance[df$P_Value <= 0.10 & df$P_Value >= 0.05] <- '1_Weak'
  df$Importance[df$P_Value <= 0.05] <- '2_Strong'
  
  return(df)
}

# Function to plot results in forest plot; 
# NOTE: use round_df function on df
plotResults <- function(res1, x=0, name=0) {
  
  res <- round_df(res1, 3)
  
  if (name != 0) {
    result_name <- name
  } else {
    result_name <- deparse(substitute(res1))
  }
  
  cols <- c("0_None" = "#dadada","1_Weak" = "#ff9530","2_Strong" = "#029921")
  
  # Plot Results
  ggplot(data=res, aes(y=Index, x=Estimate, xmin=Lower_CI, xmax=Upper_CI)) +
    geom_point() +
    geom_text(aes(label = Estimate, colour = Importance),
              size = 3.5, nudge_x = 1.5, nudge_y = 0, check_overlap = FALSE) +
    scale_colour_manual(values = cols) +
    theme(legend.position = "bottom") +
    geom_errorbarh(height=.3) +
    scale_y_continuous(breaks=1:nrow(res), labels=res$Variables) +
    labs(title=paste('Effect Size by Variable - ', result_name), x='Effect Size', y = 'Variable') +
    geom_vline(xintercept=x, color='black', linetype='dashed', alpha=.5) +
    theme_minimal()
  
}

#### OTHER ####

# Set formula names
var <- c('(Intercept) Jan/Feb season', 
         'Flood Extent: Jan/Feb season', 
         'Mar/Apr season', 'May/Jun season', 'Jul/Aug season',
         'Sep/Oct season', 'Nov/Dec season', 
         'Jan/Feb season : Treatment',
         'WDDS (BL)', 'Ramadan', 'Religion', 'Wealth',
         'Flood Extent : Mar/Apr season',
         'Flood Extent : May/Jun season',
         'Flood Extent : Jul/Aug season',
         'Flood Extent : Sep/Oct season',
         'Flood Extent : Nov/Dec season',
         'Mar/Apr season : Treatment',
         'May/Jun season : Treatment',
         'Jul/Aug season : Treatment',
         'Sep/Oct season : Treatment',
         'Nov/Dec season : Treatment',
         'Flood Extent : Jan/Feb season : Treatment',
         'Flood Extent : Mar/Apr season : Treatment',
         'Flood Extent : May/Jun season : Treatment',
         'Flood Extent : Jul/Aug season : Treatment',
         'Flood Extent : Sep/Oct season : Treatment',
         'Flood Extent : Nov/Dec season : Treatment')

# Set outcomes
outcomes_cont <- "dd10r_score_m"
outcomes_bin <- c("dd10r_min_m", "dd10r_flesh", "dd10r_dairy", "dd10r_eggs",
                  "dd10r_dglv", "dd10r_vita", "dd10r_othv", "dd10r_othf",
                  "dd10r_legume", "dd10r_nuts")

# Set ggplot theme
theme_set(theme_classic())

# Set seasons
seasons <- c("Jan/Feb", "Mar/Apr", "May/Jun", "Jul/Aug", "Sep/Oct", "Nov/Dec")

# Set aesthetics 
custom_colors_sig <- c("p>0.05"="grey", "p<0.05"="black")
custom_lines <- c("p>0.05"="dashed", "p<0.05"="solid")
custom_shapes <- c("Overall"=16, "Control"=15 , "HFP"=17)
custom_colors <- c('Overall'="#2b2a2a",
                   'Jan/Feb'="#ce1126", 
                   'Mar/Apr'="#f2609e", 
                   'May/Jun'="#e66300",
                   'Jul/Aug'="#80b517",
                   'Sep/Oct'="#057dcd",
                   'Nov/Dec'="#7849b8")

# Name dictionary
nm <- list('dd10r_score_m'='DDS', "dd10r_min_m"='MDD', 
           'dd10r_starch'='Starchy staples', "dd10r_flesh"='Flesh foods', 
           "dd10r_dairy"='Dairy', "dd10r_eggs"='Eggs',
           "dd10r_dglv"='DGLV', "dd10r_vita"='Vit. A-rich foods',
           "dd10r_othv"='Other vegetables', "dd10r_othf"='Other fruits',
           "dd10r_legume"='Legumes', "dd10r_nuts"='Nuts/seeds',
           'age_3_BL'='Age', 'wi_land_BL'='Land owned', 'hh1hh_mem_EL'='Household members',
           'woman_edu_cat__BL__0'='Education: None', 
           'woman_edu_cat__BL__1'='Education: Partial primary',
           'woman_edu_cat__BL__2'='Education: Complete primary',
           'woman_edu_cat__BL__3'='Education: Partial secondary',
           'woman_edu_cat__BL__4'='Education: Complete secondary', # ADD 4 AND 5 TOGETHER!!
           'quint2_BL__1'='Wealth: Poorest',
           'quint2_BL__2'='Wealth: Lower',
           'quint2_BL__3'='Wealth: Middle',
           'quint2_BL__4'='Wealth: Upper',
           'quint2_BL__5'='Wealth: Wealthiest',
           'g_2h_BL__1'='Religion: Muslim', 'g_2h_BL__2'='Religion: Hindu',
           'treatment'='Treatment','control' = 'Control')

# "Dietary diversity scores*",
#  "Minimum dietary diversity",