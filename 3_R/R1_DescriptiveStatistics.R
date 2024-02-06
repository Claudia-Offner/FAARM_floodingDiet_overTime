# DESCRIPTIVE CHARACTERISTICS OF DATA

# !!!! ATTENTION: THIS FILE IS STILL IN PROGRESS !!!!

# Set working directory
setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/4. Data/REPORTING/')

# NB: Need to change count of bin variables to targeted pop of round!!!
# NB: test statistics need to be checked (not matching up with JW)

#### Load Functions & Packages ####

# Load packages
library(writexl)

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


# Identify relevant variables, distinguished by data type
bin <- c("dd10r_starch", "dd10r_legume", "dd10r_nuts",	"dd10r_dairy",	
         "dd10r_flesh",	"dd10r_eggs",	"dd10r_dglv",	"dd10r_vita",	"dd10r_othf",
         "dd10r_othv", "dd10r_min_m") # binary
cont <- c( "dd10r_score_m", "perc_flooded_c") # continuous


#### SORT OBSERVATIONS OF WOMEN #####

# Get observations for women over BL 
dates <- c('2015-1', '2015-2', '2015-3', '2015-4')
w_BL <- df_BL %>% filter(year_season %in% dates & !is.na(dd_elig))
nrow(w_BL)
# Get observations for women over surveillance 
w_S <- df %>% filter(!is.na(dd_elig))
nrow(w_S)
# Combine BL & surveillance frame to extract descriptive stats
w_desc <- bind_rows(w_BL, w_S, .id = "Source")
nrow(w_desc) # number of observations
length(unique(w_desc$wcode)) # number of women


# For each BL woman, aggregate multiple BL seasons into a single observation
diets_BL <- data.frame(wcode=unique(w_BL$wcode), treatment=w_BL$treatment)
for (i in c(bin, cont)){
  if (i %in% cont) { # Calculate the mean of continuous data
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
df_BLS <- bind_rows(diets_BL, w_S, .id = "Source")


####  SUPPLEMENT TABLES 4-7: Descriptive statistics of relevant variables #### 

# t <- summary_bin(df, g1='year_season', group=1, 'dd10r_eggs')

# Export descriptive stats for binary data
 for (b in bin) {
   tre <- summary_bin(df_BLS, g1='year_season', group=1, b) # treat
   con <- summary_bin(df_BLS, g1='year_season', group=0, b) # control
   (desc <- rbind(tre, con)) # combine treat & control by row
  desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], function(x) round(x, digit=3)) # round decimals to 3
  write_xlsx(desc, paste0("ST 4-7/", b, ".xlsx")) # export to xlsx
 }

# Export descriptive stats for continuous data
for (c in cont) {
  tre <- summary_cont(df_BLS, g1='year_season', group=1, c) # treat
  con <- summary_cont(df_BLS, g1='year_season', group=0, c) # control
  (desc <- rbind(tre, con)) # combine treat & control by row
  desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], function(x) round(x, digit=3)) # round decimals to 3
  write_xlsx(desc, paste0("ST 4-7/", c, ".xlsx")) # export to xlsx
}


#### TABLE 1: Characteristics of women at baseline by intervention group ####

cont <- c("age_3_BL", "wi_land_BL", "hh1hh_mem_EL",  "dd10r_score_m_BL") # cont
cat <- c("quint2_BL", "woman_edu_cat__BL", 
         "ramadan", "g_2h_BL", "quint2_BL",
         "dd10r_starch", "dd10r_legume", "dd10r_nuts",	"dd10r_dairy",	
         "dd10r_flesh",	"dd10r_eggs",	"dd10r_dglv",	"dd10r_vita",	"dd10r_othf",
         "dd10r_othv", "dd10r_min_m")

# Export descriptive stats for cat data
for (c in cat) {
  desc <- t1_summary_cat(w_BL, c)
  desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], function(x) round(x, digit=3)) # round decimals to 3
  write_xlsx(desc, paste0("T1_Desc_chrctr/", c, ".xlsx")) # export to xlsx
}

# Export descriptive stats for continuous data
for (c in cont) {
  desc <- t1_summary_cont(w_BL, c)
  desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], function(x) round(x, digit=3)) # round decimals to 3
  write_xlsx(desc, paste0("T1_Desc_chrctr/", c, ".xlsx")) # export to xlsx
}




# ######Extra ######
# 
# # DUMMY CODE VARIABLES
# # Create a sample data frame for dummy variables
# d <- data.frame(
#   season_flood = df$season_flood,
#   treatment = df$treatment
# )
# 
# # Set meaningful labesl to categories
# d$treatment[d$treatment == 0] <- 'control'
# d$treatment[d$treatment == 1] <- 'treatment'
# 
# # Create a new factor variable for combinations of Column1 and Column2
# d$Int. <- interaction(d$season_flood, d$treatment)
# 
# # Convert the new factor variable to dummy variables
# dummy_data <- model.matrix(~ season_flood + Int., data = d)
# colnames(dummy_data) <- gsub("Int.", "", colnames(dummy_data)) # remove col characters
# colnames(dummy_data) <- gsub("season_flood", "", colnames(dummy_data)) # remove col characters
# colnames(dummy_data) <- gsub("/", "", colnames(dummy_data)) # remove col characters
# 
# # Combine the dummy variables with the original data frame
# df <- cbind(df, dummy_data)
# colnames(dummy_data)
# 
# 
# 
# 
