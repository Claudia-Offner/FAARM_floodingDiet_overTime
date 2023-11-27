# DESCRIPTIVE CHARACTERISTICS OF DATA


# !!!! ATTENTION: THIS FILE IS STILL IN PROGRESS !!!!


library(writexl)

# Set working directory
setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding TimeSeries - CO/4. Data/Final/REPORTING/')

# NB: Need to change count of bin variables to targeted pop of round!!!
# NB: test statistics need to be checked (not matching up with JW)

#### Functions ####
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
    summarise(count=n(),
              occur=sum(!is.na(get(col))),
              prop = (binom.test(occur, count, conf.level = 0.95)$estimate)*100,
              lower_CI = (binom.test(occur, count, conf.level = 0.95)$conf.int[1])*100,
              upper_CI = (binom.test(occur, count, conf.level = 0.95)$conf.int[2])*100,
              p_value = binom.test(occur, count, conf.level = 0.95)$p.value)
  
}


# Identify relevant variables, distinguished by data type
bin <- c("dd10r_starch", "dd10r_legume", "dd10r_nuts",	"dd10r_dairy",	
         "dd10r_flesh",	"dd10r_eggs",	"dd10r_dglv",	"dd10r_vita",	"dd10r_othf",
         "dd10r_othv", "dd10r_min_m") # binary
cont <- c( "dd10r_score_m", "perc_flooded") # continuous


##### GET BL VALUES FOR WOMEN #####
# Get the wcodes for women at BL
w_BL <- df_BL %>% 
       filter(year_season %in% c('2015-1', '2015-2', '2015-3', '2015-4') & !is.na(dd10r_starch))

# For each BL woman, aggregate relevant variables into a new DF
diets_BL <- data.frame(wcode=unique(w_BL$wcode), treatment=w_BL$treatment)
for (i in c(bin, cont)){
  if (i %in% cont) { # Calculate the mean of continuous data
    x <- df_BL %>% 
            group_by(wcode) %>%   # Group the data by ID
            filter(year_season %in% c('2015-1', '2015-2', '2015-3', '2015-4')) %>%
            summarise(mean = mean(get(i), na.rm = TRUE)) %>%
            rename_with(.fn = ~ i, .cols = mean)
  } else { # Calculate the mode of binary data
    x <- df_BL %>% 
      group_by(wcode) %>%   # Group the data by ID
      filter(year_season %in% c('2015-1', '2015-2', '2015-3', '2015-4')) %>%
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

# Add data back into main frame to extract descriptive stats
diets_BL$year_season <- 'baseline'
df <- bind_rows(df, diets_BL, .id = "Source")


####  SUPPLEMENT TABLES 4-7: Descriptive statistics of relevant variables #### 

tre <- summary_bin(df, g1='year_season', group=1, 'dd10r_starch') # treat

# Export descriptive stats for binary data
 for (b in bin) {
   tre <- summary_bin(df, g1='year_season', group=1, b) # treat
   con <- summary_bin(df, g1='year_season', group=0, b) # control
   desc <- rbind(tre, con) # combine treat & control by row
   print(desc)
  # desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], function(x) round(x, digit=3)) # round decimals to 3
  # write_xlsx(desc, paste0("ST 4-7/", b, ".xlsx")) # export to xlsx
}


# Export descriptive stats for continuous data
for (c in cont) {
  tre <- summary_cont(df, g1='year_season', group=1, c) # treat
  con <- summary_cont(df, g1='year_season', group=0, c) # control
  desc <- rbind(tre, con) # combine treat & control by row
  # desc[, sapply(desc, is.numeric)] <- lapply(desc[, sapply(desc, is.numeric)], function(x) round(x, digit=3)) # round decimals to 3
  # write_xlsx(desc, paste0("ST 4-7/", c, ".xlsx")) # export to xlsx
  # write.csv(desc, paste0("ST 4-7/", c, ".csv"), row.names=FALSE)
}


#### TABLE 1: Characteristics of women at baseline by intervention group ####

######Extra ######

# DUMMY CODE VARIABLES
# Create a sample data frame for dummy variables
d <- data.frame(
  season_flood = df$season_flood,
  treatment = df$treatment
)

# Set meaningful labesl to categories
d$treatment[d$treatment == 0] <- 'control'
d$treatment[d$treatment == 1] <- 'treatment'

# Create a new factor variable for combinations of Column1 and Column2
d$Int. <- interaction(d$season_flood, d$treatment)

# Convert the new factor variable to dummy variables
dummy_data <- model.matrix(~ season_flood + Int., data = d)
colnames(dummy_data) <- gsub("Int.", "", colnames(dummy_data)) # remove col characters
colnames(dummy_data) <- gsub("season_flood", "", colnames(dummy_data)) # remove col characters
colnames(dummy_data) <- gsub("/", "", colnames(dummy_data)) # remove col characters

# Combine the dummy variables with the original data frame
df <- cbind(df, dummy_data)
colnames(dummy_data)
