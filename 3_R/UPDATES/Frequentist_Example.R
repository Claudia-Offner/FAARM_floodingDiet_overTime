# RUN 3-WAY INTERACTION (FREQUENTIST)

#### Detatch packages & clear environment/plots
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
# rm(list = ls())

#### Load packages
library(dplyr)
library(spdep)
library(nlme) 
library(margins) # https://www.rdocumentation.org/packages/margins/versions/0.3.26
library(emmeans) # https://www.rdocumentation.org/packages/emmeans/versions/1.8.8/topics/emmeans

#### IMPORTANT - set file path to data folder location
setwd('C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding TimeSeries - CO/4. Data/Final')

# write.csv(marg, "C:/Users/ClaudiaOffner/Downloads/test.csv", row.names=FALSE)


#### FUNCTIONS ####

# Function to round numeric columns of data frame
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# Function to read LME results in table
getLME_res <- function(model){
  
  # Use the summary function to get a summary of the model
  summary_table <- summary(model)
  
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
  names(lme_res) <- c("Variables","Mean","SD","Lower_CI","Upper_CI", 'P_Value',"Index")
  
  # Set significance col (for plotting)
  lme_res$P_Value <- as.numeric(lme_res$P_Value)
  lme_res$Importance <- '0_None'
  lme_res$Importance[lme_res$P_Value <= 0.10 & lme_res$P_Value >= 0.05] <- '1_Weak'
  lme_res$Importance[lme_res$P_Value <= 0.05] <- '2_Strong'
  
  return(round_df(lme_res, 5))
  
}

# Function to plot R-INLA results in forest plot; NOTE: use round_df function on df
plotResults <- function(res1, x=0) {
  
  res <- round_df(res1, 3)
  
  result_name <- deparse(substitute(res1))
  cols <- c("0_None" = "#dadada","1_Weak" = "#ff9530","2_Strong" = "#029921")
  
  # Plot Results
  ggplot(data=res, aes(y=Index, x=Mean, xmin=Lower_CI, xmax=Upper_CI)) +
    geom_point() +
    geom_text(aes(label = Mean, colour = Importance),
              size = 3.5, nudge_x = 1.5, nudge_y = 0, check_overlap = FALSE) +
    scale_colour_manual(values = cols) +
    theme(legend.position = "bottom") +
    geom_errorbarh(height=.3) +
    scale_y_continuous(breaks=1:nrow(res), labels=res$Variables) +
    labs(title=paste('Effect Size by Variable - ', result_name), x='Effect Size', y = 'Variable') +
    geom_vline(xintercept=x, color='black', linetype='dashed', alpha=.5) +
    theme_minimal()
  
}

# Variable Names
var <- c('(Intercept) Jan/Feb season', 
          'Flood Extent: Jan/Feb season', 
          'Mar/Apr season', 'May/Jun season', 'Jul/Aug season',
          'Sep/Oct season', 'Nov/Dec season', 
          'Jan/Feb season : Treatment',
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

#### 0. Example Interaction model for VanderWeele ####

# Example of interactionR package
# library(interactionR)
# data (OCdata)
# 
# ## fit the interaction model
# model.glm <- glm(oc ~ alc*smk, family = binomial(link = "logit"), data = OCdata)
# ## format tables
# table_object = interactionR(model.glm, exposure_names = c("alc", "smk"), ci.type = "mover", ci.level = 0.95, em = T, recode = F)
# ## display
# interactionR_table(table_object)


#### 1. Load & Clean Data ####

# Load FAARM data
# df <- read.csv(file='2_FAARM_GEE_df.csv', fileEncoding='UTF-8-BOM')
data <- read.csv(file='3_FloodMetrics2.csv', fileEncoding='UTF-8-BOM')

# Select relevant variables
df <- data %>% select(c_code, wcode, year_season, year, season, season_DD, season_flood,
                      perc_flooded, Flood_1Lag, flooded_diff, flooded_anom_w_lag, flooded_anom_lag, flooded_weight_lag,
                      dd_elig, dd10r_starch, dd10r_legume, dd10r_nuts,	dd10r_dairy,	dd10r_flesh,	
                      dd10r_eggs,	dd10r_dglv,	dd10r_vita,	dd10r_othf,	dd10r_othv,	
                      dd10r_score, dd10r_min, dd10r_score_m, dd10r_min_m, wdiet_wt,
                      temp_mean, temp_min, temp_max, temp_mean_lag,
                      evap_mean, evap_min, evap_max, evap_mean_lag,
                      ndvi_mean, ndvi_min, ndvi_max, ndvi_mean_lag,
                      prec_mean, prec_min, prec_max, prec_mean_lag, elev,
                      treatment, ramadan, preg, dd10r_score_m_BL, dd10r_score_m_EL,
                      age_3_BL, g_2h_BL, fam_type_BL, dep_ratio, g_2h_BL, fam_type_BL,
                      wi_hl_BL, wi_al_BL, wi_land_BL, num_crops_BL, hfias_BL,
                      woman_edu_cat__BL, mobility_BL, support_BL,
                      communication_BL, decision_BL, know_score_BL,
                      dep_ratio, md_score_BL, wealth_BL, dec_BL, quint_BL,
                      terc_BL, wealth2_BL, dec2_BL, quint2_BL, terc2_BL)

# Check outcome distribution over time
y <- df %>%
  group_by(year_season, treatment) %>%
  summarise(count=n(),
            diet=sum(!is.na(dd10r_score_m)),
            flood=sum(!is.na(perc_flooded)),
            DDperc=diet/count*100,
            FloodPerc=flood/count*100)

# Create a copy of DF with BL values
# df_BL <- df
# df_BL$Baseline[df_BL$year_season %in% c('2015-1', '2015-2', '2015-3', '2015-4')] <- 'Baseline'

# REMOVE: year_season that have DD not between 30%-40% 
# NOTE: do not include 2019_05 because the cluster averages missing
ys_elim <- y$year_season[y$DDperc < 29 | y$DDperc > 39]
for (i in ys_elim) {
  df<-df[!(df$year_season==i),]
}

# Create numeric year-season_DD id for temporal effects
df$season_id <- as.numeric(factor(df$year_season))

# Re-factor Season codes so Mar/Apr is used as the reference level (dry season)
df$season_flood <- factor(df$season_flood, levels=c("Jan/Feb", "Mar/Apr","May/Jun", "Jul/Aug", "Sept/Oct", "Nov/Dec"))
# levels(df$season_DD) # Check levels

# Reset index
rownames(df) <- NULL

# Scale main flood exp
sd(df$Flood_1Lag) # SD is approx 1% flood coverage
df$Flood_1Lag  <- (df$Flood_1Lag - mean(df$Flood_1Lag))/sd(df$Flood_1Lag)
# df$Flood_1Lag  <- (df$Flood_1Lag - min(df$Flood_1Lag))/(max(df$Flood_1Lag) - min(df$Flood_1Lag)) # normalise?



#### 2. Run Model ####

# Model (Temporal - no spatial)
lme_mod <- lme(
  fixed = dd10r_score_m ~ Flood_1Lag*season_flood*treatment, # Flood_1Lag + season_flood:Flood_1Lag + treatment:Flood_1Lag + 
  random = ~1 | wcode,
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  correlation = corAR1(form = ~ season_id | wcode),  # Adding temporal autocorrelation
  data = df,
  na.action = na.omit  # Handle missing data using na.omit
)

# Get results
lme_res <- getLME_res(lme_mod)
lme_res$Variables <- var

# Get marginal effects by subgroup
marg <- summary(margins(lme_mod, at = list(treatment=c(0, 1), season_flood=c('Jan/Feb', 'Mar/Apr', 'May/Jun', 'Jul/Aug', 'Sept/Oct', 'Nov/Dec')), variables = "Flood_1Lag"))
main_marg <- summary(margins(lme_mod))

# Compare difference in seasonal flooding between trreat groups (equiv. to stata contrasts)
cont <- emmeans(lme_mod, pairwise ~ Flood_1Lag*season_flood*treatment,simple = c("treatment"))

# Print the result tables
plotResults(lme_res)
print(lme_res)
print(marg)
print(main_marg)
print(cont$contrasts)

##### EXAMPLE MANUAL CHECKS #####

-0.1263 # Flood:Cont:Jan/Feb (flood(Jan/Feb))
-0.1263 + -0.2936 # Flood:Cont:Mar/Apr (flood(Jan/Feb)  + flood:season(Mar/Apr))
-0.1263 + 0.1787 # Flood:Cont:Jul/Aug (flood(Jan/Feb)  + flood:season(Jul/Aug ))
# etc.
-0.1263 + -0.1002 # Flood:Treat:Jan/Feb (flood + flood:treat)
-0.1263 + -0.1002 + -0.2936 + -0.4745 # Flood:Treat:Mar/Apr (flood(Jan/Feb) + treat:season(Mar/Apr) + flood:season(Mar/Apr) + flood:season:treat(Mar/Apr))
-0.1263 + -0.1002 + 0.2762 + 0.0028 # Flood:Treat:May/Jun (flood(Jan/Feb) + treat:season(Jan/Feb) + flood:season(May/Jun) + flood:season:treat(May/Jun))
-0.1263 + -0.1002 + 0.1787 + 0.1084 # Flood:Treat:Jul/Aug  (flood(Jan/Feb) + treat:season(Jan/Feb) + flood:season(Jul/Aug ) + flood:season:treat(Jul/Aug ))
# etc.

