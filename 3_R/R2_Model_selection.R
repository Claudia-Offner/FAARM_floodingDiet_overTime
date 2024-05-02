################################################################################
#### MODEL SELECTION #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/2. Data/')

## Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)

# Interaction testing
# https://stats.stackexchange.com/questions/60362/choice-between-type-i-type-ii-or-type-iii-anova
## NB: stats::anova runs type 1 tests; car::Anova runs type 2 & 3 tests
library(car)


### MODEL BUILDING
# Test 3-WAY INTERACTION: are they signficant?
# Deconstruct models and test different fit combination: are they significantly different? (no)
# Run this analysis for each outcome: Is this trend seen across food groups? (some might not be important)


# 1. Test Continuous Outcomes ####
### NB: glmer models not used because significance test challenges for Gaussian distributions

# 3-WAY TEST - FLOOD*SEASON*TREATMENT
outcome <- 'dd10r_score_m'
fixed <- ' ~ Flood_1Lag * season_flood * treatment + dd10r_score_m_BL + factor(ramadan) + factor(quint2_BL)'
lme3_mod <- lme(
  fixed = as.formula(paste0(outcome, fixed)),
  random = list(wcode = (~1|season_id), c_code = (~1)), # Random effects
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  na.action = na.omit,  # Handle missing data using na.omit
  data = df
)

# Get results
(mod_res <- getLME(lme3_mod))
(stats::anova(lme3_mod)) # Check interaction significance (sequential, good for interactions)





#########
# 2-WAY TEST - FLOOD*SEASON
outcome <- 'dd10r_score_m'
fixed <- ' ~ Flood_1Lag + season_flood + treatment + dd10r_score_m_BL + factor(ramadan) + factor(quint2_BL)'
lme_mod <- lme(
  fixed = as.formula(paste0(outcome, fixed)),
  random = list(wcode = (~1|season_id), c_code = (~1)), # Random effects
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  na.action = na.omit,  # Handle missing data using na.omit
  data = df
)

# Get results
(mod_res <- getLME(lme2_mod))
(stats::anova(lme2_mod)) # Check interaction significance (sequential, good for interactions)


fit1 <- update(lme_mod, . ~ . + Flood_1Lag:season_flood)
fit3 <- update(lme_mod, . ~ . + Flood_1Lag:season_flood:treatment)

(car::anova(lme_mod, fit1, fit2, fit3)) # Check interaction significance 


summary(fit1)
# 3. Test Binary Outcomes ####
### NB: Generalized linear models used for binary outcomes (logit link function)

outcome <- 'dd10r_min_m'
fixed <- ' ~ Flood_1Lag * season_flood * treatment + dd10r_score_m_BL + factor(ramadan) + factor(quint2_BL)'
glmm_mod <- glmer(
  formula=as.formula(paste0(outcome, fixed, '+ (1 + wcode|season_id) + (1 | c_code)')),
  weights = wdiet_wt,
  data = df,
  family = binomial(link="logit"), # extracts binomial regression on logit scale
  control=glmerControl(optimizer="bobyqa") # Removes non-convergence warnings
)

# Get results - as Odds Ratios
(mod_res <- getGLMM(glmm_mod, 0, 'PROB'))
(stats::anova(glmm_mod, test = "Cp")) # Check interaction significance (sequential, good for interactions)
(car::Anova(glmm_mod)) # Check interaction significance 



library(MuMIn)

r.squaredGLMM(fit3)


# ## 3A. Average Marginal Effects (slopes) - as Probabilities
# ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', interaction)), 
#                  at=list(Flood_1Lag = levels[2]), var = 1,
#                  trans='response')$emtrends # probabilities
# (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
# (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))


##############


