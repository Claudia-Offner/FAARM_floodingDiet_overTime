################################################################################
#### GENERATE RESULTS (CATEGORICAL EXPOSURE)  #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/2. Data/')

## Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)


outcome <- 'dd10r_score_m'
fixed <- ' ~ Flood_1Lag * season_flood * treatment + dd10r_score_m_BL + factor(ramadan) + factor(quint2_BL)'
lme_mod <- lme(
  fixed = as.formula(paste0(outcome, fixed)),
  random = list(wcode = (~1|season_id), c_code = (~1)), # Random effects
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  na.action = na.omit,  # Handle missing data using na.omit
  data = df
)
anova(lme_mod)

# ## 3A. Average Marginal Effects (slopes) - as Probabilities
# ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', interaction)), 
#                  at=list(Flood_1Lag = levels[2]), var = 1,
#                  trans='response')$emtrends # probabilities
# (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
# (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))

