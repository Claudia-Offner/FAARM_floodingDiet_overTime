################################################################################
#### MODEL SELECTION #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/3. Analysis/')

## Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)

# Interaction testing
# https://stats.stackexchange.com/questions/60362/choice-between-type-i-type-ii-or-type-iii-anova
## NB: stats::anova runs type 1 tests; car::Anova runs type 2 & 3 tests
library(car)
library(emmeans)
# install.packages('car', lib=library)

### MODEL BUILDING
# Test 3-WAY INTERACTION: are they signficant?
# Deconstruct models and test different fit combination: are they significantly different? (no)
# Run this analysis for each outcome: Is this trend seen across food groups? (some might not be important)


# 1. Test Continuous Outcomes ####
### NB: glmer models not used because significance test challenges for Gaussian distributions

# 3-WAY TEST - FLOOD*SEASON*TREATMENT
outcome <- 'dd10r_score_m'
fixed <- ' ~ Flood_1Lag*season_flood*treatment + dd10r_score_m_BL + ramadan + quint2_BL'
lme3_mod <- lme(
  fixed = as.formula(paste0(outcome, fixed)),
  random = list(wcode = (~1|season_id), c_code = (~1)), # Random effects
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  na.action = na.omit,  # Handle missing data using na.omit
  data = df
)
(anov <- car::Anova(lme3_mod, type=2)) # Check interaction significance (sequential, good for interactions)
anov <- cbind(row.names(anov), anov)

anova(lme3_mod)

##############
getLME(lme3_mod)
(emm <- emmeans(lme3_mod, pairwise ~ Flood_1Lag*season_flood*treatment))$emmeans
(ame1 <- emtrends(lme3_mod, pairwise ~ Flood_1Lag*treatment|season_flood, at=list(Flood_1Lag = levels[2]), var = 1)$emtrends)


(emm <- emmeans(lme3_mod, pairwise ~ Flood_1Lag*season_flood))
(emm <- emmeans(lme3_mod, pairwise ~ Flood_1Lag*treatment))

fib.rg = ref_grid(lme3_mod, at = list(Flood_1Lag = c(0, 1, 2, 3))) # for linear flood
emmip(fib.rg, treatment ~ Flood_1Lag | season_flood, style='factor', CIs=TRUE,col = "black",
      linearg = list(), dotarg = list(size = 2), CIarg = list(alpha = 1)) +
  ggplot2::theme_bw()

emmip(fib.rg, ~ Flood_1Lag | season_flood, style='factor', CIs=TRUE,col = "black",
      linearg = list(), dotarg = list(size = 2), CIarg = list(alpha = 1)) +
  ggplot2::theme_bw()

emmip(fib.rg, treatment ~ Flood_1Lag , style='factor', CIs=TRUE,col = "black",
      linearg = list(), dotarg = list(size = 2), CIarg = list(alpha = 1)) +
  ggplot2::theme_bw()


plot(emmeans(lme3_mod, ~ Flood_1Lag | season_flood*treatment))
emmip(lme3_mod, treatment ~ Flood_1Lag | season_flood, CIs = TRUE,
      CIarg = list(lwd = 1, alpha = 1, color = "cyan"),
      dotarg = list(color = "black"))
emmip(lme3_mod, treatment ~ Flood_1Lag | season_flood, CIs = TRUE, col = "black",
      linearg = list(), dotarg = list(size = 2), CIarg = list(alpha = 1)) +
  ggplot2::theme_bw()

emms1 <- emmeans(lme3_mod,  ~Flood_1Lag | season_flood * treatment)
(con1 <- contrast(emms1, interaction = "pairwise"))
(x <- pairs(con1, by = NULL))

remotes::install_github("rvlenth/emmeans", dependencies = TRUE, build_opts = "")


comparisons(lme3_mod, variables = list(Flood_1Lag='reference'))

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

outcome <- 'dd10r_nuts'
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
(car::Anova(glmm_mod,type=3)) # Check interaction significance 



library(MuMIn)

r.squaredGLMM(fit3)


# ## 3A. Average Marginal Effects (slopes) - as Probabilities
# ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', interaction)), 
#                  at=list(Flood_1Lag = levels[2]), var = 1,
#                  trans='response')$emtrends # probabilities
# (ame1_res <- summary(ame1,  infer = c(TRUE, TRUE)))
# (ame1_cont <- summary(contrast(ame1, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))


##############


