# library(gamm4)
# library(gam)
library(mgcv)

# Descriptive Statistics
hist(df$Flood_1Lag)
plot(df$dd10r_score, df$Flood_1Lag)

# GAM model test
df$int_3<- interaction(df$season_flood, df$treatment)

### GAM INTERACTION BUILDING EXPLANATION
### https://stats.stackexchange.com/questions/86766/interactions-in-gam
## y ~ s(x1, by=x2)
# Linear interaction: the effect of x2 in modifying the nonlinear effect 
# of x1 on y is the same amount across all increments of x2 (linear var)
## y ~ te(x1, x2)
# Non-linear interaction: the effect of x2 in modifying the nonlinear 
# effect of x1 on y Is NOT the same amount across all increments of x2 (non-linear var)
### TE() VS TI()
### https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/te 
# te() produces a full tensor product smooth, 
# ti() produces a tensor product interaction, appropriate when the main effects 
# (and any lower interactions) are also present.

# Splines in linear models 
# https://stats.stackexchange.com/questions/32730/how-to-include-an-interaction-term-in-gam

# NOTE: all categorical variables can be treated as linear


#####  Overall effects (no interactions) ##### 
gam_0 <- gam(dd10r_score_m ~ s(Flood_1Lag) + factor(treatment) + season_flood +
                 dd10r_score_m_BL + factor(ramadan) + factor(g_2h_BL) + factor(quint2_BL),
               random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
             weights = wdiet_wt,
             family="gaussian", method = "REML", na.action=na.exclude,
             data = df)
summary(gam_0)
# Plot all splines with LMER marginal effects  in red
plot(gam_0, select=c(1), ylim = c(-5, 5), xlim = c(0, 10)) # Jan/Feb
abline(a = 0, b = 0.01, col = "red")


##### Seasonal effects (flood * season) ##### 
gam_2_seas <- gam(dd10r_score_m ~ ti(Flood_1Lag, by=season_flood) + factor(treatment) + season_flood +
                    dd10r_score_m_BL + factor(ramadan) + factor(g_2h_BL) + factor(quint2_BL),
                    random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
                  weights = wdiet_wt,
                  family="gaussian", method = "REML", na.action=na.exclude,
                  data = df)
summary(gam_2_seas)

# Plot all splines with LMER marginal effects  in red
plot(gam_2_seas, select=c(1), ylim = c(-5, 5), xlim = c(0, 10)) # Jan/Feb
abline(a = 0, b = 0.003, col = 'red') # col = c("red", "blue"), v=7
plot(gam_2_seas, select=c(2), ylim = c(-5, 1), xlim = c(0, 10)) # Mar/Apr
abline(a = 0, b = -0.198, col = "red")
plot(gam_2_seas, select=c(3), ylim = c(-1, 4), xlim = c(0, 30)) # May/Jun
abline(a = 0, b = 0.026, col = "red")
plot(gam_2_seas, select=c(4), ylim = c(-5, 5), xlim = c(0, 30)) # Jul/Aug
abline(a = 0, b = 0.004, col = "red")
plot(gam_2_seas, select=c(5), ylim = c(-5, 5), xlim = c(0, 30)) # Sep/Oct
abline(a = 0, b = 0.012, col = "red")
plot(gam_2_seas, select=c(6), ylim = c(-5, 5), xlim = c(0, 30)) # Nov/Dec
abline(a = 0, b = 0.009, col = "red")


##### Trial effects (flood * treatment) #####
gam_2_treat <- gam(dd10r_score_m ~ ti(Flood_1Lag, by=factor(treatment)) + factor(treatment) + season_flood +
                     dd10r_score_m_BL + factor(ramadan) + factor(g_2h_BL) + factor(quint2_BL),
                     random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
                   weights = wdiet_wt,
                   family="gaussian", method = "REML", na.action=na.exclude,
                   data = df)
summary(gam_2_treat)

# Plot all splines with LMER marginal effects  in red
plot(gam_2_treat, select=c(1), ylim = c(-1, 1), xlim = c(0, 10)) # Jan/Feb
abline(a = 0, b = 0.03, col = "red")
plot(gam_2_treat, select=c(2), ylim = c(-1, 1), xlim = c(0, 10)) # Mar/Apr
abline(a = 0, b = 0.00, col = "red")

##### Season-Trial effects (flood * season * treatment) #####
# y=a+s(x1) + s(x1,by=x2)+s(x1,by=x3)
gam_3 <- gam(dd10r_score_m ~ s(Flood_1Lag) + season_flood + treatment + s(Flood_1Lag, by=season_flood) + s(Flood_1Lag, by=treatment) + s(Flood_1Lag, by=int_3)+ #te(Flood_1Lag,as.numeric(season_flood), by=treatment) + #factor(treatment) + season_flood +
               dd10r_score_m_BL + factor(ramadan) + factor(g_2h_BL) + factor(quint2_BL),
               random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
             weights = wdiet_wt,
             family="gaussian", method = "REML", na.action=na.exclude,
             data = df)
summary(gam_3)

# Plot all splines with LMER marginal effects  in red
plot(gam_3, select=c(3)) # Jan/Feb.CON # , ylim = c(-5, 5), xlim = c(0, 10)
abline(a = 0, b = 0.02, col = "red")
plot(gam_3, select=c(2), ylim = c(-5, 1), xlim = c(0, 10)) # Mar/Apr
abline(a = 0, b = -0.16, col = "red")
plot(gam_3, select=c(3), ylim = c(-1, 4), xlim = c(0, 30)) # May/Jun
abline(a = 0, b = 0.06, col = "red")
plot(gam_3, select=c(4), ylim = c(-5, 5), xlim = c(0, 30)) # Jul/Aug
abline(a = 0, b = 0.01, col = "red")
plot(gam_3, select=c(5), ylim = c(-5, 5), xlim = c(0, 30)) # Sep/Oct
abline(a = 0, b = 0.05, col = "red")
plot(gam_3, select=c(6), ylim = c(-5, 5), xlim = c(0, 30)) # Nov/Dec
abline(a = 0, b = 0.01, col = "red")
# Plot all splines with LMER linear slopes in red
plot(gam_3, select=c(7), ylim = c(-5, 5), xlim = c(0, 10)) # Jan/Feb.TREAT
abline(a = 0, b = 0.00, col = "red")
plot(gam_3, select=c(8), ylim = c(-5, 1), xlim = c(0, 10)) # Mar/Apr
abline(a = 0, b = -0.22, col = "red")
plot(gam_3, select=c(9), ylim = c(-1, 4), xlim = c(0, 30)) # May/Jun
abline(a = 0, b = 0.011, col = "red")
plot(gam_3, select=c(10), ylim = c(-5, 5), xlim = c(0, 30)) # Jul/Aug
abline(a = 0, b = 0.004, col = "red")
plot(gam_3, select=c(11), ylim = c(-5, 5), xlim = c(0, 30)) # Sep/Oct
abline(a = 0, b = -0.003, col = "red")
plot(gam_3, select=c(12), ylim = c(-5, 5), xlim = c(0, 30)) # Nov/Dec
abline(a = 0, b = 0.01, col = "red")




#### OTHER ####

# Quadratic effect test
# df$perc_flooded_c2 <- df$Flood_1Lag^2
# Interaction: (Flood_1Lag + I(Flood_1Lag^2))

library(splines)

plot(ns(df$Flood_1Lag))
plot(df$Flood_1Lag)

formula <- as.formula(paste0('dd10r_score_m ~ Flood_1Lag * season_flood + treatment + dd10r_score_m_BL + factor(ramadan) + factor(quint2_BL)'))
lme_mod <- lme(
  fixed = formula,  # Controls
  random = list(wcode = pdDiag(~1|season_id), c_code = pdDiag(~1)),
  weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
  data = df,
  na.action = na.omit  # Handle missing data using na.omit
)

glmm_mod <- glmer(
  formula= dd10r_score_m ~  Flood_1Lag * season_flood * treatment + 
    dd10r_score_m_BL + ramadan + quint2_BL + 
    (1 + wcode|season_id) + (1 | c_code),
  weights = wdiet_wt,
  data = df,
  family = poisson(), #binomial(link="logit"), # extracts binomial regression on logit scale
  # control=glmerControl(optimizer="bobyqa") # Removes non-convergence warnings
)



# Check interaction significance
(anova(glmm_mod)) # Check interaction significance 

# Check residuals
plot(glmm_mod)
# Check AIC
AIC(glmm_mod)
aic_cont
aic_thresh

(mod_res <- getGLMM(glmm_mod, 0, 0))


(mod_res <- getLME(lme_mod))

summary(lme_mod)
## 3A. Average Marginal Effects (slopes)
ame1 <- emtrends(glmm_mod, as.formula(paste0('pairwise ', '~ Flood_1Lag*season_flood*treatment')),
                 at=list(Flood_1Lag = (0.01 - mean_value)/0.01), var = 1)$emtrends
(ame1_res1 <- summary(ame1,  infer = c(TRUE, TRUE)))



