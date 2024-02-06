

# Test associations

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/GitHub/FAARM_Analysis/Data/')

# Load packages
# install.packages('MuMIn')
library(psych)
library(oddsratio)
library(aod)
library(reshape)
library(nlme)
library(MuMIn)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmtest)
library(xts)
library(tsbox)
library(zoo)
library(rgdal)
library(spdep)
library(lubridate)


#### 0. Format data ####

# Load shape data
cluster_shp <- st_read(dsn="C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/96_Cluster_final.shp")
cluster_shp <- st_transform(cluster_shp, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") # set projection
cluster_shp <- cluster_shp %>% select( -c(OBJECTID, Shape_Leng, Shape_Le_1, AREA_M)) %>% rename(c_code = cluster_co)

# Load data
df <- read.csv(file='Cluster100_10mflood_diet_df2.csv', fileEncoding='UTF-8-BOM')

# Select relevant variables
df <- df %>% select(c_code, wcode, year_month, year, month, season, year_season, season_DD, season_flood, 
                    perc_flooded, Flood_1Lag,dd10r_score_m, dd10r_min_m, dd10r_score_m, dd10r_min, 
                    treatment, ramadan, preg, wdiet_wt, dd10r_score_m_BL,dd10r_score_m_EL,
                    age_3_BL, g_2h_BL, fam_type_BL, dep_ratio, g_2h_BL, fam_type_BL, 
                    wi_hl_BL, wi_al_BL, wi_land_BL, num_crops_BL, hfias_BL,
                    woman_edu_cat__BL, mobility_BL, support_BL, 
                    communication_BL, decision_BL, pb_621_BL,know_score_BL,
                    dep_ratio, md_score_BL, wealth_BL, dec_BL, quint_BL, 
                    terc_BL, wealth2_BL, dec2_BL, quint2_BL, terc2_BL)

# Check outcome distribution over time
y <- df %>%
  group_by(year_season) %>%
  summarise(count=n(),
            diet=sum(!is.na(dd10r_score_m)),
            flood=sum(!is.na(Flood_1Lag)),
            DDperc=diet/count*100,
            FloodPerc=flood/count*100)

# REMOVE: year_season that have DD not between 30%-40%
ys_elim <- y$year_season[y$DDperc < 30 | y$DDperc > 40]
for (i in ys_elim) {
  df<-df[!(df$year_season==i),]
} 

# Cubic transformation for flooding (based on initial exploratory analysis)
df$perc_flooded_cub <- df$perc_flooded^(1/3)
df$Flood_1Lag_cub <- df$Flood_1Lag^(1/3)

# Merge to get OBJCTID_1 for spatial effects
df <- as.data.frame(dplyr::left_join(df, cluster_shp, by = c('c_code')))
# Create numeric year-season_DD id for temporal effects
df$season_id <- as.numeric(factor(df$year_season))

# Get spatial weight matrices
adj.mat <- poly2nb(cluster_shp)
W.adj.mat <- nb2mat(adj.mat, style = "B", zero.policy=T) 

# Re-factor Season codes so Jul/Aug is used as the reference level (flooding season)
df$season_DD <- factor(df$season_DD, levels=c("Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb", "Mar/Apr","May/Jun"))
df$season_flood <- factor(df$season_flood, levels=c("Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb", "Mar/Apr","May/Jun"))
# levels(df$season_DD)

# Reset index
rownames(df) <- NULL

#### Create data matrices for autocorrelation analysis

# Cast to wide form (not long form) - Flooding
exp_time <-cast(df, year_season~wcode, mean, value = "perc_flooded") # in years
exp_time$year_season[-length(exp_time$year_season)] <- paste0(exp_time$year_season[-length(exp_time$year_season)], '-01') # Add Day to  year_season
names(exp_time)[names(exp_time)=="year_season"] <- "time" # rename year_season
exp_matrix <-data.matrix(exp_time[,2:ncol(exp_time)])

# Cast to wide form (not long form) - Diet
out_time <-cast(df, year_season~wcode, mean, value = "dd10r_score_m_m") # in years
out_time$year_season[-length(out_time$year_season)] <- paste0(out_time$year_season[-length(out_time$year_season)], '-01') # Add Day to  year_season
names(out_time)[names(out_time)=="year_season"] <- "time" # rename year_season
out_matrix <-data.matrix(out_time[,2:ncol(out_time)])

#Put the information into vectors for analysis
treat <- df[df$treatment == 1,]
control <- df[df$treatment == 0,]


#### 1. Test Crude Association ####

# WANT: 
# - Logistic (outcome is binary, exposure is continuous)
# - Mixed model (we have repeated measures)
# - Accounts for temporal autocorrelation
# - Accounts for cluster/spatial autocorrelation (+wcode random effects)

# https://stats.oarc.ucla.edu/r/dae/logit-regression/

# # Logistic regression (No Lag)
# m1<- glm(dd10r_min ~ perc_flooded+treatment, family=binomial, data = df)
# summary(m1)
# exp(coef(m1)) # Odds ratio w/ CI
# # exp(cbind(OR = coef(m1), confint(m1))) # NOTE: intercept odds is not interpretable!

# Gaussian regression (No Lag)
c1 <- glm(dd10r_score_m ~ perc_flooded+treatment+wealth2_BL+dd10r_score_m_BL, family=gaussian, data = df)
summary(c1)
exp(coef(c1)) # Odds ratio w/ CI
# exp(cbind(OR = coef(c1), confint(c1))) # NOTE: intercept odds is not interpretable!

# Stratify by treatment
c2 <- glm(dd10r_score_m ~ perc_flooded+wealth2_BL+dd10r_score_m_BL, family=gaussian, data = treat)
summary(c2)
c3 <- glm(dd10r_score_m ~ perc_flooded+wealth2_BL+dd10r_score_m_BL, family=gaussian, data = control)
summary(c3)
exp(coef(c2)) # Treatment - negative effect??
exp(coef(c3)) # Control - positive effect??

# Check model autocorrelation 
acf(residuals(c1)) # Indicates a trend & series not stationary (violates assumption) - difference
acf(diff(residuals(c1))) # Alternating +-, decay to zero - AR model. Use the PACF to help identify the order
pacf(residuals(c1)) # lag of 6-8?
# Autocorrelation analysis required

# Test for overall effect of stratified categorical variable (i.e. treatment) - be mindful of order
# wald.test(b = coef(m), Sigma = vcov(m), Terms = 3) # p< 0.001 indicates strong evidence of effect by treatment

# newdata1 <- with(df3, data.frame(perc_flooded = mean(perc_flooded), treat = 0:1))
# newdata1$rankP <- predict(m, newdata = newdata1, type = "response")
# newdata1
# # Predicted probability of have an inadequate diet is 59% for those in control group and 49% for those is treatment group



#### 2. Exploratory Data Analysis ####

#------ FLOODING
exp_mu = mean(exp_matrix) # Mean
exp_sd = sd(exp_matrix) #SD

# Histogram
hist(exp_matrix)
abline(v=exp_mu, col="red")

# Check Transformations for flooding
hist(sqrt(df$perc_flooded)) # quadatic
hist(log(df$perc_flooded)) #log
hist(df$perc_flooded^(1/3)) # cubic - most 'normal'

qqnorm(df$perc_flooded_cub)
qqline(df$perc_flooded_cub, col="red")

# Normal QQ-plot
# qqnorm(exp_matrix)
# qqline(exp_matrix, col="red")

plot(rowMeans(exp_matrix), xlab = "Year-month", ylab = "Flood % ", type="l")
# axis(1, at = seq(9, 49, 10), labels=seq(1960, 2000, 10))
# No clear trend, but there is seasonality over months

# Order heat map by c_code % flooded means to examine effects
ggplot(df) +
  geom_tile(aes(x=year_month, y=reorder(c_code, wcode, mean, order=TRUE), fill = perc_flooded)) +
  scale_fill_gradient(name = "% Flooded",low = "blue", high = "red") +
  labs(x = "Year-Month", y = "Cluster Code")

# EXAMINE BY CENTROID? OR DISTANCE TO BODY OF WATER?
# COMPARE AGAINST BASELINE/ENDLINE CHARACTERISTICS!

#------ DIET 
out_mu = mean(out_matrix) # Mean
out_sd = sd(out_matrix) # SD

# Histogram
barplot(prop.table(table(df$dd10r_min)), ylim=c(0,1), xlab = "Dietary Diversity (1==Inadequate diet)", ylab = "Frecuency (%)", col="#6699FF")
hist(df$dd10r_score_m)


#https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best
# library(fitdistrplus)
# library(logspline)
# library(DescTools)
# install.packages('DescTools')
# descdist(df$dd10r_score_m, discrete = FALSE)
# 
# x <- df$dd10r_score_m
# fit.weibull <- fitdist(x, "weibull")
# fit.norm <- fitdist(x, "norm")
# plot(fit.norm)
# plot(fit.weibull)
# 
# 
# normal_dist <- fitdist(df$perc_flooded, "norm")
# plot(normal_dist)
# PlotECDF(df$dd10r_score_m)
# 
# fitdistr(df$dd10r_score_m, "weibull")


# COMPARE AGAINST BASELINE/ENDLINE CHARACTERISTICS!

#Descriptive Statistics
describeBy(exp, out, mat=TRUE)

plot(rowMeans(out_matrix), xlab = "Year-month", ylab = "Inadequate diet", type="l")
# axis(1, at = seq(9, 49, 10), labels=seq(1960, 2000, 10))
# Possibly a trend, possibly seasonal component

# Order heat map by c_code % flooded means to examine effects
ggplot(df) +
  geom_tile(aes(x=year_month, y=reorder(c_code, wcode, mean, order=TRUE), fill = dd10r_min)) +
  scale_fill_gradient(name = "Dietary Diversity (1=Diet Diverse)",low = "blue", high = "red") +
  labs(x = "Year-Month", y = "Cluster Code")
# EXAMINE BY CENTROID? OR DISTANCE TO BODY OF WATER?

#### 3. Autocorrelation Analysis ####
# http://r-statistics.co/Time-Series-Analysis-With-R.html 
# https://towardsdatascience.com/inferring-causality-in-time-series-data-b8b75fe52c46#1c91


# FLOODING
# Decompose (average over wcodes)
x <- subset(exp_time, select= "time")
x$mean_flood <- rowMeans(exp_matrix)
x <- ts_ts(ts_long(x))
decom <- decompose(na.StructTS(x, na.rm = TRUE), type='multi')
autoplot(decom) # Trend: less flooding over time; Seasonal trends (annual)
decom <- stl(na.StructTS(x, na.rm = TRUE), s.window='periodic') #, t.window=25
autoplot(decom)
# Compare Differencing & Lag effects
p1 <- autoplot(x)
p2 <- autoplot(diff(x))
p3 <- autoplot(diff(x, lag=12)) # because the data is monthly (4 if quarterly?)
grid.arrange(p1,p2,p3)
# Check Lag effects
lag.plot(rowMeans(exp_matrix), lags=6, do.lines=FALSE)
# Quantify Temporal Dependencies
# ACF - measures autocorrelation at specified lags directly
acf(rowMeans(exp_matrix), lag.max = 30) 
# Suggests an annual seasonal pattern & trend (so check seasonal difference)
acf(diff(rowMeans(exp_matrix), lag=12, differences=1), lag.max = 30, xlab="Lag", ylab="ACF", main="Differenced autocorrelation plot") # Removes autocorrelation
# == ARIMA(0,0,2)(0,1,1)12
# PACF - Given we know todays value, how useful is it to know yesterdays value?
pacf(rowMeans(exp_matrix), lag.max=30,xlab="Lag",ylab="PACF",main="Partial Autocorrelation plot of monthly average temperatures")
pacf(diff(rowMeans(exp_matrix), lag=12, differences=1), lag.max=30,xlab="Lag",ylab="PACF",main="Partial Autocorrelation plot of monthly average temperatures")
# == ARIMA(1,0,2)(2,1,1)12

# DIETARY DIVERSITY
# Decompose (average over wcodes)
y <- subset(out_time, select= "time")
y$mean_diet <- rowMeans(out_matrix)
y <- ts_ts(ts_long(y))
decom <- decompose(na.StructTS(y, na.rm = TRUE), type='multi')
autoplot(decom) # trend: increase over time; Seasonal trends
decom <- stl(na.StructTS(y, na.rm = TRUE), s.window='periodic') #, t.window=25
autoplot(decom)
# Compare Differencing & Lag effects
p1 <- autoplot(y)
p2 <- autoplot(diff(y))
p3 <- autoplot(diff(y, lag=12))
grid.arrange(p1,p2,p3)
# Check Lag effects
lag.plot(rowMeans(out_matrix), lags=6, do.lines=FALSE)
# Quantify Temporal Dependencies
acf(rowMeans(out_matrix), lag.max=30) # not very stationary 
acf(diff(rowMeans(out_matrix), lag=12, differences=1), lag.max = 30, xlab="Lag", ylab="ACF", main="Differenced autocorrelation plot") # Removes autocorrelation
pacf(rowMeans(out_matrix), lag.max=30) # not very stationary 
pacf(diff(rowMeans(out_matrix), lag=12, differences=1), lag.max = 30, xlab="Lag", ylab="ACF", main="Differenced autocorrelation plot") # Removes autocorrelation

# ARIMA MODEL
fit1 <- arima(x, order=c(1,0,2),seasonal=list(order=c(2,1,1),period=12))
fit2 <- arima(y, order=c(1,0,2),seasonal=list(order=c(2,1,1),period=12))
tsdiag(fit1)
tsdiag(fit2)

# ADD RESPECTIVE TIME LAGS
# https://statisticsglobe.com/create-lagged-variable-by-group-in-r
df2 <- data.frame(df)
df2 <- df2 %>%
  group_by(wcode) %>%
  dplyr::mutate(time1 = dplyr::lag(perc_flooded, n = 12, default = NA)) %>% # seasonal time lag of 12 by admin_code
  dplyr::mutate(time2 = dplyr::lag(dd10r_min, n = 12, default = NA)) %>% # seasonal time lag of 12 by admin_code
  as.data.frame()


#### TEST: Generalized Linear Models (Mixed Effects)??? ####
# https://stats.stackexchange.com/questions/153631/best-way-to-account-for-time-lags-in-logistic-regression-glm-or-glmm

#### (1) Use a generalized linear mixed-effects model (GLMM) and include the site as a random effect
# m1 <- glmer(DDLag_bin ~ perc_flooded + (1|wcode), data=df, family=binomial)
# summary(m1)
# glmer(y.t ~ x.t.previous + (1|site), data=dat, family=binomial)

#### (2) Use a generalized linear model and include the lagged dependent variable (LDV) as a fixed effect:
# m2 <- glm(DDLag_bin ~ perc_flooded + dd10r_min, data=df, family=binomial)
# summary(m2)
# glm(y.t ~ x.t.previous + y.t.previous, data=dat, family=binomial)

#### (3) Use a GLMM with the LDV as a predictor as a random effect:
# m3 <- glmer(DDLag_bin ~ perc_flooded + (1|dd10r_min), data=df, family=binomial)
# summary(m3)

####  BINARY - Use a GLMM with the LDV as a predictor and a random effect of site
x <- glmer(DDLag_bin ~ perc_flooded + dd10r_min + treatment + (1|c_code) + wealth2_BL + dd10r_min_BL,
           data=df, family=binomial)
#### Stratify model by treatment group
z <- glmer(DDLag_bin ~ perc_flooded + dd10r_min + treatment + (1|c_code) + wealth2_BL + dd10r_min_BL,
           data=treat, family=binomial)
y <- glmer(DDLag_bin ~ perc_flooded + dd10r_min + treatment + (1|c_code) + wealth2_BL + dd10r_min_BL,
           data=control, family=binomial)
summary(x) #  67577.9 AIC
summary(z)
summary(y)
exp(-1.02754) # Odds ratio: perc_flooded
exp(-0.93278) # Odds ratio: perc_flooded for treatment
exp(-1.13165) # Odds ratio: perc_flooded for control

acf(residuals(x)) # Model accounts for most Autocorrelation 
# exp(2.396e-01) # Odds ratio: treatment
# model.sel(m1, m2, m3, x)

#### SCORE - Use a GLMM with the LDV as a predictor and a random effect of site 
x1 <- lm(DDLag_score ~ perc_flooded + dd10r_score_m + treatment + wealth2_BL + dd10r_score_m_BL + year_month + (1|c_code) + (1|wcode),
         data=df)
summary(x1)

x2 <- glmer(DDLag_score ~ perc_flooded + dd10r_score_m + treatment + wealth2_BL + dd10r_score_m_BL + year_month + (1|c_code) + (1|wcode),
            family = 'poisson', data=df)
summary(x2)


#### Stratify model by treatment group
z1 <- lm(DDLag_score ~ perc_flooded + dd10r_score_m + treatment + wealth2_BL + dd10r_score_m_BL + year_month + (1|c_code) + (1|wcode),
         data=treat)
y1 <- lm(DDLag_score ~ perc_flooded + dd10r_score_m + treatment + wealth2_BL + dd10r_score_m + year_month + (1|c_code) + (1|wcode),
         data=control)
summary(z1)
summary(y1)
exp(-0.018967) # Odds ratio: perc_flooded 0.8381778
exp(-0.074182) # Odds ratio: perc_flooded for treatment  0.8173801
exp(0.0582825) # Odds ratio: perc_flooded for control 0.8606658

acf(residuals(x1)) # Model accounts for most Autocorrelation 
# exp(2.396e-01) # Odds ratio: treatment
# model.sel(m1, m2, m3, x)

plot(x1)
hist(residuals(x1), col='steelblue', main='Normal')
qqnorm(residuals(x1), main='Non-normal')
qqline(residuals(x1))

# Assumption Checks
# 1) Existence of variance: Do not need to check, in practice, it is always true.
# 2) Linearity: Do not need to check, because your covariates are categorical.
# 3) Homogeneity: Need to Check by plotting residuals vs predicted values.
# 4) Normality of error term: need to check by histogram, QQplot of residuals, even Kolmogorov-Smirnov test.
# 5) Normality of random effect: Get the estimate of random effect (in your case random intercepts), and check them as check the residual. But it is not efficient because you just have 7 random intercepts.
# 6) Another assumption is the independent between subjects. No test, based on your judgement. Subject specific random intercept means the correlation between the response variable from the same subject are the same.
##########################################

LM <- lm(DDLag_score ~ perc_flooded + dd10r_score_m + treatment + wealth2_BL + dd10r_score_m_BL+ (1|c_code),
         data=df)
summary(LM)
par(mfrow = c(2,2))
plot(LM)

GLM <- gls(DDLag_score ~ perc_flooded + dd10r_score_m + treatment + wealth2_BL + dd10r_score_m_BL+ (1|c_code),
           data=df, method = "ML")
summary(GLM)

# (5) Specify AR1 covariance structure, but it is unclear to me how to do this in a generalized linear model (mixed or otherwise).
# http://midag.cs.unc.edu/shape_stats/GAR01.pdf

#### TEST: R-INLA Model??? ####
require(INLA)

#### TEST: Mediation Model??? ####

library(medflex)
library(dplyr)
library(Matrix)
library(arm)
library(glmnet)

# A. Get direct effect & effect mediated via M1 & M2 (place in reverse?)
# impData <- neImpute(dd10r_min ~ perc_flooded+ treatment,
#                      family = binomial, data = df, nMed=1)
# neMod <- neModel(dd10r_min ~ perc_flooded0 + perc_flooded1,
#                  family = binomial, expData = impData, se = "bootstrap")
# summary(neMod)
# neMod2$neModelFit

##############

# https://stats.stackexchange.com/questions/395445/how-to-account-for-temporal-autocorrelation-in-logistic-regression-with-longitud

m1 <- glmer(DDLag_bin ~ perc_flooded + (1 | c_code), data = df, family = binomial()) # accounts for id as a random effect
summary(m1)
# exp(coef(m1))
acf(residuals(m1)) # still a lot of autocorrelation

m2 <- glmer(DDLag_bin ~ perc_flooded + (1 | year_month), data = df, family = binomial()) # accounts for time as a random effect
summary(m2)
# exp(coef(m2))
acf(residuals(m2)) # still a lot of autocorrelation

m3 <- glmer(DDLag_bin ~ perc_flooded + (1 | year_month) + (1 | wcode), data = df, family = binomial()) # accounts for random effects of time & id 
summary(m3)
# exp(coef(m3))
acf(residuals(m3)) # better but autocorrelation is still present

model.sel(m1, m2, m3)

# + Treatment
#### OTHER PACKAGES ####
# # https://drizopoulos.github.io/GLMMadaptive/
# 
# # https://rpubs.com/markpayne/164550
# # Account for temporal autocorrelation
# mdl.ac <- gls(dd10r_min ~ perc_flooded, data=df3, 
#               correlation = corCompSymm(form= ~1 | year_month),
#               na.action=na.omit) # takes ~10 min
# summary(mdl.ac)
# model.sel(m, Model8)
# 
# # fit model and show variable of interest with Z-score
# fit <- glm(DDLag_bin ~ perc_flooded + treatment+(wcode * year_month), binomial(), df)
# summary(fit) 
# acf(residuals(fit)) # still a lot of autocorrelation
# exp(coef(fit))
# 
# # not sure this is a good idea but we do the plots anyway to show 
# # that there is not sign of auto correlation as expected
# par(mar = c(5, 4, .5, .5), mfcol = c(3, 3))
# tapply(
#   residuals(fit, type = "pearson"), acf, main = "")
# 
# # fit model faster
# library(speedglm)
# fit <- speedglm(
#   dd10r_min ~ perc_flooded + c_code * year_month, family = binomial(), data = df, sparse = TRUE)
# summary(fit)$coefficients["x", , drop = FALSE]
# 
# 
# library(lme4)
# library(MuMIn)
# fm1 <- glmer(dd10r_min ~ perc_flooded + (year_month | wcode), data = df, family = binomial())
# 
# 
# 
# 
# mod <- nlme(dd10r_min ~ perc_flooded, data=df3, fixed= perc_flooded~1, correlation = corAR1(form = ~ 1 | wcode)) 
# summary(mod)
# 
# mod <- lme(dd10r_min ~ perc_flooded, random = ~ year_month | wcode, data=df)
# summary(mod)







# # Markov Imputation (~3 hours)
# sapply(df, function(x) sum(is.na(x))) 
# # Fix treatment missing; fix baseline missing
# df_imp <- mice(df, m = 20, maxit = 35, printFlag = FALSE)  # 20, 35
# df_imp_set <- complete(df_imp, 1)
# sapply(df_imp_set, function(x) sum(is.na(x)))

#### Original  ####
# install.packages('MuMIn')
# library(INLA)
# library(mice)
# library(psych)
# library(oddsratio)
# library(aod)
# library(reshape)
# library(nlme)
# library(MuMIn)
# library(ggplot2)
# library(lmtest)
# library(xts)
# library(tsbox)
# library(zoo)
# library(rgdal)
# library(spdep)
# library(lubridate)
# library(tidyr)
# library(dplyr)
# library(bayestestR)

# ## HERE: This model has AR component - so it accounts for previous values but only investigates current flooding with current DD (?)
# mod_og <- inla(dd10r_score_m ~ perc_flooded_cub*season_DD+factor(treatment)+wealth2_BL+dd10r_score_m_BL+
#                  fam_type_BL + hfias_BL + woman_edu_cat__BL + mobility_BL +
#                  f(wcode, model = 'iid')+
#                  f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
#                    group = season_id, control.group = list(model = "ar1"),
#                    hyper = prec.prior),
#                family ='gaussian', data = df,
#                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#                control.predictor = list(compute = TRUE))
# Res_og <- getINLA_res(mod_og)
# plotResults(Res_og)
# Res_og$ODDS <- exp(Res_og$Mean)
# Res_og
# 
# ## HERE: Seasonality is not adequately captured - we need seasons to match with PREVIOUS flooding season (while also including current DD season)
# mod1 <- inla(dd10r_score_m ~ Flood_1Lag_cub*season_DD+factor(treatment)+wealth2_BL+dd10r_score_m_BL+ 
#                fam_type_BL + hfias_BL + woman_edu_cat__BL + mobility_BL +
#                f(wcode, model = 'iid')+
#                f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
#                  group = season_id, control.group = list(model = "ar1"),
#                  hyper = prec.prior),
#              family ='gaussian', data = df,
#              control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#              control.predictor = list(compute = TRUE))
# Res1 <- getINLA_res(mod1)
# plotResults(Res1)
# Res1$ODDS <- exp(Res1$Mean)
# Res1

################################################################################
library(rgdal)
library(SpatialEpi)
library(rgeos)
library(INLA)

# 0. Load shape data
cluster_shp <- readOGR(dsn="C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/96_Cluster_final.shp")
cluster_shp <- spTransform(cluster_shp, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'))
plot(cluster_shp)

# 1. Get centroids in KM
pts <- gCentroid(cluster_shp, byid=TRUE)@coords
# !!! Transform into km !!!
Lockm <- latlong2grid(pts)
Loc <- cbind(Lockm$x, Lockm$y)

# 2. what are the distances between the points?
D <- dist(Loc)
par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
hist(D, 
     freq = TRUE,
     main = "", 
     xlab = "Distance between sites (km)",
     ylab = "Frequency")

plot(x = sort(D), 
     y = (1:length(D))/length(D), 
     type = "l",
     xlab = "Distance between sites (km)",
     ylab = "Cumulative proportion")

# 3. Create mesh
mesh <- inla.mesh.2d(Loc, 
                     max.edge = c(1, 35), 
                     cutoff   = 1)
mesh$n

par(mar = c(0, 0, 0, 0))
plot(mesh, asp = 1, main = "")
lines(Loc, col = 3, with = 2)

#Create SPDE
meuse.spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
A.meuse <- inla.spde.make.A(mesh = mesh, loc = Loc)
s.index <- inla.spde.make.index(name = "spatial.field",
                                n.spde = meuse.spde$n.spde)

#Create data structure
meuse.stack <- inla.stack(data  = list(zinc = meuse$zinc),
                          A = list(A.meuse, 1),
                          effects = list(c(s.index, list(Intercept = 1)),
                                         list(dist = meuse$dist)),
                          tag = "meuse.data")

