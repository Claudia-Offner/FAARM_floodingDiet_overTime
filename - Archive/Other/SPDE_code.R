### !!!! TAKES 1 HOUR TO RUN !!!!

library(rgdal)
library(SpatialEpi)
library(rgeos)
library(INLA)
library(dplyr)
library(bayestestR)
library(ggplot2)

# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/GitHub/FAARM_Analysis/Data/')

# Ensure that RINLA can handle big datasets
inla.setOption("num.threads", 4)

#### 1. Format & Merge Data ####

# Load FAARM data
df <- read.csv(file='Cluster100_10mflood_diet_df.csv', fileEncoding='UTF-8-BOM')

# Load shape data (for SPDE)
cluster_shpDF <- readOGR(dsn="C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/96_Cluster_final.shp")
cluster_shpDF <- spTransform(cluster_shpDF, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'))
cluster_shpDF@data <- cluster_shpDF@data %>% select( -c(OBJECTID, Shape_Leng, Shape_Le_1, AREA_M)) %>% dplyr::rename(c_code = cluster_co)
cluster_shpDF@data$c_code <- as.numeric(cluster_shpDF@data$c_code)


# Select relevant variables
df <- df %>% select(c_code, wcode, season, season_DD, year, month, year_month, year_season, 
                    treatment, perc_flooded, Flood_1Lag,
                    dd10r_score, dd10r_min,
                    age_3_BL, g_2h_BL, fam_type_BL, dep_ratio,  dd10r_score_EL,
                    dd10r_score_BL, dd10r_min_BL, g_2h_BL, fam_type_BL, 
                    wi_hl_BL, wi_al_BL, wi_land_BL, num_crops_BL, hfias_BL,
                    woman_edu_cat__BL, mobility_BL, support_BL, 
                    communication_BL, decision_BL, pb_621_BL,know_score_BL,
                    dep_ratio, md_score_BL, wealth_BL, dec_BL, quint_BL, 
                    terc_BL, wealth2_BL, dec2_BL, quint2_BL, terc2_BL)
df$c_code <- as.numeric(df$c_code)

### NOTE: Removing 2015 for now to keep season_DDs consistent 
df <-subset(df, year != 2015)

# Round outcome variable
df <- df %>% dplyr::mutate_at(vars(dd10r_min), funs(round(., 0))) #!!!!!!!!!!!!

# Cubic transformation for flooding (based on initial exploratory analysis)
df$perc_flooded_cub <- df$perc_flooded^(1/3)
df$Flood_1Lag_cub <- df$Flood_1Lag^(1/3)

# Create numeric year-season_DD id for temporal effects
df$season_id <- as.numeric(factor(df$year_season))

# Add lag for season codes (improve interpret ability)
df <- df %>%
  group_by(wcode) %>%
  dplyr::mutate(season_flood = dplyr::lag(season_DD, n = 1, default = NA)) %>% # general time  lag of 1 by DD season
  as.data.frame()
# Check NAs
colSums(is.na(df)) # !!! fill season_flood NA with Nov/Dec
# Replace NAs with expected season
df['season_flood'][is.na(df['season_flood'])] <- 'Nov/Dec'

# Re-factor Season codes so Jul/Aug is used as the reference level (flooding season)
df$season_DD <- factor(df$season_DD, levels=c("Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb", "Mar/Apr","May/Jun"))
df$season_flood <- factor(df$season_flood, levels=c("Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb", "Mar/Apr","May/Jun"))

####
# MERGE (get OBJCTID_1 for spatial effects)
cluster_shpDF@data <- as.data.frame(dplyr::left_join(cluster_shpDF@data, df, by = c('c_code')))

# Get centroids in KM
pts <- gCentroid(cluster_shpDF, byid=TRUE)@coords
Lockm <- latlong2grid(pts)
Loc <- cbind(Lockm$x, Lockm$y)

#### Get LOC for every wcode at everytime
x <- as.data.frame(Loc)
y <- unique(df[c("c_code")])
x$c_code <- y$c_code
# Get df w/ wcodes - no duplicates
# y <- df %>% select(wcode, c_code) #%>% distinct(.keep_all = TRUE)
z <- as.data.frame(dplyr::left_join(df, x, by = c('c_code')))
# Convert to numeric for modelling
Loc <- cbind(z$V1, z$V2)

# Set generic priors
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

#### FUNCTIONS

# Function to read ME results in table
getME_res <- function(model){
  
  # Get p-values
  CI <- confint(model) # get CI
  gme_res <- data.frame(coef(summary(model)))
  gme_res$Lower_CI <- CI[3:(length(CI)/2),1]
  gme_res$Higher_CI <- CI[3:(length(CI)/2),2]
  gme_res$p.z <- round(2 * (1 - pnorm(abs(gme_res$t.value))), 6)
  gme_res <- gme_res %>% dplyr::select(Estimate, Std..Error,Lower_CI,Higher_CI,p.z)
  # Plot
  gme_plot = tibble::rownames_to_column(gme_res, "variables") # create column for intercepts
  gme_plot$index <- 1:nrow(gme_plot) # set index
  names(gme_plot) <- c("Variables","Mean","SD","Lower_CI","Upper_CI", 'P-Value',"Index")
  
  return(gme_plot)
  
}

# Function to read R-INLA results in table
getINLA_res <- function(model) {
  
  # Function to Format INLA results in table
  result <- rbind(cbind(summary(model)$fixed[,-7])) #, cbind(summary(model)$hyperpar)
  result <- tibble::rownames_to_column(data.frame(result), "variables") # create column for intercepts
  result$index <- 1:nrow(result) # set index
  # Get posterior distribution & Calculate MAP
  # https://easystats.github.io/bayestestR/reference/p_map.html
  names <- model$names.fixed
  posterior = data.frame()
  for (i in names) {
    x <- data.frame(model$marginals.fixed[i])[,1]
    x <- cbind(i, p_map(x)[1])
    posterior <- rbind(posterior, x)
  }
  colnames(posterior) <- c('variables', 'MAP P')
  result <- as.data.frame(dplyr::left_join(as.data.frame(result), posterior, by = c('variables')))
  
  # Reorganise dataframe
  colnames(result) <- c("Variables", "Mean","SD", "Lower_CI", "median", "Upper_CI", "mode", "Index", 'MAP_P') #, "Z_score", "P_Value"
  result <- as.data.frame(result) %>% 
    dplyr::select(Index, Variables, Mean, SD, Lower_CI, Upper_CI, MAP_P) %>% #  Z_score, P_Value
    mutate_if(is.numeric, round, 5)
  
  return(result)
}

# Function to plot R-INLA results in forest plot
plotResults <- function(res) {
  
  result_name <- deparse(substitute(res))
  
  # Plot Results
  ggplot(data=res, aes(y=Index, x=Mean, xmin=Lower_CI, xmax=Upper_CI)) +
    geom_point() + 
    geom_errorbarh(height=.3) +
    scale_y_continuous(breaks=1:nrow(res), labels=res$Variables) +
    labs(title=paste('Effect Size by Variable - ', result_name), x='Effect Size', y = 'Variable') +
    geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
    theme_minimal()
  
}

# Function to compare R-INLA models in table
selINLA_mod <- function(models){
  
  res <- data.frame() 
  
  for (mod in models) {
    # Get model name
    m <- get(mod)
    # print(deparse(substitute(m)))
    # Add row to data frame w/ matching col names
    x <- as.data.frame(cbind(summary(m)$mlik[2], summary(m)$dic$dic, summary(m)$waic$waic)) #sum(log(m$cpo$cpo))
    colnames(x) = c('MLIK', 'DIC', 'WAIC') # 'CPO'
    res <- rbind(res, x)
  }
  row.names(res) <- models
  res <- res[order(res$DIC), ]
  
  return(res)
  
}



#### 2. Create mesh ####

# What are the distances between the points?
D <- dist(Loc[!duplicated(Loc), ])

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

mesh <- inla.mesh.2d(Loc, 
                     max.edge = c(1.7, 3), 
                     cutoff   = 0) # THIS ONE WAS MAKING IT WEIRD!!
mesh$n # want within 700-800 vertices
par(mar = c(0, 0, 0, 0))
plot(mesh, asp = 1, main = "")
points(Loc, col = 2, pch = 16, cex = 1)
# lines(Loc, col = 3, with = 2)

#### 3. Create Spatio-temporal SPDE, A & Index ####

# spde needed for indexing mesh (cannot use n.mesh directly anymore)
cluster.spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
names(cluster.spde)
cluster.spde$n.spde # will be the same as mesh$n

#### KEY: GROUP A & INDEX BY TIME
# https://www.sciencedirect.com/science/article/pii/S1877584513000336#s0025
# - Should we do A/stack Prediction too? 

# A Estimation
A.est <- inla.spde.make.A(mesh=mesh, 
                          loc=as.matrix(Loc), 
                          group=df$season_id,
                          n.group=24) #MAKE SURE that the points in mesh match wcode x time #
dim(A.est)  #check 

# Index by spde & group by time
s.index <- inla.spde.make.index(name = "spatial.field",
                                n.spde = cluster.spde$n.spde,
                                n.group = 24)
str(s.index)


#### 4. Create data structure ####

# Make the X matrix
Xm <- model.matrix( ~ Flood_1Lag_cub+treatment+wealth2_BL+wcode+season_DD+Flood_1Lag_cub:season_flood+season_id,
                    data = cluster_shpDF@data)

# Define sample size
N <- nrow(cluster_shpDF@data)

# This is the X data matrix
X <- data.frame(Flood_1Lag_cub = Xm[,2],
                treatment= Xm[,3],
                wealth2_BL= Xm[,4],
                wcode= Xm[,5],
                sep.oct = Xm[,6],
                nov.dec = Xm[,7],
                jan.feb = Xm[,8],
                mar.apr = Xm[,9],
                may.jun = Xm[,10],
                FLOODsep.oct= Xm[,11],
                FLOODnov.dec= Xm[,12],
                FLOODjan.feb= Xm[,13],
                FLOODmar.apr= Xm[,14],
                FLOODmay.jun= Xm[,15],
                season_id = Xm[,16])

# DATA STACK (ESTIMATION)
# https://groups.google.com/g/r-inla-discussion-group/c/zTsvjsFGu_Y
stack.est <- inla.stack(data=list(y=cluster_shpDF@data$dd10r_score), #select outcome (?)
                        A=list(A.est,1),
                        effects = list(c(s.index, list(Intercept = 1)),
                                       list(X)), # MAKE SURE the data matrix is fit correctly
                        # effects = list(s.index, intercept = rep(1, length(df$season_id))), # This is where the mismatching occurred
                        tag="est")


#### 5. Fit model ####

h.spec <- list(rho = list(prior = 'pc.cor1', param = c(0, 0.9)))

# SPECIFY FORMULA
# https://becarioprecario.bitbucket.io/spde-gitbook/ch-spacetime.html
form <- y ~ Flood_1Lag_cub + treatment + wealth2_BL + sep.oct + 
            nov.dec + jan.feb+ mar.apr + may.jun + ## DD by seasons
            FLOODsep.oct+FLOODnov.dec+FLOODjan.feb+FLOODmar.apr+FLOODmay.jun + ## DD by flood season * flooding
            f(wcode, model = 'iid') +
            f(spatial.field, model = cluster.spde, group = spatial.field.group, control.group = list(model = 'ar1', hyper = h.spec))

# Fit the model
m1 <- inla(form,  data = inla.stack.data(stack.est), 
            control.predictor = list(compute = TRUE,
                                     A = inla.stack.A(stack.est)), 
            control.family = list(hyper = list(prec = prec.prior)), 
           control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
           control.fixed = list(expand.factor.strategy = 'inla'))

#Summary of results
summary(m1)
Res <- getINLA_res(m1)
plotResults(Res)
Res$ODDS <- exp(Res$Mean)
Res

##################################################

# Fit model
m1 <- inla(form, data = inla.stack.data(stack.est, spde = cluster.spde),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(stack.est), compute = TRUE),
           control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
           



#### 0. Format data ####
# To run this code, set the work directory to folder containing the provided files & data
setwd('C:/Users/offne/Documents/GitHub/FAARM_Analysis/Data/')

# Load packages
# install.packages('MuMIn')
library(INLA)
library(mice)
library(psych)
library(oddsratio)
library(aod)
library(reshape)
library(nlme)
library(MuMIn)
library(ggplot2)
library(lme4)
library(lmtest)
library(xts)
library(tsbox)
library(zoo)
library(rgdal)
library(spdep)
library(lubridate)
library(tidyr)
library(dplyr)
library(bayestestR)

# Ensure that RINLA can handle big datasets
inla.setOption("num.threads", 4)

# Load FAARM data
df <- read.csv(file='Cluster100_10mflood_diet_df.csv', fileEncoding='UTF-8-BOM')

# Load shape data
cluster_shpDF <- st_read(dsn="C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/C. FAARM/1. Data/2. Geospatial Data/96_Cluster_final.shp")
cluster_shpDF <- st_transform(cluster_shpDF, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") # set projection
cluster_shpDF <- cluster_shpDF %>% select( -c(OBJECTID, Shape_Leng, Shape_Le_1, AREA_M)) %>% dplyr::rename(c_code = cluster_co)

# Select relevant variables
df <- df %>% select(c_code, wcode, season, season_DD, year, month, year_month, year_season, 
                    treatment, perc_flooded, Flood_1Lag,
                    dd10r_score, dd10r_min,
                    age_3_BL, g_2h_BL, fam_type_BL, dep_ratio,  dd10r_score_EL,
                    dd10r_score_BL, dd10r_min_BL, g_2h_BL, fam_type_BL, 
                    wi_hl_BL, wi_al_BL, wi_land_BL, num_crops_BL, hfias_BL,
                    woman_edu_cat__BL, mobility_BL, support_BL, 
                    communication_BL, decision_BL, pb_621_BL,know_score_BL,
                    dep_ratio, md_score_BL, wealth_BL, dec_BL, quint_BL, 
                    terc_BL, wealth2_BL, dec2_BL, quint2_BL, terc2_BL)


### NOTE: Removing 2015 for now to keep season_DDs consistent 
df <-subset(df, year != 2015)

# Round outcome variable
df <- df %>% dplyr::mutate_at(vars(dd10r_min), funs(round(., 0))) #!!!!!!!!!!!!

# Cubic transformation for flooding (based on initial exploratory analysis)
df$perc_flooded_cub <- df$perc_flooded^(1/3)
df$Flood_1Lag_cub <- df$Flood_1Lag^(1/3)

# Merge to get OBJCTID_1 for spatial effects
df <- as.data.frame(dplyr::left_join(df, cluster_shpDF, by = c('c_code')))

# Create numeric year-season_DD id for temporal effects
df$season_id <- as.numeric(factor(df$year_season))

# Get spatial weight matrices
adj.mat <- poly2nb(cluster_shpDF)
W.adj.mat <- nb2mat(adj.mat, style = "B", zero.policy=T) 

# Set generic priors
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

# Add lag for season codes (improve interpretability)
df <- df %>%
  group_by(wcode) %>%
  dplyr::mutate(season_flood = dplyr::lag(season_DD, n = 1, default = NA)) %>% # general time  lag of 6 by admin_code
  as.data.frame()

# Re-factor Season codes so Jul/Aug is used as the reference level (flooding season)
df$season_DD <- factor(df$season_DD, levels=c("Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb", "Mar/Apr","May/Jun"))
df$season_flood <- factor(df$season_flood, levels=c("Jul/Aug", "Sept/Oct", "Nov/Dec", "Jan/Feb", "Mar/Apr","May/Jun"))
levels(df$season_DD)

#### Check crude ####

mod2 <- inla(dd10r_score ~ Flood_1Lag_cub+treatment+wealth2_BL+season_DD+season_flood:Flood_1Lag_cub+
               f(wcode, model = 'iid')+
               f(OBJECTID_1, model = "besagproper", graph = W.adj.mat,
                 group = season_id, control.group = list(model = "ar1"),
                 hyper = prec.prior),
             family ='gaussian', data = df,
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE))
Res2 <- getINLA_res(mod2)
plotResults(Res2)
Res2$ODDS <- exp(Res2$Mean)
Res2


summary(m1) #-34555.40 
summary(mod2) #-34634.28 
comp <- selINLA_mod(list('m1', 'mod2'))
comp


