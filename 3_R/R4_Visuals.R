# EXTRACT VISUALS

#### Detatch packages & clear environment/plots
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
# rm(list = ls())

#### IMPORTANT - set file path to data folder location
path <- 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/2. Manuscripts'
setwd(path)
# Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)

##### LINKS
# https://shouldbewriting.netlify.app/posts/2020-04-13-estimating-and-testing-glms-with-emmeans/#fnref2
# https://stats.oarc.ucla.edu/r/dae/logit-regression/

# 0. Packages & Functions ######
# Define functions. Source: https://github.com/jkeirstead/r-slopegraph
library(xlsx)
library(dplyr)
theme_set(theme_classic())

# Figure 1
tufte_sort <- function(df, x="year", y="value", group="group", method="tufte", min.space=0.05, max.space=0.1) {
  ## First rename the columns for consistency
  ids <- match(c(x, y, group), names(df))
  df <- df[,ids]
  names(df) <- c("x", "y", "group")
  
  ## Expand grid to ensure every combination has a defined value
  tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
  tmp <- merge(df, tmp, all.y=TRUE)
  df <- mutate(tmp, y=ifelse(is.na(y), 0, y))
  
  ## Cast into a matrix shape and arrange by first column
  require(reshape2)
  tmp <- dcast(df, group ~ x, value.var="y")
  ord <- order(tmp[,2])
  tmp <- tmp[ord,]
  
  min.space <- min.space*diff(range(tmp[,-1]))
  max.space <- max.space*diff(range(tmp[,-1]))
  yshift <- numeric(nrow(tmp))
  ## Start at "bottom" row
  ## Repeat for rest of the rows until you hit the top
  for (i in 2:nrow(tmp)) {
    ## Shift subsequent row up by equal space so gap between
    ## two entries is >= minimum
    mat <- as.matrix(tmp[(i-1):i, -1])
    d.min <- min(diff(mat))
    d.max <- max(diff(mat))
    # yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
    if (d.min < min.space) {
      yshift[i] <- min.space - d.min
    } else if (d.max > max.space) {
      yshift[i] <- max.space - d.max
    } else {
      yshift[i] <- 0
    }
  }
  
  
  tmp <- cbind(tmp, yshift=cumsum(yshift))
  
  scale <- 1
  tmp <- melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
  ## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y
  
  tmp <- transform(tmp, ypos=y + scale*yshift)
  return(tmp)
  
}
# Figure 1
plot_absolute <- function(df, link_colours=c('blue', 'red'), col) {
  ylabs <- subset(df, x==head(x,1))$group
  yvals <- subset(df, x==head(x,1))$ypos
  fontSize <- 3
  gg <- ggplot(df,aes(x=x,y=ypos, colour=sig)) +
    geom_line(aes(group=group, colour=sig)) +
    scale_color_manual(values = link_colours)+  # Set line colors manually
    labs(color = "Significant") +  # Modify legend label
    geom_point(colour="white",size=8) +
    geom_text(aes(label=y, colour=sig), size=fontSize, family="American Typewriter", show.legend = FALSE) +
    scale_y_continuous(name="", breaks=yvals, labels=ylabs) + 
    theme(axis.ticks = element_blank(),
          plot.title = element_text(hjust=0.5, family = "American Typewriter", face="bold"),
          axis.text = element_text(family = "American Typewriter", face="bold"),
          axis.text.y = element_text(color = col))
  return(gg)
}    


# SF3: Box plots ####

hist(df$dd10r_score_m, col="#ff5050", )
hist(df$Flood_1Lag_norm, col="#6699FF", )

# Overall 
mean(df$Flood_1Lag_norm)
sd(df$Flood_1Lag_norm)
(range(df$Flood_1Lag_norm))

# Season & Year
(season_means <- df %>% group_by(season_flood) 
  %>% summarize(
    flood_mean = mean(Flood_1Lag_norm, na.rm = TRUE),
    flood_min = min(Flood_1Lag_norm, na.rm = TRUE),
    flood_max = max(Flood_1Lag_norm, na.rm = TRUE),
    flood_sd = sd(Flood_1Lag_norm, na.rm = TRUE)))
(year_means <- df %>% group_by(year) 
  %>% summarize(
    flood_mean = mean(Flood_1Lag_norm, na.rm = TRUE),
    flood_min = min(Flood_1Lag_norm, na.rm = TRUE),
    flood_max = max(Flood_1Lag_norm, na.rm = TRUE),
    flood_sd = sd(Flood_1Lag_norm, na.rm = TRUE)))
(treat_means <- df %>% group_by(treatment) 
  %>% summarize(
    flood_mean = mean(Flood_1Lag_norm, na.rm = TRUE),
    flood_min = min(Flood_1Lag_norm, na.rm = TRUE),
    flood_max = max(Flood_1Lag_norm, na.rm = TRUE),
    flood_sd = sd(Flood_1Lag_norm, na.rm = TRUE)))

# Box plots by group
ggplot(df,aes(x=season_flood,y=Flood_1Lag_norm))+
  geom_boxplot(fill="#6699FF",outlier.color="black")+
  labs(x = "Season", y = "Flooding (Cluster %)", title = "Central Tendancy of Flooding by Season")

df$treat2 <- ifelse(df$treatment == 0, "Control", "HFP")
ggplot(df,aes(x=treat2,y=Flood_1Lag_norm))+
  geom_boxplot(fill="#ff5050",outlier.color="black")+
  labs(x = "Intervention", y = "Flooding (Cluster %)", title = "Central Tendancy of Flooding by Intervention")







# MF2: Relative differences in probability of different strata of season & trial-arms for each DD outcome (1% flood) ####
# https://stackoverflow.com/questions/20060949/ggplot2-multiple-sub-groups-of-a-bar-chart

# NEED: Group the y-axes by Model & add segment lines accordingly
#       Fix the text alignment & Title/axes names

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Rel-Diff')

outcome <- 'Dairy'
source_f1 <- source_f0 %>% filter(Outcome == outcome) 
source_f1$Index <- 1:nrow(source_f1)
# Set significance col (for plotting)
source_f1$Importance <- '0_None'
source_f1$Importance[source_f1$P <= 0.10 & source_f1$P >= 0.05] <- '1_Weak'
source_f1$Importance[source_f1$P <= 0.05] <- '2_Strong'
cols <- c("0_None" = "#dadada","1_Weak" = "#ff9530","2_Strong" = "#029921")
source_f1 <- round_df(source_f1, 3)

# Plot Results
ggplot(data=source_f1, aes(y=Index, x=Diff, xmin=Lower.CI, xmax=Upper.CI)) +
  geom_point() + # aes(colour=Model)
  geom_text(aes(label = Diff, colour = Importance),
            size = 3.5, nudge_x = 0.1, nudge_y = 0, check_overlap = FALSE) +
  scale_colour_manual(values = cols) +
  theme(legend.position = "bottom") +
  geom_errorbarh(height=.3) +
  scale_y_continuous(breaks=1:nrow(source_f1), labels=source_f1$Group) +
  labs(title=paste('Effect Size by Variable - ', outcome), x='Marginal Effects', y = 'Strata') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  geom_hline(yintercept = seq(0.5, length(source_f1$Model), by = 1), color="gray", size=.5, alpha=.5) +# set horizontal lines between x groups
  theme_classic()
  



# MF3: Absolute differences in probability of DD outcomes for different % flooding for each season (both trial-arms) ####

# NEED: Add dietary diversity variables at top, with specific colours (*astrix for WDDS)
#       AND set significance by dotted line
#       AND loop over each season
#       AND extract each plot individually AND as a 2x6 plot grid

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Abs-Flood_Levels')
# Filter to correct season
seasons <- 'Jan/Feb'
source_f1 <- source_f0 %>% filter(season == seasons) %>% filter(!(group %in% c("Minimum dietary diversity*", "Dietary diversity scores*")))

# Prepare data    
f1 <- tufte_sort(source_f1, x="increase", y="value", group="group", 
                 method="tufte", min.space=0.05, max.space=1)
f1 <- transform(f1, x=factor(x, levels=c(0, 1, 5,10), 
                             labels=c("0%","1%","5%","10%")), y=round(y, 2))
# Add sig back in
source_f1 <- source_f1[order(source_f1$group, source_f1$increase), ]
f1 <- f1[order(f1$group, f1$x), ]
f1$sig <- source_f1$sig
# Get axes colors
link_colour <- c('darkgreen', 'grey')
f1$a <- ifelse(f1$sig == 'p>0.05', link_colour[2], link_colour[1])
f1_unique <- f1[!duplicated(f1$group), ]
axes_colours <- f1_unique$a[order(f1_unique$group)] # ggplot interprets by name
# Plot
ylabs <- subset(f1, x==head(x,1))$group
yvals <- subset(f1, x==head(x,1))$ypos
fontSize <- 3
gg1 <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
  geom_line(aes(group=group, colour=sig)) +
  scale_color_manual(values = link_colour)+  # Set line colors manually
  labs(color = "95% Confidence") +  # Modify legend label
  geom_point(colour="white",size=8) +
  geom_text(aes(label=y, colour=sig), size=fontSize, family="Helvetica", show.legend = FALSE) +
  scale_y_continuous(name="", breaks=yvals, labels=ylabs) + 
  theme(axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
        axis.text = element_text(family = "Helvetica", face="bold"),
        axis.text.y = element_text(color = axes_colours)) + 
  labs(title=paste0("Predicted probabilities of achieving subsequent dietary outcomes \nin ", seasons, " for different % of cluster flooded")) +
  xlab("Increase in flooding")
gg1


# library(gridExtra)
# grid.arrange(plot1, plot2, plot3, nrow = 2, ncol = 2)

# MF4: Absolute differences in probability of DD outcomes for different trial-arms for each season (1% flood) ####

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Abs-Treat_Levels')

# Filter to correct season
seasons <- 'Jan/Feb'
source_f1 <- source_f0 %>% filter(season == seasons) %>% filter(!(group %in% c("Minimum dietary diversity*", "Dietary diversity scores*")))

## Prepare data    
f1 <- tufte_sort(source_f1, x="increase", y="value", group="group", 
                 method="tufte", min.space=0.05, max.space=0.15)
f1 <- transform(f1, x=factor(x, levels=c(0, 1), 
                             labels=c("Control","HFP")), y=round(y, 2))

# Add sig back in
source_f1 <- source_f1[order(source_f1$group, source_f1$increase), ]
f1 <- f1[order(f1$group, f1$x), ]
f1$sig <- source_f1$sig
# Get axes colors
link_colour <- c('darkgreen', 'grey')
f1$a <- ifelse(f1$sig == 'p>0.05', link_colour[2], link_colour[1])
f1_unique <- f1[!duplicated(f1$group), ]
axes_colours <- f1_unique$a[order(f1_unique$group)] # ggplot interprets by name
# Plot
ylabs <- subset(f1, x==head(x,1))$group
yvals <- subset(f1, x==head(x,1))$ypos
fontSize <- 3
gg1 <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
  geom_line(aes(group=group, colour=sig)) +
  scale_color_manual(values = link_colour)+  # Set line colors manually
  labs(color = "95% Confidence") +  # Modify legend label
  geom_point(colour="white",size=8) +
  geom_text(aes(label=y, colour=sig), size=fontSize, family="Helvetica", show.legend = FALSE) +
  scale_y_continuous(name="", breaks=yvals, labels=ylabs) + 
  theme(axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
        axis.text = element_text(family = "Helvetica", face="bold"),
        axis.text.y = element_text(color = axes_colours)) + 
  labs(title=paste0("Predicted probabilities of achieving subsequent dietary outcomes \nwith 1% increase in flooding in ", seasons, " for each trial-arm")) +
  xlab("Increase in flooding")
gg1



