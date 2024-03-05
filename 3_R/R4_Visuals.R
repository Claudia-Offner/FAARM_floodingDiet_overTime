# EXTRACT VISUALS

#### Detatch packages & clear environment/plots
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
# rm(list = ls())

#### IMPORTANT - set file path to data folder location
path <- 'C:/Users/ClaudiaOffner/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/- DD-Flooding Interaction - CO/4. Data/REPORTING'
setwd(path)
# Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)

# 0. Packages & Functions ######
# Define functions. Source: https://github.com/jkeirstead/r-slopegraph

##### LINKS
# https://shouldbewriting.netlify.app/posts/2020-04-13-estimating-and-testing-glms-with-emmeans/#fnref2
# https://stats.oarc.ucla.edu/r/dae/logit-regression/
# https://stackoverflow.com/questions/20060949/ggplot2-multiple-sub-groups-of-a-bar-chart
# https://www.khstats.com/blog/forest-plots/

# PACKAGES
library(xlsx)
library(dplyr)
library(ggplot2)
library (patchwork)
library(tidyverse)
library(gt)
theme_set(theme_classic())

# Set seasons
seasons <- c("Jan/Feb", "Mar/Apr", "May/Jun", "Jul/Aug", "Sep/Oct", "Nov/Dec")


# FUNCTIONS
# Round all numeric values in a df
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
# Extract legend from ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
# Formatting for MF2
mf2_plot_format <- function(df){
  # wrangle results into pre-plotting table form
  res_plot <- df |>
    # round estimates and 95% CIs to 2 decimal places for journal specifications
    mutate(across(
      c(Diff, Lower.CI, Upper.CI),
      ~ str_pad(
        round(.x, 2),
        width = 4,
        pad = "0",
        side = "right"
      )
    ),
    # add an "-" between HR estimate confidence intervals
    estimate_lab = paste0(Diff, " (", Lower.CI, "-", Upper.CI, ")")) |>
    # round p-values to two decimal places, except in cases where p < .001
    mutate(P = case_when(
      P < .001 ~ "<0.001",
      round(P, 2) == .05 ~ as.character(round(P,3)),
      P < .01 ~ str_pad( # if less than .01, go one more decimal place
        as.character(round(P, 2)),
        width = 4,
        pad = "0",
        side = "right"
      ),
      TRUE ~ str_pad( # otherwise just round to 2 decimal places and pad string so that .2 reads as 0.20
        as.character(round(P, 2)),
        width = 4,
        pad = "0",
        side = "right"
      )
    )) |>
    # add a row of data that are actually column names which will be shown on the plot in the next step
    bind_rows(
      data.frame(
        Group = "Strata",
        estimate_lab = "Probability Change (95% CI)",
        Lower.CI  = "",
        Upper.CI = "",
        P = "p-value",
        Index = 22
      )
    ) |>
    mutate(Group = fct_rev(fct_relevel(Group, "Group")))
  
  return(res_plot)
  
}
# Tufte formater for mf3 & mf4
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
# Absolute difference plotter (MF3 & MF4)
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

# # SF3: Box plots ####
# 
# hist(df$dd10r_score_m, col="#ff5050", )
# hist(df$Flood_1Lag_norm, col="#6699FF", )
# 
# # Overall 
# mean(df$Flood_1Lag_norm)
# sd(df$Flood_1Lag_norm)
# (range(df$Flood_1Lag_norm))
# 
# # Season & Year
# (season_means <- df %>% group_by(season_flood) 
#   %>% summarize(
#     flood_mean = mean(Flood_1Lag_norm, na.rm = TRUE),
#     flood_min = min(Flood_1Lag_norm, na.rm = TRUE),
#     flood_max = max(Flood_1Lag_norm, na.rm = TRUE),
#     flood_sd = sd(Flood_1Lag_norm, na.rm = TRUE)))
# (year_means <- df %>% group_by(year) 
#   %>% summarize(
#     flood_mean = mean(Flood_1Lag_norm, na.rm = TRUE),
#     flood_min = min(Flood_1Lag_norm, na.rm = TRUE),
#     flood_max = max(Flood_1Lag_norm, na.rm = TRUE),
#     flood_sd = sd(Flood_1Lag_norm, na.rm = TRUE)))
# (treat_means <- df %>% group_by(treatment) 
#   %>% summarize(
#     flood_mean = mean(Flood_1Lag_norm, na.rm = TRUE),
#     flood_min = min(Flood_1Lag_norm, na.rm = TRUE),
#     flood_max = max(Flood_1Lag_norm, na.rm = TRUE),
#     flood_sd = sd(Flood_1Lag_norm, na.rm = TRUE)))
# 
# # Box plots by group
# ggplot(df,aes(x=season_flood,y=Flood_1Lag_norm))+
#   geom_boxplot(fill="#6699FF",outlier.color="black")+
#   labs(x = "Season", y = "Flooding (Cluster %)", title = "Central Tendancy of Flooding by Season")
# 
# df$treat2 <- ifelse(df$treatment == 0, "Control", "HFP")
# ggplot(df,aes(x=treat2,y=Flood_1Lag_norm))+
#   geom_boxplot(fill="#ff5050",outlier.color="black")+
#   labs(x = "Intervention", y = "Flooding (Cluster %)", title = "Central Tendancy of Flooding by Intervention")
# 
# 
# 
# 
# 
# 
# 
# MF2: Relative differences in probability of different strata of season & trial-arms for each DD outcome (1% flood) ####

# NEED: Group the y-axes by Model & add segment lines accordingly
#       Fix the text alignment & Title/axes names

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Rel-Diff')
dd_outcomes <- unique(source_f0$Outcome)

for(d in dd_outcomes) {
  
  # Select data
  source_f1 <- source_f0 %>% filter(Outcome == d) 
  source_f1$Index <- 1:nrow(source_f1)
  source_f1$Index <- source_f1$Index[rev(1:length(source_f1$Index))]
  
  # Format data for left & right side of plots
  res_plot <- mf2_plot_format(source_f1)
  # Set color scheme
  link_colour <- c("#F8766D", "#7CAE00", "#619CFF","#C77CFF")
  source_f1$mod <- link_colour[1]
  source_f1$mod[source_f1$Model == 'F2'] <- link_colour[2]
  source_f1$mod[source_f1$Model == 'F3'] <- link_colour[3]
  source_f1$mod[source_f1$Model == 'F4'] <- link_colour[4]
  
  # Split up the models
  f1 <- source_f1 %>% filter(Model == 'F1') 
  f2 <- source_f1 %>% filter(Model == 'F2') 
  f3 <- source_f1 %>% filter(Model == 'F3') 
  f4 <- source_f1 %>% filter(Model == 'F4') 
  
  # Conditional names for x-axes
  if(d=='WDDS') {
    x_ax <- 'Changes in mean'
    x_lim <- c(-0.3, .25)
  } else {
    x_ax <- 'Changes in probability'
    x_lim <- c(-0.12, .12)
  }
  
  # Get legend
  (all_plot <- 
      source_f1 |>
      # Make plot
      ggplot(aes(y = Index)) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Model), shape=15, size=3) +
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Model)) +
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(0.5, length(source_f1$Group)+1, by = 1), color="gray", size=.5, alpha=.5) +
      # Add labels & set color
      labs(x=x_ax, y="", color="") + 
      scale_y_continuous(breaks=1:nrow(source_f1), labels=rev(source_f1$Group)) +
      scale_color_manual(values = link_colour) +  
      # Format
      coord_cartesian(ylim=c(1,22), xlim=x_lim) + # Zoom out
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.line.y = element_blank(), # Remove y axes
            axis.ticks.y= element_blank(),
            legend.position="bottom")) 
  legend <- get_legend(all_plot)
  
  # Create forest plots
  (plot_f4 <- 
      f4 |>
      # Make plot
      ggplot(aes(y = Index)) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Model), shape=15, size=3) +
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Model)) +
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(0.5, length(f4$Group)+1, by = 1), color="gray", size=.5, alpha=.5) +
      # Add labels & set color
      labs(x=x_ax, y="") + 
      scale_y_continuous(breaks=1:nrow(f4), labels=rev(f4$Group)) +
      scale_color_manual(values = f4$mod) +  
      # Format
      coord_cartesian(ylim=c(1,12), xlim=x_lim) + # Zoom out
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.line.y = element_blank(), # Remove y axes
            axis.ticks.y= element_blank(),
            legend.position="none")) 
  
  (plot_f3 <- 
      f3 |>
      # Make plot
      ggplot(aes(y = seq(1, 6))) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Model), shape=15, size=3) +
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Model)) +
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(1.5, length(f3$Group)+1, by = 1), color="gray", size=.5, alpha=.5) +
      # Add labels & set color
      labs(x="", y="") + 
      scale_y_continuous(breaks=1:nrow(f3), labels=rev(f3$Group)) +
      scale_color_manual(values = f3$mod) +  
      # Format
      coord_cartesian(ylim=c(0.5, 7), xlim=x_lim) + # Zoom out
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.line.y = element_blank(), # Remove y axes
            axis.ticks.y= element_blank(),
            axis.line.x = element_line(color = "gray"),
            axis.ticks.x=element_blank(),
            axis.text.x= element_blank(),
            legend.position="none")) 
  
  (plot_f2 <- 
      f2 |>
      # Make plot
      ggplot(aes(y = seq(1, 2))) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Model), shape=15, size=3) +
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Model)) +
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(1.5, length(f2$Group)+1, by = 1), color="gray", size=.5, alpha=.5) +
      # Add labels & set color
      labs(x="", y="") + 
      scale_y_continuous(breaks=1:nrow(f2), labels=rev(f2$Group)) +
      scale_color_manual(values = f2$mod) +  
      # Format
      coord_cartesian(ylim=c(0.5, 3), xlim=x_lim) + # Zoom out
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.line.y = element_blank(), # Remove y axes
            axis.ticks.y= element_blank(),
            axis.line.x = element_line(color = "gray"),
            axis.ticks.x=element_blank(),
            axis.text.x= element_blank(),
            legend.position="none"))
  
  (plot_f1 <- 
      f1 |>
      # Make plot
      ggplot(aes(y = seq(1))) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Model), shape=15, size=3) +
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Model)) +
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(1.5, length(f1$Group)+1, by = 1), color="gray", size=.5, alpha=.5) +
      # Add labels & set color
      labs(x="", y="") + 
      scale_y_continuous(breaks=1:nrow(f1), labels=rev(f1$Group)) +
      scale_color_manual(values = f1$mod) +  
      # Format
      coord_cartesian(ylim=c(0.5, 2), xlim=x_lim) + # Zoom out
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.line.y = element_blank(), # Remove y axes
            axis.ticks.y= element_blank(),
            axis.line.x = element_line(color = "gray"),
            axis.ticks.x=element_blank(),
            axis.text.x= element_blank(),
            legend.position="none"))

  # Put plots together
  layout <- c(
    area(t = 0, l = 0, b = 2, r = 15), # F1
    area(t = 3, l = 0, b = 5, r = 15), # F2
    area(t = 6, l = 0, b = 12, r = 15), # F3
    area(t = 13, l = 0, b = 25, r = 15), # F4
    area(t = 26, l = 5, b = 26, r = 10) # Legend

  )
  
  # Conditional names for plot title
  if(d=='MDD') {
    name <- 'Minimum Dietary Diversity'
  } else if (d=='WDDS') {
    name <- 'Dietary Diversity Score'
  } else {
    name <- paste0('Food Group: ', d)
  }
  if(d=='Nuts/seeds'){
    d <- 'Nuts & seeds'
  }
  
  # Final plot arrangement
  (res <- plot_f1 + plot_f2 + plot_f3 + plot_f4 + legend 
    + plot_layout(design = layout) 
    + plot_annotation(name, theme = theme(plot.title = element_text(size = 16, hjust = 0.62))))

  ggsave(paste0('MF2_RelativeDiff/', d, "_mf2.png"), res, width=18, height=16, units='cm')
  
}


# MF3: Absolute differences in probability of DD outcomes for different % flooding for each season (both trial-arms) ####

# NEED: Add dietary diversity variables at top, with specific colours (*astrix for WDDS)
#       AND set significance by dotted line
#       AND loop over each season
#       AND extract each plot individually AND as a 2x6 plot grid

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Abs-Flood_Levels')

for(s in seasons) {
  
  link_colour <- c('darkgreen', 'grey')
  # s <- 'Nov/Dec'
  # FOOD GROUP PLOT
  source_f1 <- source_f0 %>% filter(season == s) %>% filter(!(group %in% c("Minimum dietary diversity", "Dietary diversity scores*")))
  # Prepare data
  f1 <- tufte_sort(source_f1, x="increase", y="value", group="group", method="tufte", min.space=0.05, max.space=1)
  f1 <- transform(f1, x=factor(x, levels=c(0, 1, 5, 10), labels=c("0%","1%","5%","10%")), y=round(y, 2))
  # Add sig back in
  source_f1 <- source_f1[order(source_f1$group, source_f1$increase), ]
  f1 <- f1[order(f1$group, f1$x), ]
  f1$sig <- source_f1$sig
  # Get axes colors
  f1$a <- ifelse(f1$sig == 'p>0.05', link_colour[2], link_colour[1])
  f1_unique <- f1[!duplicated(f1$group), ]
  axes_colours <- f1_unique$a[order(f1_unique$group)] # ggplot interprets by name
  
  # Conditional formatting for Nov/Dec
  if(s=='Nov/Dec'){
    link_colour <- rev(link_colour)
  }
  
  # Plot
  (gg_fg <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
      geom_line(aes(group=group, colour=sig)) +
      scale_color_manual(values = link_colour) +  # Set line colors manually
      labs(color = "95% Confidence") +  # Modify legend label
      xlab("Increase in flooding") +
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) +
      geom_point(colour="white",size=8) +
      geom_text(aes(label=y, colour=sig), size=3, family="Helvetica", show.legend = FALSE) +
      scale_y_continuous(name="", breaks=subset(f1, x==head(x,1))$ypos, labels=subset(f1, x==head(x,1))$group) + 
      theme(axis.ticks = element_blank(),
            plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
            axis.text = element_text(family = "Helvetica", face="bold"),
            axis.text.y = element_text(color = axes_colours),
            plot.margin=unit(c(0.5,2.5,0.5,0), 'cm'),
            legend.position="bottom")
    )
  
  # MDD PLOT
  source_f2 <- source_f0 %>% filter(season == s) %>% filter(group =="Minimum dietary diversity")
  # Prepare data
  f2 <- source_f2 %>%
    rename(x = increase, y = value) %>%
    mutate(yshift=0.000, ypos=yshift+y) %>%
    select(group, yshift, x, y, ypos, sig)
  f2 <- transform(f2, x=factor(x, levels=c(0, 1, 5, 10), labels=c("0%","1%","5%","10%")), y=round(y, 2))
  # Get axes colors
  f2$a <- ifelse(f2$sig == 'p>0.05', link_colour[2], link_colour[1])
  axes_colours <- unique(f2$a)
  # Conditional formatting for Nov/Dec
  if(s=='Nov/Dec'){
    link_colour <- rev(link_colour)
  }
  # Plot
  (gg_mdd <- ggplot(f2,aes(x=x,y=ypos, colour=sig)) +
    geom_line(aes(group=group, colour=sig)) +
    scale_color_manual(values = f2$a)+  # Set line colors manually
    geom_point(colour="white",size=8) +
    geom_text(aes(label=y, colour=sig), size=3, family="Helvetica", show.legend = FALSE) +
    scale_y_continuous(name="", breaks=subset(f2, x==head(x,1))$ypos, labels=subset(f2, x==head(x,1))$group) + 
    coord_cartesian(ylim=c(min(f2$y)-0.1, max(f2$y)+0.1)) + # Zoom out
    theme(axis.ticks = element_blank(),
          plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
          axis.text.y = element_text(color = f2$a, family = "Helvetica", face="bold"),
          axis.line.x = element_blank(),
          axis.text.x=element_blank(),
          axis.title.x= element_blank(),
          plot.margin=unit(c(0.5,2.5,0.5,0), 'cm'),
          legend.position="none")) 
  
  # WDDS PLOT
  source_f3 <- source_f0 %>% filter(season == s) %>% filter(group =="Dietary diversity scores*")
  # Prepare data
  f3 <- source_f3 %>%
    rename(x = increase, y = value) %>%
    mutate(yshift=0.000, ypos=yshift+y) %>%
    select(group, yshift, x, y, ypos, sig)
  f3 <- transform(f3, x=factor(x, levels=c(0, 1, 5, 10), labels=c("0%","1%","5%","10%")), y=round(y, 2))
  # Get axes colors
  f3$a <- ifelse(f3$sig == 'p>0.05', link_colour[2], link_colour[1])
  axes_colours <- unique(f3$a)
  # Plot
  (gg_wdds <- ggplot(f3,aes(x=x,y=ypos, colour=sig)) +
      geom_line(aes(group=group, colour=sig)) +
      scale_color_manual(values = f3$a)+  # Set line colors manually
      geom_point(colour="white",size=8) +
      geom_text(aes(label=y, colour=sig), size=3, family="Helvetica", show.legend = FALSE) +
      scale_y_continuous(name="", breaks=subset(f3, x==head(x,1))$ypos, labels=subset(f3, x==head(x,1))$group) + 
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) +
      coord_cartesian(ylim=c(min(f3$y)-0.1, max(f3$y)+0.1)) + # Zoom out
      theme(axis.ticks = element_blank(),
            plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
            axis.text.y = element_text(color = f3$a, family = "Helvetica", face="bold"),
            axis.line.x = element_blank(),
            axis.text.x=element_blank(),
            axis.title.x= element_blank(),
            plot.margin=unit(c(0.5,2.5,0.5,0), 'cm'),
            legend.position="none")) 

  # Put plots together
  layout <- c(
    area(t = 0, l = 0, b = 3, r = 15), # WDDS
    area(t = 4, l = 0, b = 6, r = 15), # MDD
    area(t = 7, l = 0, b = 30, r = 15) # Food groups
  )

  # Final plot arrangement
  (res <- gg_wdds + gg_mdd + gg_fg  
    + plot_layout(design = layout) 
    + plot_annotation(s, theme = theme(plot.title = element_text(size = 16, hjust = 0.62))))
  
  
  # Export image
  ggsave(paste0('MF3_AbsDiff - FloodLevels/', str_replace(s, "/", "-"), "_mf3.png"), 
         res, width=15, height=20, units='cm')
  
}


# MF4: Absolute differences in probability of DD outcomes for different trial-arms for each season (1% flood) ####

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Abs-Treat_Levels')

for(s in seasons) {
  ## Prepare data    
  source_f1 <- source_f0 %>% filter(season == s) %>% filter(!(group %in% c("Dietary diversity scores*")))
  f1 <- tufte_sort(source_f1, x="increase", y="value", group="group", method="tufte", min.space=0.05, max.space=0.15)
  f1 <- transform(f1, x=factor(x, levels=c(0, 1), labels=c("Control","HFP")), y=round(y, 2))
  
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
  (gg_mf4 <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
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
      labs(title=paste0("Predicted probabilities of achieving subsequent dietary outcomes \nwith 1% increase in flooding in ", s, " for each trial-arm")) +
      xlab("Trial-arm"))
  # Export image
  ggsave(paste0('MF4_AbsDiff - Trial-arm/', str_replace(s, "/", "-"), "_mf4.png"), gg_mf4, width=15, height=15, units='cm')
  
}




########

# Add text
# annotate("text", x = -.08, y = 22, label = "Flooding harmful") +
# annotate("text", x = .08, y = 22, label = "Flooding protective") + 

# Change legend position
# theme(legend.position = c(0.9, 0.8)) +


# (legend <- get_legend(p_mid))
# (p_mid <- p_mid + theme(legend.position="none"))
#
# # Left side of plot - strata names & relative diff (CIs)
# (p_left <-
#     res_plot  |>
#     # Order model on y-axis
#     ggplot(aes(y = Index)) +
#     # Add text for strata column
#     geom_text(aes(x = 0, label = Group), hjust = 0, fontface = "bold") +
#     # Add text for diff (CI)
#     # geom_text(aes(x = 1, label = estimate_lab), hjust = 0, fontface = ifelse(res_plot$estimate_lab == "Probability Change (95% CI)", "bold", "plain")) +
#     # Remove background and format
#     theme_void()) #+
#     # coord_cartesian(xlim = c(0, 2.5)))

# # Right side of plot - pvalues
# (p_right <-
#     res_plot  |>
#     # Order model on y-axis
#     ggplot(aes(y = Index)) +
#     # Add text for strata column
#     geom_text(aes(x = 0, y = Index, label = P), hjust = 0, fontface = ifelse(res_plot$P == "p-value", "bold", "plain")) +
#     # Remove background and format
#     theme_void())
# 
# # Put plots together
# layout <- c(
#   area(t = 0, l = 0, b = 25, r = 4), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
#   area(t = 1, l = 5, b = 25, r = 9) # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
#   # area(t = 0, l = 9, b = 25, r = 11), # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
#   # area(t = 0, l = 11, b = 25, r = 11) # Legend
# )
# # Final plot arrangement
# res <- p_left + p_mid + plot_layout(design = layout) + plot_annotation(paste0('Dietary Outcome: ', d), theme = theme(plot.title = element_text(size = 16)))




# Set significance col (for plotting)
# source_f1$Importance <- '0_None'
# source_f1$Importance[source_f1$P <= 0.10 & source_f1$P >= 0.05] <- '1_Weak'
# source_f1$Importance[source_f1$P <= 0.05] <- '2_Strong'
# cols <- c("0_None" = "#dadada","1_Weak" = "#ff9530","2_Strong" = "#029921")

# source_f1 <- round_df(source_f1, 3)

# Plot Results
# ggplot(data=source_f1, aes(y=Index, x=Diff, xmin=Lower.CI, xmax=Upper.CI)) +
#   geom_point() + # aes(colour=Model)
#   # geom_text(aes(label = Diff, colour = Importance),
#             # size = 3.5, nudge_x = 0.1, nudge_y = 0, check_overlap = FALSE) +
#   scale_colour_manual(values = cols) +
#   theme(legend.position = "bottom") +
#   geom_errorbarh(height=.3) +
#   scale_y_continuous(breaks=1:nrow(source_f1), labels=source_f1$Group) +
#   labs(title=paste('Effect Size by Variable - ', outcome), x='Marginal Effects', y = 'Strata') +
#   geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
#   # geom_hline(yintercept = seq(0.5, length(source_f1$Model), by = 1), color="gray", size=.5, alpha=.5) +# set horizontal lines between x groups
#   theme_classic()
# 

