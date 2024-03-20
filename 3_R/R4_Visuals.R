# EXTRACT VISUALS

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
library(patchwork)
library(tidyverse)
library(RColorBrewer) # color
# install.packages('randomcoloR')
theme_set(theme_classic())

# Set seasons
seasons <- c("Jan/Feb", "Mar/Apr", "May/Jun", "Jul/Aug", "Sep/Oct", "Nov/Dec")
# Set aesthetics 
custom_colors_sig <- c("p>0.05"="grey", "p<0.05"="black")
custom_lines <- c("p>0.05"="dashed", "p<0.05"="solid")
custom_shapes <- c("Overall"=16, "Control"=15 , "HFP"=17)
custom_colors <- c('Overall'="#2b2a2a",
                   'Jan/Feb'="#ce1126", 
                   'Mar/Apr'="#f2609e", 
                   'May/Jun'="#e66300", #ebac23
                   'Jul/Aug'="#80b517",
                   'Sep/Oct'="#057dcd",
                   'Nov/Dec'="#7849b8")

# FUNCTIONS

# Extract legend from ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
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

# GG plotter for MF (abs diff flood levels)
Abs_flood <- function(df, s, binary='Yes', x_labs='Yes', link_colour=c('darkgreen', 'grey')){
  
  
  if (binary=='Yes') {
    
    # FOOD GROUP PLOT
    source_f1 <- source_f0 %>% filter(season == s) %>% filter(!(group %in% c("Minimum dietary diversity", "Dietary diversity scores*")))
    # Prepare data
    f1 <- tufte_sort(source_f1, x="increase", y="value", group="group", method="tufte", min.space=0.05, max.space=1)
    f1 <- transform(f1, x=factor(x, levels=c(0, 1, 5), labels=c("0%","1%","5%")), y=round(y, 2))
    # Add sig back in
    source_f1 <- source_f1[order(source_f1$group, source_f1$increase), ]
    f1 <- f1[order(f1$group, f1$x), ]
    f1$sig <- source_f1$sig
    # Get axes colors
    f1$a <- ifelse(f1$sig == 'p>0.05', link_colour[2], link_colour[1])
    f1_unique <- f1[!duplicated(f1$group), ]
    axes_colours <- f1_unique$a[order(f1_unique$group)] # ggplot interprets by name
    val <- rev(link_colour)
    
  } else {
    # DD PLOT
    source_f1 <- source_f0 %>% filter(season == s) %>% filter(group %in% c("Minimum dietary diversity", "Dietary diversity scores*"))
    # Prepare data
    f1 <- source_f1 %>%
      rename(x = increase, y = value) %>%
      mutate(yshift = if_else(group == "Minimum dietary diversity", 0, 0.1), ypos=yshift+y) %>%
      select(group, yshift, x, y, ypos, sig)
    f1 <- transform(f1, x=factor(x, levels=c(0, 1, 5), labels=c("0%","1%","5%")), y=round(y, 2))
    # Get axes colors
    f1$a <- ifelse(f1$sig == 'p>0.05', link_colour[2], link_colour[1])
    val <- f1$a
  }
  
  # Plot
  (gg <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
      geom_line(aes(group=group, colour=sig)) +
      scale_color_manual(name="Difference from 0%", values = val, limits=c('p>0.05', 'p<0.05')) +  # Set line colors manually
      labs(color = "") +  # Modify legend label
      xlab("Increase in flooding") +
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) +
      annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) +
      geom_point(colour="white",size=8) +
      geom_text(aes(label=y, colour=sig), size=3, family="Helvetica", show.legend = FALSE) +
      scale_y_continuous(name="", breaks=subset(f1, x==head(x,1))$ypos, labels=subset(f1, x==head(x,1))$group) + 
      theme_classic() +
      theme(axis.ticks = element_blank(),
            plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
            axis.text = element_text(family = "Helvetica", face="bold"),
            plot.margin=unit(c(0,2.5,0,0), 'cm')))
  
  if (x_labs=="Yes"){
    
    gg <- gg + theme(axis.text.y = element_text(color = axes_colours),
                     legend.position='bottom')
  } else {
    
    gg <- gg + 
      coord_cartesian(ylim=c(min(f1$ypos)-1, max(f1$ypos)+1)) + # Zoom out for DD variables
      theme(axis.text.y = element_text(color = f1$a, family = "Helvetica", face="bold"),
            axis.text.x=element_blank(),
            axis.title.x= element_blank(),
            legend.position="none") 
  }
  
  
  return (gg)
}

# GG plotter for SF (abs diff flood levels by HFP)
Abs_flood_Treat <- function(df, s, outcome, title="Outcome", x_labs='No', legend='Yes', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes){
  
  df <- source_f0
  # FILTER DATA
  source_f1 <- df %>% filter(season == s) %>% filter(group %in% c(outcome))  %>% filter(!(treat %in% c("HFP-Control")))
  source_f2 <- df %>% filter(season == s) %>% filter(group %in% c(outcome))  %>% filter(treat %in% c("HFP-Control"))
  
  # PROCESS FOR PLOTING
  f1 <- tufte_sort(source_f1, x="increase", y="value", group="treat", method="tufte", min.space=0.05, max.space=0.3)
  f1 <- transform(f1, x=factor(x, levels=c(0, 1, 5), labels=c("0%","1%","5%")), y=round(y, 2))
  f2 <- transform(source_f2, increase=factor(increase, levels=c(0, 1, 5), labels=c("0%","1%","5%")), y=round(value, 2))
  # Create Label Points for vertical lines (Difference between Treat & Control)
  control <- f1[f1$group == "Control", ] 
  diff_labs <- control$y + (source_f2$value/2) 
  
  
  if (binary=='Yes'){
    y_lab <- 'Probability'
    y_breaks <- seq(0, 1.1, 0.25)
    y_lim <- c(0, 1.1)
    nudge <- 0.1
  } else{
    y_lab <- 'Mean'
    y_breaks <- seq(4, 7, 0.5)
    y_lim <- c(4, 6.5)
    nudge <- 0.4
  }
  
  if(y_labs=='None'){
    y_lab <- ""
  } 
  
  if(title=="Season"){
    title <- s
  } else{
    title <- outcome
  }
  # AESTETIC DATA
  f1 <- f1[order(f1$group, f1$x), ]   
  f1$sig <- source_f2$sig # Add sig back in (this represents the flood trend sig)
  f1$seas <- source_f2$season # Add season back in
  seas_col <- custom_colors[[unique(f1$seas)]] # get season color '#ce1126' 
  f1$labels <- round(source_f2$value, 2) # get geom labels
  # Get test line colors
  f1$test_col <- 'grey'
  f1$test_col[f1$sig == 'p<0.05'] <- seas_col
  # Get trend line colors (shift significance t be more intuitive visually)
  line_sig <- subset(f1, group =='Control')$sig
  if(all(line_sig==c("p>0.05", "p>0.05", "p>0.05"))){
    f1$line_sig <- c("p>0.05", "p>0.05", "p>0.05")
  } else if (all(line_sig==c("p<0.05", "p>0.05", "p>0.05"))){
    f1$line_sig <- c("p>0.05", "p>0.05", "p>0.05")
  } else if (all(line_sig==c("p<0.05", "p<0.05", "p>0.05"))){
    f1$line_sig <- c("p<0.05", "p>0.05", "p>0.05")
  } else if (all(line_sig==c("p<0.05", "p<0.05", "p<0.05"))){
    f1$line_sig <- c("p<0.05", "p<0.05", "p<0.05")
  } else if (all(line_sig==c("p>0.05", "p<0.05", "p<0.05"))){ # 5% increase sig
    f1$line_sig <- c("p>0.05", "p<0.05", "p<0.05")
  } else if (all(line_sig==c("p>0.05", "p>0.05", "p<0.05"))){
    f1$line_sig <- c("p>0.05", "p>0.05", "p<0.05")
  } else if (all(line_sig==c("p<0.05", "p>0.05", "p<0.05"))){
    f1$line_sig <- c("p>0.05", "p>0.05", "p>0.05") #****
  } else if (all(line_sig==c("p>0.05", "p<0.05", "p>0.05"))){
    f1$line_sig <- c("p>0.05", "p>0.05", "p>0.05") #****
  }
  
  # PLOT
  (gg <- ggplot(f1,aes(x=x,y=y)) +
      # Add geometry
      geom_line(aes(group=x, linetype=sig), colour=seas_col, show.legend = FALSE) +
      geom_line(aes(group=group, colour=line_sig), show.legend = FALSE) + # Points
      geom_point(aes(shape=group, colour=sig), size=3) +
      # Add text to geom
      geom_text(data = subset(f1, sig == "p<0.05" & group == "HFP"), aes(label=labels, y=max(f1$y)), colour=seas_col, nudge_y=nudge) +
      # Set color, shapes & line types
      scale_shape_manual(values=custom_shapes, name='Trial-arm') +
      scale_linetype_manual(values=custom_lines, name='') +
      scale_color_manual(values=custom_colors_sig, name='Difference between trial-arms', limits=c('p>0.05', 'p<0.05')) +
      # Set axes
      scale_y_continuous(limits = y_lim, breaks=y_breaks) + 
      # scale_y_continuous(breaks=seq(round(min(f1$y), 1), max(f1$y), round(diff(range(f1$y))/5, 2))) + # set y breaks
      # scale_y_continuous(breaks=y_breaks) + # set y breaks
      labs(title=title, y=y_lab, x="Increase in flooding", color = "", shape="", linetype="") +  # Modify axes & legend labels
      # Format borders
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) +
      annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) +
      guides(shape = guide_legend(order = 1))) # guide_legend(order = 2, override.aes = list(shape=NA))
  
  # Fix legend
  if(x_labs=='Yes'){
    
    (gg2 <- gg + theme(axis.ticks.x = element_blank(),
                       axis.title.x = element_text(margin = margin(t = 15)), # shift away from x_labs
                       axis.title.y = element_text(margin = margin(r = 15)), 
                       plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
                       axis.text = element_text(family = "Helvetica", face="bold"),
                       legend.position="bottom"))
    if(legend=='No'){
      
      (gg2 <- gg2 + theme(legend.position="none"))
      
    }
    
  } else if(x_labs=='No'){
    
    (gg2 <- gg + theme(axis.ticks.x = element_blank(),
                       axis.title.x = element_blank(), 
                       axis.title.y = element_text(margin = margin(r = 15)), 
                       axis.text.x = element_blank(),
                       plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
                       axis.text = element_text(family = "Helvetica", face="bold"),
                       legend.position="none"))
  }
  
  
  return(gg2)
  
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
# SF2: Relative differences in probability of different strata of season & trial-arms for each DD outcome (1% flood) ####

# NEED: Group the y-axes by Model & add segment lines accordingly
#       Fix the text alignment & Title/axes names

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Rel-Diff')
dd_outcomes <- unique(source_f0$Outcome)

for(d in dd_outcomes) {
  
  # Select data
  source_f1 <- source_f0 %>% filter(Outcome == d) %>%
    mutate(Seas = factor(Seas, levels = c('Overall', 'Jan/Feb', 'Mar/Apr', 'May/Jun', 'Jul/Aug', 'Sep/Oct', 'Nov/Dec')),
           Treat = factor(Treat, levels = c('Overall', 'Control', 'HFP'))) %>%
    arrange(Model, Seas, Treat)
  source_f1$Index <- 1:nrow(source_f1)
  source_f1$Index <- source_f1$Index[rev(1:length(source_f1$Index))]

  # Split up the models
  f1 <- source_f1 %>% filter(Model == 'F1') 
  f2 <- source_f1 %>% filter(Model == 'F2') 
  f3 <- source_f1 %>% filter(Model == 'F3') 
  f4 <- source_f1 %>% filter(Model == 'F4') 
  
  # Conditional names for x-axes
  if(d=='WDDS') {
    x_ax <- 'Changes in mean'
    x_lim <- c(-0.35, .35)
    x_breaks <- seq(-0.35, 0.35, 0.1)
  } else {
    x_ax <- 'Changes in probability'
    x_lim <- c(-0.20, 0.20)
    x_breaks <- seq(-0.20, 0.20, 0.05)
  }
  
  # Get legend
  (all_plot <- 
      source_f1 |>
      # Make plot
      ggplot(aes(y = Index)) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Seas, shape=Treat), size=3) + ##
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Seas)) + ##
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      # Add labels, color & shape
      labs(x=x_ax, y="", color="Season", shape="Trial-arm") + 
      scale_y_continuous(breaks=1:nrow(source_f1), labels=rev(source_f1$Group)) +
      scale_x_continuous(breaks=x_breaks) +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = custom_shapes) +
      # Format
      coord_cartesian(ylim=c(1,22), xlim=x_lim) + # Zoom out
      guides(shape = guide_legend(order = 1), color = guide_legend(order = 2)) + # Legend order
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.line.y = element_blank(), # Remove y axes
            axis.ticks.y= element_blank())) 
  legend <- get_legend(all_plot)
  
  # Create forest plots
  (plot_f4 <- 
      f4 |>
      # Make plot
      ggplot(aes(y = rev(1:nrow(f4)))) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Seas, shape=Treat), size=3) + 
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Seas)) + 
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(0.5, length(source_f1$Index), by = 2), color="gray", size=.5, alpha=.5) +# set horizontal lines between x groups
      # Add labels & set color
      scale_x_continuous(breaks=x_breaks) +
      labs(x=x_ax, y=unique(f4$Model)) + 
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = custom_shapes) +
      # Format
      coord_cartesian(ylim=c(0.5,12.5), xlim=x_lim, expand=FALSE) + # Zoom out
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) + # add boarder on top (x)
      annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) + # add board on side (y)
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.text.y = element_blank(),
            axis.ticks.y= element_blank(),
            legend.position="none")) 
  
  (plot_f3 <- 
      f3 |>
      # Make plot
      ggplot(aes(y = rev(1:nrow(f3)))) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Seas, shape=Treat), size=3) + 
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Seas)) + 
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(0.5, length(source_f1$Index), by = 1), color="gray", size=.5, alpha=.5) +# set horizontal lines between x groups
      # Add labels & set color
      labs(x="", y=unique(f3$Model)) + 
      scale_y_continuous(breaks=1:nrow(f3), labels=rev(f3$Group)) +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = custom_shapes) +
      # Format
      coord_cartesian(ylim=c(0.5, 6.5), xlim=x_lim, expand=FALSE) + # Zoom out
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) + # add boarder on top (x)
      annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) + # add board on side (y)
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.text.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x= element_blank(),
            legend.position="none")) 
  
  (plot_f2 <- 
      f2 |>
      # Make plot
      ggplot(aes(y = rev(1:nrow(f2)))) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Seas, shape=Treat), size=3) + 
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Seas)) + 
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(0.5, length(source_f1$Index), by = 1), color="gray", size=.5, alpha=.5) +# set horizontal lines between x groups
      # Add labels & set color
      labs(x="", y=unique(f2$Model)) + 
      scale_y_continuous(breaks=1:nrow(f2), labels=rev(f2$Group)) +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = custom_shapes) +
      # Format
      coord_cartesian(ylim=c(0.5, 2.5), xlim=x_lim, expand=FALSE) + # Zoom out
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) + # add boarder on top (x)
      annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) + # add board on side (y)
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.text.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x= element_blank(),
            legend.position="none"))
  
  (plot_f1 <- 
      f1 |>
      # Make plot
      ggplot(aes(y = rev(1:nrow(f1)))) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Seas, shape=Treat), size=3) + 
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Seas)) + 
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(0.5, length(source_f1$Index), by = 1), color="gray", size=.5, alpha=.5) +# set horizontal lines between x groups
      # Add labels & set color
      labs(x="", y=unique(f1$Model)) + 
      scale_y_continuous(breaks=1:nrow(f1), labels=rev(f1$Group)) +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = custom_shapes) +
      # Format
      coord_cartesian(ylim=c(0.5, 1.5), xlim=x_lim, expand=FALSE) + # Zoom out
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) + # add boarder on top (x)
      annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) + # add board on side (y)
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.text.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x= element_blank(),
            legend.position="none"))

  # Put plots together
  layout <- c(
    area(t = 0, l = 0, b = 2, r = 15), # F1
    area(t = 3, l = 0, b = 5, r = 15), # F2
    area(t = 6, l = 0, b = 12, r = 15), # F3
    area(t = 13, l = 0, b = 25, r = 15), # F4
    area(t = 0, l = 15, b = 25, r = 20) # Legend

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
    + plot_annotation(name, theme = theme(plot.title = element_text(size = 16, hjust = 0.4))))

  ggsave(paste0('MF2_RelativeDiff/', d, "_sf2.png"), res, width=18, height=16, units='cm')
  
}


# MF: Absolute differences in probability of DD outcomes for different % flooding for each season (both trial-arms) ####

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName='R_Abs-Flood_Levels')


for(s in seasons) {
  
  (fg <- Abs_flood(df=source_f0, s='Jan/Feb', binary='Yes', x_labs='Yes'))
  (dd <- Abs_flood(df=source_f0, s='Jan/Feb', binary='No', x_labs='No'))

  # Put plots together
  layout <- c(
    area(t = 0, l = 0, b = 6, r = 15), # DD
    area(t = 8, l = 0, b = 30, r = 15) # Food groups
  )

  # Final plot arrangement
  (res <- dd + fg  
    + plot_layout(design = layout) 
    + plot_annotation(s, theme = theme(plot.title = element_text(size = 16, hjust = 0.62))))
  
  # Export image
  ggsave(paste0('MF3_AbsDiff - FloodLevels/', str_replace(s, "/", "-"), "_sf3.png"), 
         res, width=15, height=20, units='cm')
  
}


# SF4: Contrasting absolute differences in flood impact on DD outcomes between trial-arms for different levels of flooding across seasons #######

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R-Abs-Flood-Treat_Levels')

for(s in seasons) {
  
  p0 <- Abs_flood_Treat(source_f0, s, 'Dietary diversity scores*', x_labs='Yes', legend='Yes', binary='No', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  legend <- get_legend(p0)
  p1 <- Abs_flood_Treat(source_f0, s, 'Dietary diversity scores*', x_labs='No', legend='No', binary='No', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  p2 <- Abs_flood_Treat(source_f0, s, 'Minimum dietary diversity', x_labs='No', legend='No', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  p3 <- Abs_flood_Treat(source_f0, s, 'Starchy staples', x_labs='No', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  p4 <- Abs_flood_Treat(source_f0, s, 'Flesh foods', x_labs='No', legend='No', binary='Yes',y_labs='None',  custom_colors, custom_lines, custom_shapes)
  p5 <- Abs_flood_Treat(source_f0, s, 'Dairy', x_labs='No', legend='No', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  p6 <- Abs_flood_Treat(source_f0, s, 'Eggs', x_labs='No', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  p7 <- Abs_flood_Treat(source_f0, s, 'Dark green leafy vegetables', x_labs='No', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  p8 <- Abs_flood_Treat(source_f0, s, 'Vitamin A-rich foods', x_labs='No', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  p9<- Abs_flood_Treat(source_f0, s, 'Other vegetables', x_labs='Yes', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  p10 <- Abs_flood_Treat(source_f0, s, 'Other fruits', x_labs='Yes', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  p11 <- Abs_flood_Treat(source_f0, s, 'Legumes', x_labs='Yes', legend='No', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  p12 <- Abs_flood_Treat(source_f0, s, 'Nuts/seeds', x_labs='Yes', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  
  # Put plots together
  layout <- c(
    area(t = 1, l = 0, b = 5, r = 5), # WDDS
    area(t = 1, l = 6, b = 5, r = 10), # MDD
    area(t = 1, l = 11, b = 5, r = 15), # Starch
    area(t = 1, l = 16, b = 5, r = 20), # Flesh
    
    area(t = 6, l = 0, b = 11, r = 5), # Dairy
    area(t = 6, l = 6, b = 11, r = 10), # Eggs
    area(t = 6, l = 11, b = 11, r = 15), # dglv
    area(t = 6, l = 16, b = 11, r = 20), # vita

    area(t = 12, l = 0, b = 17, r = 5), # othf
    area(t = 12, l = 6, b = 17, r = 10), # othv
    area(t = 12, l = 11, b = 17, r = 15), # leg
    area(t = 12, l = 16, b = 17, r = 20), # nuts
    area(t = 18, l = 0, b = 18, r = 20) # legend
  )
  
  # Final plot arrangement
  (res <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + legend
    + plot_layout(design = layout) 
    + plot_annotation(s, theme = theme(plot.title = element_text(size = 16, hjust = 0.5))))
  
  # Export image
  ggsave(paste0('MF4_AbsDiff - Trial-Levels/', str_replace(s, "/", "-"), "_sf4.png"), 
         res, width=30, height=20, units='cm')
  
}

# MF: Contrasting absolute differences in flood impact on WDDS ONLY ####

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R-Abs-Flood-Treat_Levels')
outcome <- 'Dietary diversity scores*'

# Create plots
p0 <- Abs_flood_Treat(source_f0, 'Jan/Feb', outcome, title="Season", x_labs='Yes', legend='Yes', binary='No', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
legend <- get_legend(p0)
p1 <- Abs_flood_Treat(source_f0, 'Jan/Feb', outcome, title="Season", x_labs='No', legend='No', binary='No', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
p2 <- Abs_flood_Treat(source_f0, 'Mar/Apr', outcome, title="Season", x_labs='No', legend='No', binary='No', y_labs='None', custom_colors, custom_lines, custom_shapes)
p3 <- Abs_flood_Treat(source_f0, 'May/Jun', outcome, title="Season", x_labs='No', legend='No', binary='No', y_labs='None', custom_colors, custom_lines, custom_shapes)
p4 <- Abs_flood_Treat(source_f0, 'Jul/Aug', outcome, title="Season", x_labs='Yes', legend='No', binary='No', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
p5 <- Abs_flood_Treat(source_f0, 'Sep/Oct', outcome, title="Season", x_labs='Yes', legend='No', binary='No', y_labs='None',  custom_colors, custom_lines, custom_shapes)
p6 <- Abs_flood_Treat(source_f0, 'Nov/Dec', outcome, title="Season", x_labs='Yes', legend='No', binary='No', y_labs='None', custom_colors, custom_lines, custom_shapes)

# Plot layout
layout <- c(
  area(t = 1, l = 0, b = 5, r = 5), # Jan/Feb
  area(t = 1, l = 6, b = 5, r = 10), # Mar/Apr
  area(t = 1, l = 11, b = 5, r = 15), # May/Jun

  area(t = 6, l = 0, b = 11, r = 5), # Jul/Aug
  area(t = 6, l = 6, b = 11, r = 10), # Sep/Oct
  area(t = 6, l = 11, b = 11, r = 15), # Nov/Dec

  area(t = 12, l = 0, b = 12, r = 15) # legend
)

# Final arrangement
(res <- p1 + p2 + p3 + p4 + p5 + p6 + legend
  + plot_layout(design = layout) 
  + plot_annotation("Dietary Diversity Scores", theme = theme(plot.title = element_text(size = 16, hjust = 0.5))))

# Export image
ggsave(paste0('MF4_AbsDiff - Trial-Levels/', str_replace(outcome, "/", "-"), "_mf.png"), 
       res, width=30, height=20, units='cm')




# # OPTION MF4 : Absolute differences in probability of DD outcomes for different trial-arms for each season (1% flood) ####
# 
# source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Abs-Treat_Levels')
# 
# for(s in seasons) {
#   
#   link_colour <- c('darkgreen', 'grey')
#   s <- 'Jan/Feb'
#   # FOOD GROUP PLOT
#   source_f1 <- source_f0 %>% filter(season == s) %>% filter(!(group %in% c("Minimum dietary diversity", "Dietary diversity scores*")))
#   # Prepare data
#   f1 <- tufte_sort(source_f1, x="increase", y="value", group="group", method="tufte", min.space=0.05, max.space=0.15)
#   f1 <- transform(f1, x=factor(x, levels=c(0, 1), labels=c("Control","HFP")), y=round(y, 2))
#   # Add sig back in
#   source_f1 <- source_f1[order(source_f1$group, source_f1$increase), ]
#   f1 <- f1[order(f1$group, f1$x), ]
#   f1$sig <- source_f1$sig
#   # Get axes colors
#   f1$a <- ifelse(f1$sig == 'p>0.05', link_colour[2], link_colour[1])
#   f1_unique <- f1[!duplicated(f1$group), ]
#   axes_colours <- f1_unique$a[order(f1_unique$group)] # ggplot interprets by name
#   
#   # Plot
#   (gg_fg <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
#       geom_line(aes(group=group, colour=sig)) +
#       scale_color_manual(values = rev(link_colour), limits=c('p>0.05', 'p<0.05')) +  # Set line colors manually
#       labs(color = "95% Confidence") +  # Modify legend label
#       xlab("Trial-arm") +
#       annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) +
#       annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) +
#       geom_point(colour="white",size=8) +
#       geom_text(aes(label=y, colour=sig), size=3, family="Helvetica", show.legend = FALSE) +
#       scale_y_continuous(name="", breaks=subset(f1, x==head(x,1))$ypos, labels=subset(f1, x==head(x,1))$group) + 
#       theme(axis.ticks = element_blank(),
#             plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
#             axis.text = element_text(family = "Helvetica", face="bold"),
#             axis.text.y = element_text(color = axes_colours),
#             plot.margin=unit(c(0,2.5,0,0), 'cm'),
#             legend.position="bottom"))
#   
#   # DD PLOT
#   source_f2 <- source_f0 %>% filter(season == s) %>% filter(group %in% c("Minimum dietary diversity", "Dietary diversity scores*"))
#   # Prepare data
#   f2 <- source_f2 %>%
#     rename(x = increase, y = value) %>%
#     mutate(yshift = if_else(group == "Minimum dietary diversity", 0, 0.1), ypos=yshift+y) %>%
#     select(group, yshift, x, y, ypos, sig)
#   f2 <- transform(f2, x=factor(x, levels=c(0, 1), labels=c("Control","HFP")), y=round(y, 2))
#   # Get axes colors
#   f2$a <- ifelse(f2$sig == 'p>0.05', link_colour[2], link_colour[1])
#   # Plot
#   (gg_dd <- ggplot(f2,aes(x=x,y=ypos, colour=sig)) +
#       geom_line(aes(group=group, colour=sig)) +
#       scale_color_manual(values = f2$a)+  # Set line colors manually
#       geom_point(colour="white",size=8) +
#       annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) +
#       annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) +
#       geom_text(aes(label=y, colour=sig), size=3, family="Helvetica", show.legend = FALSE) +
#       scale_y_continuous(name="", breaks=subset(f2, x==head(x,1))$ypos, labels=subset(f2, x==head(x,1))$group) + 
#       coord_cartesian(ylim=c(min(f2$ypos)-1, max(f2$ypos)+1)) + # Zoom out
#       theme(axis.ticks = element_blank(),
#             plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
#             axis.text.y = element_text(color = f2$a, family = "Helvetica", face="bold"),
#             # axis.line.x = element_blank(),
#             axis.text.x=element_blank(),
#             axis.title.x= element_blank(),
#             plot.margin=unit(c(0,2.5,0,0), 'cm'),
#             legend.position="none")) 
#   
#   # Put plots together
#   layout <- c(
#     area(t = 0, l = 0, b = 6, r = 15), # DD
#     area(t = 8, l = 0, b = 30, r = 15) # Food groups
#   )
#   
#   # Final plot arrangement
#   (res <- gg_dd + gg_fg  
#     + plot_layout(design = layout) 
#     + plot_annotation(s, theme = theme(plot.title = element_text(size = 16, hjust = 0.62))))
#   
#   # Export image
#   ggsave(paste0('MF4_AbsDiff - Trial-arm/', str_replace(s, "/", "-"), "_mf4.png"), 
#          res, width=15, height=20, units='cm')
#   
# }
# 
# 
# 
# 
# 
# Other #######


# # MDD PLOT
# source_f2 <- source_f0 %>% filter(season == s) %>% filter(group =="Minimum dietary diversity")
# # Prepare data
# f2 <- source_f2 %>%
#   rename(x = increase, y = value) %>%
#   mutate(yshift=0.000, ypos=yshift+y) %>%
#   select(group, yshift, x, y, ypos, sig)
# f2 <- transform(f2, x=factor(x, levels=c(0, 1, 5, 10), labels=c("0%","1%","5%","10%")), y=round(y, 2))
# # Get axes colors
# f2$a <- ifelse(f2$sig == 'p>0.05', link_colour[2], link_colour[1])
# # Plot
# (gg_mdd <- ggplot(f2,aes(x=x,y=ypos, colour=sig)) +
#   geom_line(aes(group=group, colour=sig)) +
#   scale_color_manual(values = f2$a)+  # Set line colors manually
#   geom_point(colour="white",size=8) +
#   annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) +
#   geom_text(aes(label=y, colour=sig), size=3, family="Helvetica", show.legend = FALSE) +
#   scale_y_continuous(name="", breaks=subset(f2, x==head(x,1))$ypos, labels=subset(f2, x==head(x,1))$group) + 
#   coord_cartesian(ylim=c(min(f2$y)-0.1, max(f2$y)+0.1)) + # Zoom out
#   theme(axis.ticks = element_blank(),
#         plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
#         axis.text.y = element_text(color = f2$a, family = "Helvetica", face="bold"),
#         # axis.line.x = element_blank(),
#         axis.text.x=element_blank(),
#         axis.title.x= element_blank(),
#         plot.margin=unit(c(0.5,2.5,0.5,0), 'cm'),
#         legend.position="none")) 
# 
# # WDDS PLOT
# source_f3 <- source_f0 %>% filter(season == s) %>% filter(group =="Dietary diversity scores*")
# # Prepare data
# f3 <- source_f3 %>% 
#   rename(x = increase, y = value) %>% 
#   mutate(yshift=0.05, ypos=yshift+y) %>% 
#   select(group, yshift, x, y, ypos, sig)
# f3 <- transform(f3, x=factor(x, levels=c(0, 1, 5, 10), labels=c("0%","1%","5%","10%")), y=round(y, 2))
# # Get axes colors
# f3$a <- ifelse(f3$sig == 'p>0.05', link_colour[2], link_colour[1])
# # Plot
# (gg_wdds <- ggplot(f3,aes(x=x,y=ypos, colour=sig)) +
#     geom_line(aes(group=group, colour=sig)) +
#     scale_color_manual(values = f3$a)+  # Set line colors manually
#     geom_point(colour="white",size=8) +
#     geom_text(aes(label=y, colour=sig), size=3, family="Helvetica", show.legend = FALSE) +
#     scale_y_continuous(name="", breaks=subset(f3, x==head(x,1))$ypos, labels=subset(f3, x==head(x,1))$group) + 
#     annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) +
#     annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) +
#     coord_cartesian(ylim=c(min(f3$y)-0.1, max(f3$y)+0.1)) + # Zoom out
#     theme(axis.ticks = element_blank(),
#           plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
#           axis.text.y = element_text(color = f3$a, family = "Helvetica", face="bold"),
#           axis.line.x = element_blank(),
#           axis.text.x=element_blank(),
#           axis.title.x= element_blank(),
#           plot.margin=unit(c(0.5,2.5,0.5,0), 'cm'),
#           legend.position="none")) 

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

