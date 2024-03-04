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
dd_outcomes <- unique(source_f0$Outcome)[-1]

for(d in dd_outcomes) {
  
  # Select data
  source_f1 <- source_f0 %>% filter(Outcome == d) 
  source_f1$Index <- 1:nrow(source_f1)
  source_f1$Index <- source_f1$Index[rev(1:length(source_f1$Index))]
  
  # Format data for left & right side of plots
  res_plot <- mf2_plot_format(source_f1)

  # Create forest plots
  (res <- 
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
      labs(title=paste0('Dietary Outcome: ', d), x="Change in probability", y="") + 
      # Zoom out
      coord_cartesian(ylim=c(1,22), xlim=c(-0.12, .12)) + 
      # Add text
      annotate("text", x = -.08, y = 22, label = "Flooding harmful") +
      annotate("text", x = .08, y = 22, label = "Flooding protective") + 
      scale_y_continuous(breaks=1:nrow(source_f1), labels=rev(source_f1$Group)) +
      # Centre title
      theme(plot.title = element_text(hjust = 0.5)))
      # Change legend position
      # theme(legend.position = c(0.9, 0.8)) +
      # Remove axes
      # theme(axis.line.y = element_blank(),
      #       axis.ticks.y= element_blank(),
      #       axis.text.y= element_blank(),
      #       axis.title.y= element_blank()))
      
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
  ggsave(paste0('MF2_RelativeDiff/', d, "_mf2.png"), res, width=15, height=15, units='cm')
  
}


# MF3: Absolute differences in probability of DD outcomes for different % flooding for each season (both trial-arms) ####

# NEED: Add dietary diversity variables at top, with specific colours (*astrix for WDDS)
#       AND set significance by dotted line
#       AND loop over each season
#       AND extract each plot individually AND as a 2x6 plot grid

source_f0 <- read.xlsx(file='Visuals.xlsx', sheetName = 'R_Abs-Flood_Levels')

for(s in seasons) {
  # Prepare data    
  source_f1 <- source_f0 %>% filter(season == s) %>% filter(!(group %in% c("Dietary diversity scores*")))
  f1 <- tufte_sort(source_f1, x="increase", y="value", group="group", method="tufte", min.space=0.05, max.space=1)
  f1 <- transform(f1, x=factor(x, levels=c(0, 1, 5, 10), labels=c("0%","1%","5%","10%")), y=round(y, 2))
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
  (gg_mf3 <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
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
      labs(title=paste0("Predicted probabilities of achieving subsequent dietary outcomes \nin ", s, " for different % of cluster flooded")) +
      xlab("Increase in flooding"))
  # Export image
  ggsave(paste0('MF3_AbsDiff - FloodLevels/', str_replace(s, "/", "-"), "_mf3.png"), gg_mf3, width=15, height=15, units='cm')
  
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

