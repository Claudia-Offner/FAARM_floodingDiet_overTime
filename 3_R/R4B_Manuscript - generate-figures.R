################################################################################
#### GENERATE FIGURES  #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('G:/My Drive/1. Work/13. LSHTM/2. Research/FAARM/3. Analysis/')


## Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)

#### IMPORTANT - Run R0_Data_formatting first

# PACKAGES ####
packages <- c('openxlsx', 'reshape', 'reshape2', 'dplyr', 'stringr', 
              'ggmap', 'patchwork', 'gridExtra', 'ggh4x')
# library <- 'C:/Users/offne/Documents/R/win-library/FAARM/' # set path
# (.libPaths(library)) # Set library directory

#### Load packages from library
for (p in packages){
  # install.packages(p, lib = library) # devtools::install_github(username/repository)
  library(p, character.only = TRUE)
}


# FUNCTIONS ####

# Set ggplot theme
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

# GG mapper for flood levels, by season and cluster (highlighting Trial arms)
mapper <- function(basemap, df_spatial, season=NA, legend='yes') {
  
  if(is.na(season)) {
    
    title <- 'Average flood levels across all time points, by cluster'
    
  } else {
    
    df_spatial <- df_spatial[, c('c_code', 'lon', 'lat', 'treatment', season), drop = FALSE]
    colnames(df_spatial)[colnames(df_spatial) == season] <- "perc_flooded_c"
    title <- season    
    
  }
  
  (ggmap <- ggmap(basemap) + 
      # Add geometry
      geom_sf(data = df_spatial, aes(fill=perc_flooded_c, color=treatment, linetype=treatment), linewidth=1) +
      # Set fill, color & line types
      scale_fill_gradientn(name = 'Average cluster\nflood coverage', 
                           colours = c("#ffffff", "#49C1ADFF", "#357BA2FF", "#3E356BFF", "#0B0405FF"), #rev(mako(5)) - library(viridis)
                           limits = c(0, 0.35),  # Set limits for the legend
                           breaks = c(0, 0.04, 0.15, 0.25, 0.35),  # Specify breaks
                           labels = c("0%", "5%", "15%", "25%", "35%")) +  # Custom labels
      scale_linetype_manual(name='Trial arm', values=c('dashed', 'solid'), ) +
      scale_color_manual(name='Trial arm', values=c('black', 'red'), limits=c('Control', 'HFP')) +
      # Set legend 
      labs(title=title,  color = "", shape="", linetype="") +  
      guides(fill=guide_legend(order=1)) + 
      # Set axes
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size=24, face='bold'),  # center title
            legend.title = element_text(size=16), # legend text
            legend.text = element_text(size=16), # legend text
            legend.position = "right",       # legend position
            legend.key.size = unit(1, 'cm'), # legend size
            axis.title.x = element_blank(),  # Remove x-axis title
            axis.title.y = element_blank(),  # Remove y-axis title
            axis.text.x = element_blank(),   # Remove x-axis labels
            axis.text.y = element_blank(),   # Remove y-axis labels
            axis.line = element_blank(),     # Remove axis lines
            axis.ticks = element_blank(),    # Remove axis ticks
            panel.grid = element_blank(),    # Remove grid lines
            panel.border = element_blank(),  # Remove panel borders
            panel.background = element_blank() # Remove panel background
      ))
  
  if(legend=='no'){
    
    ggmap <- ggmap + theme(legend.position = "none")
    
  } 
  
  return(ggmap)
  
}

# Extract legend from ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# GG plotter for marginal effects - plot sections
marg_effect_sect <- function(df, d, m, legend='Yes', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes){
  
  # Select & format data
  source_f0 <- df
  source_f1 <- source_f0 %>% filter(Outcome == d) %>%
    mutate(Seas = factor(Seas, levels = c('Overall', 'Jan/Feb', 'Mar/Apr', 'May/Jun', 'Jul/Aug', 'Sep/Oct', 'Nov/Dec')),
           Treat = factor(Treat, levels = c('Overall', 'Control', 'HFP'))) %>%
    arrange(Model, Seas, Treat)
  source_f1$Index <- 1:nrow(source_f1)
  source_f1$Index <- source_f1$Index[rev(1:length(source_f1$Index))]
  
  # Split up models & formatting as needed
  if (m=="All"){
    f1 <- source_f1
    hz_line <- 1
    y_lim <- c(0.5, 21.5)
    
  } else if (m=="F4") {
    f1 <- source_f1 %>% filter(Model == m) 
    hz_line <- 2
    y_lim <- c(0.5,12.5)
    
  } else if(m=="F3"){
    f1 <- source_f1 %>% filter(Model == m) 
    hz_line <- 1
    y_lim <- c(0.5, 6.5)
    
  } else if(m=="F2"){
    f1 <- source_f1 %>% filter(Model == m) 
    hz_line <- 1
    y_lim <- c(0.5, 2.5)
    
  } else if(m=='F1') {
    f1 <- source_f1 %>% filter(Model == m) 
    hz_line <- 1
    y_lim <- c(0.5, 1.5)
  } 
  
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
  
  # Initial Plot
  (gg <- 
      f1 |>
      # Make plot
      ggplot(aes(y = rev(1:nrow(f1)))) + 
      theme_classic() +
      # Add points & error bars
      geom_point(aes(x=Diff, colour=Seas, shape=Treat), size=5) + 
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Seas)) + 
      # Add vertical & horizontal lines
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_hline(yintercept = seq(0.5, length(f1$Index), by = hz_line), color="gray", size=.5, alpha=.5) +# set horizontal lines between x groups
      # Add labels & set color
      labs(x=x_ax, y="", color="Season", shape="Trial arm") + 
      # geom_text(aes(label=Group)) + 
      scale_x_continuous(breaks=x_breaks) +
      scale_y_continuous(breaks=1:nrow(f1), labels=rev(f1$Group)) +
      scale_color_manual(values = custom_colors) +
      scale_shape_manual(values = custom_shapes) +
      # Format
      coord_cartesian(ylim=y_lim, xlim=x_lim, expand=FALSE) + # Zoom out
      guides(shape = guide_legend(order = 1), color = guide_legend(order = 2)) + # Legend order
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) + # add boarder on top (x)
      annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) + # add board on side (y)
      theme(plot.title = element_text(hjust = 0.5), # Center title
            axis.text.y = element_text(size=14),
            axis.text.x = element_text(size=14),
            axis.title.y = element_text(size=16),
            axis.title.x = element_text(size=16),            # axis.ticks.y= element_blank(),
            legend.position="right")) 
  
  # Conditional Plot
  if (legend=='No'){
    (gg <- gg +
       theme(legend.position="none"))
  }
  if (x_axes == 'No'){
    (gg <- gg +
       # Update labels
       labs(x="") + 
       # Update format
       theme(axis.ticks.x=element_blank(),
             axis.text.x= element_blank()))
  }
  if (y_axes == 'No'){
    (gg <- gg +
       # Update labels
       labs(y="") + 
       # Update format
       theme(axis.text.y = element_blank(),
             axis.ticks.y= element_blank())) 
  }
  
  
  return(gg)
}

# GG plotter for marginal effects - full plot
marg_effect_full <- function(df, d, legend='No', x_axes='No', y_axes='No', custom_colors, custom_shapes) {
  
  if (legend=='Yes'){
    (f0 <- marg_effect_sect(df, d, 'All', legend=legend, x_axes='No', y_axes=y_axes, custom_colors, custom_shapes))
    (leg <- get_legend(f0))
  }
  
  (f1 <- marg_effect_sect(df, d, 'F1', legend='No', x_axes='No', y_axes=y_axes, custom_colors, custom_shapes))
  (f2 <- marg_effect_sect(df, d, 'F2', legend='No', x_axes='No', y_axes=y_axes, custom_colors, custom_shapes))
  (f3 <- marg_effect_sect(df, d, 'F3', legend='No', x_axes='No', y_axes=y_axes, custom_colors, custom_shapes))
  (f4 <- marg_effect_sect(df, d, 'F4', legend='No', x_axes=x_axes, y_axes=y_axes, custom_colors, custom_shapes))
  
  # Conditional names for plot title
  if(d=='MDD') {
    name <- 'Minimum Dietary Diversity'
  } else if (d=='WDDS') {
    name <- 'Dietary Diversity Scores'
  } else {
    name <- paste0('Food Group: ', d)
  }
  if(d=='Nuts/seeds'){
    d <- 'Nuts & seeds'
  }
  
  # Put plots together
  layout <- c(
    area(t = 0, l = 0, b = 2, r = 15), # F1
    area(t = 3, l = 0, b = 5, r = 15), # F2
    area(t = 6, l = 0, b = 12, r = 15), # F3
    area(t = 13, l = 0, b = 25, r = 15), # F4
    area(t = 0, l = 15, b = 25, r = 20) # Legend
  )
  
  # Final plot arrangement
  if (legend=='Yes'){
    
    (res <- f1 + f2 + f3 + f4 + leg
      + plot_layout(design = layout) 
      + plot_annotation(name, theme = theme(plot.title = element_text(size = 18, hjust = 0.45))))
    
  } else {
    
    (res <- f1 + f2 + f3 + f4 
      + plot_layout(design = layout) 
      + plot_annotation(name, theme = theme(plot.title = element_text(size = 18, hjust = 0.45))))
    
  }

  ggsave(paste0('III. Figures/Marginal_Effects/', d, ".png"), res, width=18, height=20, units='cm')
  
  return(res)
}

# Tufte formater  (make sure reshape2 is used)
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
  tmp <- reshape2::dcast(df, group ~ x, value.var="y")
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
    yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
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
  tmp <- reshape2::melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
  ## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y
  
  tmp <- transform(tmp, ypos=y + scale*yshift)
  return(tmp)
  
}

# GG plotter for MF (abs diff flood levels by season)
Abs_flood <- function(df, s, binary='Yes', x_labs='Yes', link_colour=c('darkgreen', 'grey')){
  
  
  if (binary=='Yes') {
    
    # FOOD GROUP PLOT
    source_f1 <- source_f0 %>% filter(season == s) %>% filter(!(group %in% c("Dietary diversity scores", "Minimum dietary diversity", "Flesh foods", "Eggs", "Other vegetables", "Nuts/seeds")))
    source_f1$value <- as.numeric(source_f1$value)
    # Prepare data
    f1 <- tufte_sort(source_f1, x="increase", y="value", group="group", method="tufte", min.space=0.05, max.space=1)
    f1 <- transform(f1, x=factor(x, levels=c(0, 1, 2, 3), labels=c("None","1SD","2SD", ">2SD")), y=round(y, 2))
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
    if(s=='Mar/Apr'){
      spacing <- 4.5
    } else {
      spacing <- 3.9
    }
    # DD PLOT
    source_f1 <- source_f0 %>% filter(season == s) %>% filter(group %in% c("Dietary diversity scores"))
    source_f1$value <- as.numeric(source_f1$value)
    # Prepare data
    f1 <- source_f1 %>%
      dplyr::rename(x = increase, y = value) %>%
      mutate(yshift = if_else(group == "Minimum dietary diversity", 0, 0.2), ypos=yshift+y) %>%
      select(group, yshift, x, y, ypos, sig)
    f1$ypos[f1$group == "Dietary diversity scores*"] <- f1$ypos[f1$group == "Dietary diversity scores*"] - spacing
    f1 <- transform(f1, x=factor(x, levels=c(0, 1, 2, 3), labels=c("None","1SD","2SD", ">2SD")), y=round(y, 2))
    # Get axes colors
    f1$a <- ifelse(f1$sig == 'p>0.05', link_colour[2], link_colour[1])
    val <- f1$a
  }
  
  # Plot
  (gg <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
      geom_line(aes(group=group, colour=sig)) +
      scale_color_manual(name="Difference from no change", values = val, limits=c('p>0.05', 'p<0.05')) +  # Set line colors manually
      labs(color = "") +  # Modify legend label
      xlab("\nIncrease in flooding from seasonal average") +
      annotate(geom = 'segment', y = Inf, yend = Inf, x = -Inf, xend = Inf) +
      annotate(geom = 'segment', y = -Inf, yend = Inf, x = Inf, xend = Inf) +
      geom_point(colour="white",size=5) +
      geom_text(aes(label=y, colour=sig), size=5, family="Helvetica", show.legend = FALSE) +
      scale_y_continuous(name="", breaks=subset(f1, x==head(x,1))$ypos, labels=subset(f1, x==head(x,1))$group) + 
      theme_classic() +
      theme(axis.ticks = element_blank(),
            plot.title = element_text(hjust=0.5, family = "Helvetica", face="bold"),
            legend.title = element_text(size=14),
            legend.text = element_text(size=14),
            axis.text = element_text(family = "Helvetica", face="bold", size=14),
            axis.title = element_text(family = "Helvetica", face="bold", size=16),
            plot.margin=unit(c(0,2.5,0,0), 'cm')))
  
  if (x_labs=="Yes"){
    
    (gg <- gg + 
     theme(axis.text.y = element_text(color = axes_colours),
                     legend.position='bottom'))
  } else {
    
    (gg <- gg + 
      coord_cartesian(ylim=c(min(f1$ypos)-0.1, max(f1$ypos)+0.1)) + # Zoom out for DD variables
      theme(axis.text.y = element_text(color = f1$a, family = "Helvetica", face="bold"),
            axis.text.x=element_blank(),
            axis.title.x= element_blank(),
            legend.position="none")) 
  }
  
  
  return (gg)
}

# GG plotter for MF (abs diff flood levels by season and treatment)
Abs_flood_Treat <- function(df, s, outcome, title="Outcome", x_labs='No', legend='Yes', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes){
  # outcome <-'WDDS'
  # df <- source_f0
  # s <- 'Jan/Feb'
  # FILTER DATA
  source_f1 <- df %>% filter(season == s) %>% filter(group %in% c(outcome))  %>% filter(!(treat %in% c("HFP-Control")))
  source_f2 <- df %>% filter(season == s) %>% filter(group %in% c(outcome))  %>% filter(treat %in% c("HFP-Control"))
  
  # PROCESS FOR PLOTING
  f1 <- tufte_sort(source_f1, x="increase", y="value", group="treat", method="tufte", min.space=0.05, max.space=0.3)
  f1 <- transform(f1, x=factor(x, levels=c(0, 1, 2, 3), labels=c("None","1SD","2SD", ">2SD")), y=round(y, 2))
  f2 <- transform(source_f2, increase=factor(increase, levels=c(0, 1, 2, 3), labels=c("None","1SD","2SD", ">2SD")), y=round(value, 2))
  # Create Label Points for vertical lines (Difference between Treat & Control)
  control <- f1[f1$group == "Control", ] 
  diff_labs <- control$y + (source_f2$value/2) 
  
  
  if (binary=='Yes'){
    y_lab <- 'Probability'
    y_breaks <- seq(0, 1.1, 0.25)
    y_lim <- c(0, 1.1)
    nudge <- 0.15
  } else{
    y_lab <- 'Mean'
    y_breaks <- seq(4, 7, 0.5)
    y_lim <- c(4, 6.5)
    nudge <- 0.55
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
  f1$labels <- sprintf("%.2f", source_f2$value) # get geom labels
  # Get test line colors
  f1$test_col <- 'grey'
  f1$test_col[f1$sig == 'p<0.05'] <- seas_col
  # Get trend line colors (shift significance t be more intuitive visually)
  line_sig <- subset(f1, group =='Control')$sig
  if(all(line_sig==c("p>0.05", "p>0.05", "p>0.05", "p>0.05"))){ # no sig
    f1$line_sig <- c("p>0.05", "p>0.05", "p>0.05", "p>0.05")
  } else if (all(line_sig==c("p<0.05", "p>0.05", "p>0.05", "p>0.05"))){ # 1st sig
    f1$line_sig <- c("p>0.05", "p>0.05", "p>0.05","p>0.05")
  } else if (all(line_sig==c("p<0.05", "p<0.05", "p>0.05","p>0.05"))){ # 2nd sig
    f1$line_sig <- c("p<0.05", "p>0.05", "p>0.05","p>0.05")
  } else if (all(line_sig==c("p<0.05", "p<0.05", "p<0.05","p>0.05"))){ # 3rd sig
    f1$line_sig <- c("p<0.05", "p<0.05", "p<0.05","p>0.05")
  } else if (all(line_sig==c("p>0.05", "p<0.05", "p<0.05", "p>0.05"))){
    f1$line_sig <- c("p>0.05", "p<0.05", "p>0.05", "p>0.05") #****
  # } 
  # else if (all(line_sig==c("p>0.05", "p<0.05", "p>0.05"))){
  #   f1$line_sig <- c("p>0.05", "p>0.05", "p>0.05") #****
  } else {
    f1$line_sig <- line_sig
  } 

  
  # PLOT
  (gg <- ggplot(f1,aes(x=x,y=y)) +
      # Add geometry
      geom_line(aes(group=x, linetype=sig), colour=seas_col, show.legend = FALSE) +
      geom_line(aes(group=group, colour=line_sig), show.legend = FALSE) + # Points
      geom_point(aes(shape=group, colour=sig), size=4) +
      # Add text to geom
      geom_text(data = subset(f1, sig == "p<0.05" & group == "HFP"), aes(label=labels, y=max(f1$y)), colour=seas_col, size=5, nudge_y=nudge) +
      # Set color, shapes & line types
      scale_shape_manual(values=custom_shapes, name='Trial arm') +
      scale_linetype_manual(values=custom_lines, name='') +
      scale_color_manual(values=custom_colors_sig, name='Difference between trial arms', limits=c('p>0.05', 'p<0.05')) +
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
    
    (gg2 <- gg + theme(axis.title = element_text(family = "Helvetica", face="bold", size=15),
                       axis.text = element_text(family = "Helvetica", face="bold", size=14),
                       axis.title.y = element_text(margin = margin(r = 15)), 
                       axis.title.x = element_text(margin = margin(t = 15)), # shift away from x_labs
                       axis.ticks.x = element_blank(),
                       legend.title = element_text(size=14),
                       legend.text = element_text(size=14),
                       plot.title = element_text(size=16, hjust=0.5, family = "Helvetica", face="bold"),
                       legend.position="bottom"))
    if(legend=='No'){
      
      (gg2 <- gg2 + theme(legend.position="none"))
      
    }
    
  } else if(x_labs=='No'){
    
    (gg2 <- gg + theme(axis.title = element_text(family = "Helvetica", face="bold", size=15),                       axis.title.x = element_blank(), 
                       axis.text = element_text(family = "Helvetica", face="bold", size=14),
                       axis.title.y = element_text(margin = margin(r = 15)), 
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       legend.title = element_text(size=14),
                       legend.text = element_text(size=14),
                       plot.title = element_text(size=16, hjust=0.5, family = "Helvetica", face="bold"),
                       legend.position="none"))
  }
  
  return(gg2)
  
}


# SF: Descriptive - Temporal distribution of flooding & diets, pooled ####

# Load data
source_f0 <- read.xlsx(xlsxFile='II. Tables/desc_trial_rounds.xlsx')
# Clean data
source_f0 <- source_f0[, grepl('Round|Treatment|PROB|COEF', names(source_f0))]
source_f0 <- melt(source_f0, id.vars = c('Round', 'Treatment'))
source_f0 <- source_f0 %>% filter(!(Round %in% c("Baseline"))) # Remove non probability variables
f1 <- cbind(source_f0, str_split(source_f0$Round, "-", simplify = TRUE))
colnames(f1)[5:6] <- c("year", "month")
# f1$Round <- factor(f1$Round, levels = c("Baseline", sort(unique(f1$Round))[-25])) # Factor rounds

# Rename rounds
f1$month[f1$month==1] <- 'Jan/Feb'
f1$month[f1$month==2] <- 'Mar/Apr'
f1$month[f1$month==3] <- 'May/Jun'
f1$month[f1$month==4] <- 'Jul/Aug'
f1$month[f1$month==5] <- 'Sep/Oct'
f1$month[f1$month==6] <- 'Nov/Dec'
f1$Round <- paste0(f1$year, ' ', f1$month)

# Rename variables
unique(f1$variable)
f1$variable <- gsub("dd10r_score_m_COEF", "(A) WDDS", f1$variable)
f1$variable <- gsub("dd10r_min_m_PROB", "(B) MDD-W", f1$variable)
f1$variable <- gsub("dd10r_starch_PROB", "(C) Starchy staples", f1$variable)
f1$variable <- gsub("dd10r_flesh_PROB", "(D) Flesh foods", f1$variable)
f1$variable <- gsub("dd10r_dairy_PROB", "(E) Dairy products", f1$variable)
f1$variable <- gsub("dd10r_eggs_PROB", "(F) Eggs", f1$variable)
f1$variable <- gsub("dd10r_dglv_PROB", "(G) DGLV", f1$variable)
f1$variable <- gsub("dd10r_vita_PROB", "(H) Vitamin A-rich foods", f1$variable)
f1$variable <- gsub("dd10r_othv_PROB", "(I) Other vegetables", f1$variable)
f1$variable <- gsub("dd10r_othf_PROB", "(J) Other fruits", f1$variable)
f1$variable <- gsub("dd10r_legume_PROB", "(K) Legumes", f1$variable)
f1$variable <- gsub("dd10r_nuts_PROB", "(L) Nuts & Seeds", f1$variable)
f1$variable <- gsub("perc_flooded_c_PROB", "(M) Percent Flooded", f1$variable)
f1$variable<- factor(f1$variable, levels=unique(f1$variable))
f1$Treatment <- ifelse(f1$Treatment==0, 'Control', 'HFP')
f1$variable <- factor(f1$variable, levels = c('(A) WDDS', '(B) MDD-W', 
                                              '(C) Starchy staples', '(D) Flesh foods','(E) Dairy products',
                                              '(F) Eggs', '(G) DGLV', '(H) Vitamin A-rich foods', '(I) Other vegetables', 
                                              '(J) Other fruits', '(K) Legumes', '(L) Nuts & Seeds',
                                              '(M) Percent Flooded')) # Factor rounds

# Subset the data frame to keep only columns with names not containing the character(s) to remove
(gg <- ggplot(f1, aes(x = Round, y = as.numeric(value), color = as.factor(Treatment), group = as.factor(Treatment))) +
    geom_line() +
    labs(x = "", y = "", color = "variable") +
    scale_x_discrete(breaks = unique(f1$Round)[seq(1, length(unique(f1$Round)), by = 3)], # Label every other round
                     guide = guide_axis(angle = 45)) + # Rotate  axes labels
    scale_color_manual(values = c("Control" = "#3388f7", "HFP" = "#b51731"), name='Trial arm') + 
    facet_wrap(~ variable, nrow = NULL, ncol = NULL, scales = "free_y") +
    ggh4x::facetted_pos_scales(y = list(
      variable == '(A) WDDS' ~ scale_y_continuous(limits = c(0, 10)),
      variable == '(M) Percent Flooded' ~ scale_y_continuous(limits = c(0, 10)),
      TRUE ~ scale_y_continuous(limits = c(0, 100))
      )) + theme_bw())


ggsave(paste0('III. Figures/Descriptives_Time/', "time_series.png"), 
       gg, width=35, height=25, units='cm')

# MF: Descriptive - Spatial flood distribution across seasons, by cluster ####


# NEED API's (https://www.appsilon.com/post/r-ggmap)
# ggmap::register_google(key = "AIzaSyCVzPwqMVzz-f374mq0b-6UfsLXmMCFIU8", write = TRUE) # Use at own risk, it is connected to billing address
ggmap::register_stadiamaps(key="f2f7765b-7259-42c9-a46d-fc1a61dc4375")

# Clean flood data for visualizing
df[df$treatment == 0, "treatment"] <- 'Control'
df[df$treatment == 1, "treatment"] <- 'HFP'
flood_ALL <- aggregate(perc_flooded_c ~ c_code+treatment, data = df, FUN = mean) #  pooled
flood_SEASON <-cast(df, c_code+treatment~season_DD, mean, value = "perc_flooded_c") # by season
# Merge datasets
sdf_all <- merge(cluster_shp, flood_ALL, by='c_code') 
sdf_season <- merge(cluster_shp, flood_SEASON, by='c_code') 
# NB: Fiddle with the mapper flood limits based on min, max, etc.
summary(sdf_season)

# Get a buffered bounding box for the basemap
poly_box <- st_as_sfc(st_bbox(cluster_shp))
buffered_polygon <- st_buffer(poly_box, dist = 1000)  # 1km = 1000 meters
bbox <- as.list(st_bbox(buffered_polygon))
# NB: ggmap uses long/lat (NOT lat/long)
basemap <- get_map(c(left = bbox$xmin, 
                     bottom = bbox$ymin, 
                     right = bbox$xmax, 
                     top = bbox$ymax), 
                   source="stadia", maptype='stamen_terrain') #_background

### MAP

# Overall Map
(m0 <- mapper(basemap, sdf_all, season=NA, legend='yes'))
ggsave(paste0('III. Figures/Descriptives_Flood/Overall.png'), 
       m0, width=15, height=20, units='cm')

# Seasonal Maps
for (s in seasons) {
  # Make summary stats table
  col <- season_means[season_means$season_flood == s, ] # Use table from formatting - it best reps the data analysed
  # col <- sdf_season[[s]]
  # col <- df[df$season_DD == s, ]$perc_flooded_c
  (sum <- data.frame(
    Seasonal = c("Mean", "SD", "Min", "Max"), #Row.Names
    Statistics = paste0(sprintf("%.2f", c(col$flood_mean, col$flood_sd, col$flood_min, col$flood_max)*100), '%')
  ))
  
  (m1 <- mapper(basemap, sdf_season, season=s, legend='no') +
      annotation_custom(grob = tableGrob(sum, rows=NULL, cols=NULL, ttheme_default(base_size=20)), xmin = 91.49, xmax = 91.61, ymin=24.35, ymax = 24.5)
  )
  
  # Export image
  ggsave(paste0('III. Figures/Descriptives_Flood/', str_replace(s, "/", "-"), ".png"), 
         m1, width=15, height=20, units='cm')
  
}



# MF: Marginal effects of interaction model for each DD outcome (1SD flood) ####

source_f0 <- read.xlsx(xlsxFile='III. Figures/Visuals.xlsx', sheet='R_Rel_Diff')
# Check outcome names
# dd_outcomes <- unique(source_f0$Outcome)

# Get full plot for each dietary outcome
(f1 <- marg_effect_full(source_f0, 'WDDS', legend='No', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes))
(f2 <- marg_effect_full(source_f0, 'MDD', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes))
(f3 <- marg_effect_full(source_f0, 'Flesh foods', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes))
(f4 <- marg_effect_full(source_f0, 'Dairy', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes))
(f5 <- marg_effect_full(source_f0, 'Eggs', legend='No', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes))
(f6 <- marg_effect_full(source_f0, 'DGLV', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes))
(f7 <- marg_effect_full(source_f0, 'Vitamin A-rich foods', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes))
(f8 <- marg_effect_full(source_f0, 'Other vegetables', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes))
(f9 <- marg_effect_full(source_f0, 'Other fruits', legend='No', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes))
(f10 <- marg_effect_full(source_f0, 'Legumes', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes))
(f11 <- marg_effect_full(source_f0, 'Nuts/seeds', legend='Yes', x_axes='Yes', y_axes='No', custom_colors, custom_shapes))

# MF: Predicted measures of DD outcomes for different flood levels across seasons (pooled Trial arms) ####

source_f0 <- read.xlsx(xlsxFile='III. Figures/Visuals.xlsx', sheet='R_Abs_Flood_Levels')
# Adapt other veg or visualization purposes
source_f0$sig[source_f0$group=='Other vegetables'] <- 'p>0.05'
source_f0$group[source_f0$group=='Dietary diversity scores*'] <- 'Dietary diversity scores'

for(s in seasons) {
  
  fg <- Abs_flood(df=source_f0, s=s, binary='Yes', x_labs='Yes')
  dd <- Abs_flood(df=source_f0, s=s, binary='No', x_labs='No')
  
  # Put plots together
  layout <- c(
    area(t = 0, l = 0, b = 6, r = 15), # DD
    area(t = 8, l = 0, b = 30, r = 15) # Food groups
  )
  
  # Final plot arrangement
  (res <- dd + fg  
    + plot_layout(design = layout) 
    + plot_annotation(s, theme = theme(plot.title = element_text(size = 18, hjust = 0.7))))
  
  # Export image
  ggsave(paste0('III. Figures/Predicted_Meas_Season/', str_replace(s, "/", "-"), ".png"), 
         res, width=20, height=20, units='cm')
  
}



# MF: Predicted measures of DD outcomes for different flood levels across seasons and Trial arms ####

source_f0 <- read.xlsx(xlsxFile='III. Figures/Visuals.xlsx', sheet='R_Abs_Flood_Treat_Levels')
source_f0$group[source_f0$group=='WDDS'] <- 'Dietary diversity scores'

for(s in seasons) {
  
  p0 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Dietary diversity scores', x_labs='Yes', legend='Yes', binary='No', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  legend <- get_legend(p0)
  p1 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Dietary diversity scores', x_labs='No', legend='No', binary='No', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  p2 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Dairy', x_labs='No', legend='No', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  p3 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'DGLV', x_labs='No', legend='No', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  p4 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Vitamin A-rich foods', x_labs='No', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  p5 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Other fruits', x_labs='Yes', legend='No', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  p6 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Legumes', x_labs='Yes', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  
  # p1 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'MDD', x_labs='No', legend='No', binary='Yes', y_labs='Yes', custom_colors, custom_lines, custom_shapes)
  # p2 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Flesh foods', x_labs='No', legend='No', binary='Yes',y_labs='None',  custom_colors, custom_lines, custom_shapes)
  # p3 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Eggs', x_labs='No', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  # p4 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Other vegetables', x_labs='Yes', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  # p5 <- Abs_flood_Treat(source_f0, s=s, title="Outcome", 'Nuts/seeds', x_labs='Yes', legend='No', binary='Yes', y_labs='None', custom_colors, custom_lines, custom_shapes)
  
  # Put plots together
  layout <- c(
    area(t = 1, l = 0, b = 5, r = 5), # WDDS
    area(t = 1, l = 6, b = 5, r = 10), # Dairy

    area(t = 6, l = 0, b = 11, r = 5), # DGLV
    area(t = 6, l = 6, b = 11, r = 10), # Vita
    
    area(t = 12, l = 0, b = 17, r = 5), # fruit
    area(t = 12, l = 6, b = 17, r = 10), # legume

    area(t = 18, l = 0, b = 18, r = 10) # legend
  )
  
  # Final plot arrangement
  (res <- p1 + p2 + p3 + p4 + p5 + p6 + legend
    + plot_layout(design = layout) 
    + plot_annotation(s, theme = theme(plot.title = element_text(size = 20, hjust = 0.5, face='bold'))))
  
  # Export image
  ggsave(paste0('III. Figures/Predicted_Meas_Season-trial/', str_replace(s, "/", "-"), ".png"), 
         res, width=20, height=18, units='cm')
  
}

