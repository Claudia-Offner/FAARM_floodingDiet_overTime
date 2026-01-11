### ------------------------------------------------------------------------ ### 
### Generate figures
### ------------------------------------------------------------------------ ### 

# Clear environment
rm(list=ls())

### IMPORTANT - set file paths to folder locations
data_path <- 'C:/Users/claer14/OneDrive - University of Cambridge/V. Other/Flooding-Diets-HFP/Data/'
git_path  <- 'C:/Users/claer14/Documents/GitHub/FAARM_floodingDiet_overTime/3_R'
setwd(git_path)

#### DEPENDENCIES ####
source('R0_Dependencies.R')

# GG mapper for flood levels, by season and cluster (highlighting Trial arms)
mapper <- function(basemap, df_spatial, stats_df=NULL, title='') {
  
  p <- ggmap(basemap) +
    geom_sf(data=df_spatial, aes(fill=perc_flooded_c, color=treatment, linetype=treatment), linewidth=0.4) +
    facet_wrap(~season_flood, ncol=3) +
    # Set fill, color & line types
    scale_fill_gradientn(name='Average cluster\nflood coverage', 
                         colours=c('#ffffff', '#49C1ADFF', '#357BA2FF', '#3E356BFF', '#0B0405FF'),
                         breaks=c(0, 0.05, 0.15, 0.25, 0.35),
                         labels=c('0-4.9%', '5-14.9%', '15-24.9%', '25-34.9%', '>=35%')) +
    scale_linetype_manual(name='Trial arm', values=c('solid', 'solid'), ) +
    scale_color_manual(name='Trial arm', values=c('black', 'red'), limits=c('Control', 'HFP')) +
    # Set legend 
    labs(title=title,  color='', shape='', linetype='') +  
    guides(fill=guide_legend(order=1)) + 
    # Set axes
    theme_minimal() +
    theme(
      plot.title=element_blank(),
      strip.text=element_text(size=15, face='bold'),
      axis.text=element_blank(),
      axis.title=element_blank(),
      panel.grid=element_blank(),
      panel.spacing=unit(0.2, 'cm'),
      plot.margin=margin(5,5,5,5, 'pt'),
      legend.title=element_text(face='bold')
      ) +
    coord_sf(clip='off') 
  
  if(!is.null(stats_df)){
    p <- p +
      geom_richtext(
        data=stats_df,
        aes(x=Inf, y=-Inf, label=label),
        hjust=1, vjust=0,
        size=3,
        lineheight=0.95,
        fill='white',
        label.color='black',
        label.padding=unit(c(4, 6, 4, 6), 'pt'),
        inherit.aes=FALSE
      )
  }
  
  return(p)
}

# GG plotter for marginal effects - full plot
marg_effect_full <- function(df, alpha, d, legend='No', x_axes='No', y_axes='No', custom_colors, custom_shapes) {
  
  # GG plotter for marginal effects - plot sections
  marg_effect_sect <- function(df, alpha='', d, m, title='No',legend='Yes', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes){
    
    # Select & format data
    source_f0 <- df
    source_f1 <- source_f0 %>% filter(Outcome == d) %>%
      mutate(Seas=factor(Seas, levels=c('Overall', 'Jan/Feb', 'Mar/Apr', 'May/Jun', 'Jul/Aug', 'Sep/Oct', 'Nov/Dec')),
             Treat=factor(Treat, levels=c('Overall', 'Control', 'HFP'))) %>%
      arrange(Model, Seas, Treat)
    source_f1$Index <- 1:nrow(source_f1)
    source_f1$Index <- source_f1$Index[rev(1:length(source_f1$Index))]
    
    # Split up models & formatting as needed
    if (m=='All'){
      f1 <- source_f1
      hz_line <- 1
      y_lim <- c(0.5, 21.5)
      
    } else if (m=='F4') {
      f1 <- source_f1 %>% filter(Model == m) 
      hz_line <- 2
      y_lim <- c(0.5,12.5)
      
    } else if(m=='F3'){
      f1 <- source_f1 %>% filter(Model == m) 
      hz_line <- 1
      y_lim <- c(0.5, 6.5)
      
    } else if(m=='F2'){
      f1 <- source_f1 %>% filter(Model == m) 
      hz_line <- 1
      y_lim <- c(0.5, 2.5)
      
    } else if(m=='F1') {
      f1 <- source_f1 %>% filter(Model == m) 
      hz_line <- 1
      y_lim <- c(0.5, 1.5)
    } 
    
    # Conditional names for x-axes
    if(d=='DDS') {
      x_ax <- 'Change in mean'
      x_lim <- c(-0.35, .35)
      x_breaks <- seq(-0.35, 0.35, 0.1)
    } else {
      x_ax <- 'Change in probability'
      x_lim <- c(-0.20, 0.20)
      x_breaks <- seq(-0.20, 0.20, 0.05)
    }
    
    # Conditional names for plot title
    if(d=='MDD') {
      name <- paste0(alpha, ' Minimum Dietary Diversity')
    } else if (d=='DDS') {
      name <- paste0(alpha, ' Dietary Diversity Score')
    } else {
      name <- paste0(alpha, ' Food Group: ', d)
    }
    
    # Initial Plot
    gg <- 
        f1 |>
        # Make plot
        ggplot(aes(y=rev(1:nrow(f1)))) + 
        theme_classic() +
        # Add points & error bars
        geom_point(aes(x=Diff, colour=Seas, shape=Treat), size=5) + 
        geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, colour=Seas)) + 
        # Add vertical & horizontal lines
        geom_vline(xintercept=0, linetype='dashed') +
        geom_hline(yintercept=seq(0.5, length(f1$Index), by=hz_line), color='gray', size=.5, alpha=.5) +
        # Add labels & set color
        labs(x=x_ax, y='', color='Season', shape='Trial arm') + 
        # geom_text(aes(label=Group)) + 
        scale_x_continuous(breaks=x_breaks) +
        scale_y_continuous(breaks=1:nrow(f1), labels=rev(f1$Group)) +
        scale_color_manual(values=custom_colors) +
        scale_shape_manual(values=custom_shapes) +
        # Format
        coord_cartesian(ylim=y_lim, xlim=x_lim, expand=FALSE) + # Zoom out
        guides(shape=guide_legend(order=1), color=guide_legend(order=2)) + # Legend order
        annotate(geom='segment', y=Inf, yend=Inf, x=-Inf, xend=Inf) + # add boarder on top (x)
        annotate(geom='segment', y=-Inf, yend=Inf, x=Inf, xend=Inf) + # add board on side (y)
        theme(plot.title=element_text(hjust=0.5), # Center title
              axis.text.y=element_text(size=14),
              axis.text.x=element_text(size=14, angle=45, hjust=1),
              axis.title.y=element_text(size=16),
              axis.title.x=element_text(size=16),
              plot.margin=margin(0, 0, 0, 0),
              legend.position='right')
    
    # Conditional Plot
    if (legend=='No'){
      gg <- gg +
         theme(legend.position='none')
    }
    if (x_axes == 'No'){
      gg <- gg +
         # Update labels
         labs(x='') + 
         # Update format
         theme(axis.ticks.x=element_blank(),
               axis.text.x= element_blank())
    }
    if (y_axes == 'No'){
      gg <- gg +
         # Update labels
         labs(y='') + 
         # Update format
         theme(axis.text.y=element_blank(),
               axis.ticks.y= element_blank()) 
    }
    if (title=='Yes'){
      gg <- gg + 
        ggtitle(name) +
        theme(plot.title=element_text(size=18, hjust=0.5, face='bold'))
      if (y_axes != 'No'){
        gg <- gg + theme(plot.title=element_text(hjust=0.75))
      }
    }
    if(m !='F4'){
      gg <- gg + theme(
        axis.title.x=element_text(size=3),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank()
      )
    }
    
    
    return(gg)
  }
  
  if (legend=='Yes'){
    f0 <- marg_effect_sect(df, alpha, d, m='All', title='No', legend=legend, x_axes='No', y_axes=y_axes, custom_colors, custom_shapes) +
       theme(
         legend.title=element_text(size=18, face='bold'),
         legend.text =element_text(size=16),
         legend.key.size=unit(1, "cm"),
         legend.spacing.y=unit(0.1, "cm")
       )
    leg <- get_legend(f0)
  } else{
    leg <- NULL
  }
  
  f1 <- marg_effect_sect(df, alpha, d, m='F1', title='Yes', legend='No', x_axes='No', y_axes=y_axes, custom_colors, custom_shapes)
  f2 <- marg_effect_sect(df, alpha, d, m='F2', title='No', legend='No', x_axes='No', y_axes=y_axes, custom_colors, custom_shapes)
  f3 <- marg_effect_sect(df, alpha, d, m='F3', title='No', legend='No', x_axes='No', y_axes=y_axes, custom_colors, custom_shapes)
  f4 <- marg_effect_sect(df, alpha, d, m='F4', title='No', legend='No', x_axes=x_axes, y_axes=y_axes, custom_colors, custom_shapes)
  

  # Final plot arrangement
  res <- wrap_plots(
    list(f1, f2, f3, f4),
    ncol=1,
    heights=c(2, 4, 9, 18)
  )

  return(list(res=res, leg=leg))
}

# GG plotter for MF (marginal means - abs diff in flood levels by season)
marginal_means_s <- function(df, alpha, s, x_labs='No'){
  
  abs_diff_s <- function(df, alpha, s, binary='Yes', x_labs='Yes', legend='No', link_colour=c('darkgreen', 'grey')){
    
    
    if (binary=='Yes') {
      
      # FOOD GROUP PLOT
      source_f1 <- source_f0 %>% filter(season == s) %>% filter(!(group %in% c('DDS', 'MDD', 'Flesh foods', 'Eggs', 'Other vegetables', 'Nuts/seeds')))
      source_f1$value <- as.numeric(source_f1$value)
      # Prepare data
      f1 <- tufte_sort(source_f1, x='increase', y='value', group='group', method='tufte', min.space=0.1, max.space=1)
      f1 <- transform(f1, x=factor(x, levels=c(0, 1, 2, 3), labels=c('None','1SD','2SD', '>2SD')), y=round(y, 2))
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
      source_f1 <- source_f0 %>% filter(season == s) %>% filter(group %in% c('DDS'))
      source_f1$value <- as.numeric(source_f1$value)
      # Prepare data
      f1 <- source_f1 %>%
        dplyr::rename(x=increase, y=value) %>%
        mutate(yshift=if_else(group == 'Minimum dietary diversity', 0, 0.2), ypos=yshift+y) %>%
        select(group, yshift, x, y, ypos, sig)
      f1$ypos[f1$group == 'Dietary diversity scores*'] <- f1$ypos[f1$group == 'Dietary diversity scores*'] - spacing
      f1 <- transform(f1, x=factor(x, levels=c(0, 1, 2, 3), labels=c('None','1SD','2SD', '>2SD')), y=round(y, 2))
      # Get axes colors
      f1$a <- ifelse(f1$sig == 'p>0.05', link_colour[2], link_colour[1])
      val <- f1$a
    }
    
    # Plot
    gg <- ggplot(f1,aes(x=x,y=ypos, colour=sig)) +
      geom_line(aes(group=group, colour=sig)) +
      scale_color_manual(name='Difference from no change', values=val, limits=c('p>0.05', 'p<0.05')) +  # Set line colors manually
      labs(color='') +
      xlab('\nIncrease in flooding from \nseasonal average') +
      annotate(geom='segment', y=Inf, yend=Inf, x=-Inf, xend=Inf) +
      annotate(geom='segment', y=-Inf, yend=Inf, x=Inf, xend=Inf) +
      geom_point(colour='white',size=13) +
      geom_text(aes(label=y, colour=sig), size=5, family='Helvetica', show.legend=FALSE) +
      scale_y_continuous(name='', breaks=subset(f1, x==head(x,1))$ypos, labels=subset(f1, x==head(x,1))$group) + 
      theme_classic() +
      theme(axis.ticks=element_blank(),
            plot.title=element_text(hjust=0.5, family='Helvetica', face='bold'),
            axis.text=element_text(family='Helvetica', face='bold', size=16),
            axis.title=element_text(family='Helvetica', face='bold', size=18),
            plot.margin=unit(c(0,2.5,0,0), 'cm'),
            legend.position='bottom')
    
    if (x_labs=='No'){
      gg <- gg + theme(axis.title.x=element_blank())
    } 
    
    if(binary=='No'){
      name <- paste0(alpha, ' ', s)
      gg <- gg + 
        ggtitle(name) +
        xlab('') +
        coord_cartesian(ylim=c(min(f1$ypos)-0.1, max(f1$ypos)+0.1)) + # Zoom out for DD variables
        theme(axis.text.y=element_text(color=f1$a, family='Helvetica', face='bold'),
              axis.title.x=element_text(size=3),
              axis.text.x=element_blank(),
              plot.title=element_text(size=22, hjust=0.53, face='bold'))
    } else {
      gg <- gg + 
        theme(axis.text.y=element_text(color=axes_colours))
    }
    
    if (legend=='No'){
      gg <- gg + theme(legend.position="none")
    } else {
      gg <- gg + theme(
        legend.title=element_text(size=20, face='bold'),
        legend.text =element_text(size=18),
        legend.key.size=unit(1.2, "cm"),
        legend.spacing.y=unit(0.1, "cm")
      )
    }
    
    return (gg)
  }
  
  dd <- abs_diff_s(df=source_f0, alpha=alpha, s=s, binary='No', x_labs=x_labs, legend='No')
  fg <- abs_diff_s(df=source_f0, alpha=alpha, s=s, binary='Yes', x_labs=x_labs, legend='No')
  leg <- get_legend(abs_diff_s(df=source_f0, alpha=alpha, s=s, binary='Yes', x_labs=x_labs, legend='Yes'))
  
  res <- wrap_plots(dd, fg, ncol=1, heights=c(6, 23)) & theme(plot.margin=margin(5,5,5,5))
  
  return(list(res=res, leg=leg))
}

# GG plotter for MF (marginal means - abs diff flood levels by season and treatment)
marginal_means_s_t <- function(df, alpha, s, highlight=list(T, T, F, T, T, F), legend=F, x_tit=F){
  
  abs_diff_s <- function(df, s, alpha='', outcome,  legend='Yes', binary='Yes', highlight=F, x_labs='No', x_tit=F, y_labs='Yes'){
    
    # FILTER DATA
    source_f1 <- df %>% filter(season == s) %>% filter(group %in% c(outcome))  %>% filter(!(treat %in% c('HFP-Control')))
    source_f2 <- df %>% filter(season == s) %>% filter(group %in% c(outcome))  %>% filter(treat %in% c('HFP-Control'))
    
    # PROCESS FOR PLOTING
    f1 <- tufte_sort(source_f1, x='increase', y='value', group='treat', method='tufte', min.space=0.05, max.space=0.3)
    f1 <- transform(f1, x=factor(x, levels=c(0, 1, 2, 3), labels=c('None','1SD','2SD', '>2SD')), y=round(y, 2))
    f2 <- transform(source_f2, increase=factor(increase, levels=c(0, 1, 2, 3), labels=c('None','1SD','2SD', '>2SD')), y=round(value, 2))
    # Create Label Points for vertical lines (Difference between Treat & Control)
    control <- f1[f1$group == 'Control', ] 
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
      nudge <- 0.5
    }
    
    if(y_labs=='None'){
      y_lab <- ''
    } 
    
    # AESTETIC DATA
    f1 <- f1[order(f1$group, f1$x), ]   
    f1$sig <- source_f2$sig # Add sig back in (this represents the flood trend sig)
    f1$seas <- source_f2$season # Add season back in
    seas_col <- 'darkgreen' #custom_colors[[unique(f1$seas)]]
    f1$labels <- sprintf('%.2f', source_f2$value) # get geom labels
    # Get test line colors
    f1$test_col <- 'grey'
    f1$test_col[f1$sig == 'p<0.05'] <- seas_col
    # Get trend line colors (shift significance t be more intuitive visually)
    line_sig <- subset(f1, group =='Control')$sig
    if(all(line_sig==c('p>0.05', 'p>0.05', 'p>0.05', 'p>0.05'))){ # no sig
      f1$line_sig <- c('p>0.05', 'p>0.05', 'p>0.05', 'p>0.05')
    } else if (all(line_sig==c('p<0.05', 'p>0.05', 'p>0.05', 'p>0.05'))){ # 1st sig
      f1$line_sig <- c('p>0.05', 'p>0.05', 'p>0.05','p>0.05')
    } else if (all(line_sig==c('p<0.05', 'p<0.05', 'p>0.05','p>0.05'))){ # 2nd sig
      f1$line_sig <- c('p<0.05', 'p>0.05', 'p>0.05','p>0.05')
    } else if (all(line_sig==c('p<0.05', 'p<0.05', 'p<0.05','p>0.05'))){ # 3rd sig
      f1$line_sig <- c('p<0.05', 'p<0.05', 'p<0.05','p>0.05')
    } else if (all(line_sig==c('p>0.05', 'p<0.05', 'p<0.05', 'p>0.05'))){
      f1$line_sig <- c('p>0.05', 'p<0.05', 'p>0.05', 'p>0.05') 
    } else {
      f1$line_sig <- line_sig
    } 
    
    
    # PLOT
    gg <- ggplot(f1,aes(x=x,y=y)) +
        # Add geometry
        geom_line(aes(group=x, linetype=sig), colour=seas_col, show.legend=FALSE) +
        geom_line(aes(group=group, colour=line_sig), show.legend=FALSE) + # Points
        geom_point(aes(shape=group, colour=sig), size=4) +
        # Add text to geom
        geom_text(data=subset(f1, sig == 'p<0.05' & group == 'HFP'), aes(label=labels, y=max(f1$y)), colour=seas_col, size=5, nudge_y=nudge) +
        # Set color, shapes & line types
        scale_shape_manual(values=custom_shapes, name='Trial arm') +
        scale_linetype_manual(values=custom_lines, name='') +
        scale_color_manual(values=c('p>0.05'='grey', 'p<0.05'=seas_col), name='Difference between trial arms', limits=c('p>0.05', 'p<0.05')) +
        # Set axes
        scale_y_continuous(limits=y_lim, breaks=y_breaks) + 
        labs(title=paste0(alpha, ' ', outcome), y=y_lab, x='Increase in flooding', color='', shape='', linetype='') +  # Modify axes & legend labels
        # Format borders
        annotate(geom='segment', y=Inf, yend=Inf, x=-Inf, xend=Inf) +
        annotate(geom='segment', y=-Inf, yend=Inf, x=Inf, xend=Inf) +
        guides(shape=guide_legend(order=1))
    
    # Fix legend
    if(x_labs=='Yes'){
      
      gg2 <- gg + theme(axis.title=element_text(family='Helvetica', face='bold', size=15),
                        axis.text=element_text(family='Helvetica', face='bold', size=14),
                        axis.title.y=element_text(margin=margin(r=15)), 
                        axis.title.x=element_text(margin=margin(t=15)), # shift away from x_labs
                        axis.ticks.x=element_blank(),
                        legend.title=element_text(size=20, face='bold'),
                        legend.text=element_text(size=18),
                        legend.key.size=unit(1.2, "cm"),
                        legend.spacing.y=unit(0.1, "cm"),
                        plot.title=element_text(size=16, hjust=0.5, family='Helvetica', face='bold'),
                        legend.position='bottom')
      if(legend=='No'){
        
        gg2 <- gg2 + theme(legend.position='none')
        
      }
      
      if(x_tit==F){
        gg2 <- gg2 + theme(axis.title.x=element_blank())
      }
      
    } else if(x_labs=='No'){
      
      gg2 <- gg + theme(axis.title=element_text(family='Helvetica', face='bold', size=15),                       axis.title.x=element_blank(), 
                         axis.text=element_text(family='Helvetica', face='bold', size=14),
                         axis.title.y=element_text(margin=margin(r=15)), 
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         legend.title=element_text(size=20, face='bold'),
                         legend.text=element_text(size=18),
                         legend.key.size=unit(1.2, "cm"),
                         legend.spacing.y=unit(0.1, "cm"),
                         plot.title=element_text(size=16, hjust=0.5, family='Helvetica', face='bold'),
                         legend.position='none')
    }
    
    # Highlight border if requested
    if (highlight == T) {
      gg2 <- gg2 + theme(panel.border=element_rect(colour="red",linewidth=3.5, fill=NA))
    } else {
      gg2 <- gg2 + theme(panel.border=element_rect(colour="black",linewidth=0.8, fill=NA))
    }
    
    return(gg2)
    
  }
  
  # Get season specs for each outcome
  p0 <- abs_diff_s(df=source_f0, s=s, alpha='I.', outcome='Legumes', highlight=F, legend='Yes', binary='No', x_labs='Yes', x_tit=T, y_labs='Yes')
  p1 <- abs_diff_s(df=source_f0, s=s, alpha='I.', outcome='DDS', highlight=highlight[1], legend='No', binary='No', x_labs='No', x_tit=F, y_labs='Yes')
  p2 <- abs_diff_s(df=source_f0, s=s, alpha='II.', outcome='Dairy', highlight=highlight[2], legend='No', binary='Yes', x_labs='No', x_tit=F, y_labs='Yes')
  p3 <- abs_diff_s(df=source_f0, s=s, alpha='III.', outcome='DGLV', highlight=highlight[3], legend='No', binary='Yes', x_labs='No', x_tit=F, y_labs='Yes')
  p4 <- abs_diff_s(df=source_f0, s=s, alpha='IV.', outcome='Vit. A-rich foods',  highlight=highlight[4], legend='No', binary='Yes', x_labs='No', x_tit=F, y_labs='None')
  p5 <- abs_diff_s(df=source_f0, s=s, alpha='V.', outcome='Other fruits', highlight=highlight[5], legend='No', binary='Yes', x_labs='Yes', x_tit=x_tit, y_labs='Yes')
  p6 <- abs_diff_s(df=source_f0, s=s, alpha='VI.', outcome='Legumes', highlight=highlight[6], legend='No', binary='Yes', x_labs='Yes', x_tit=x_tit, y_labs='None')
  
  output <- wrap_plots(list(p1, p2, p3, p4, p5, p6), ncol=2) & theme(plot.margin=margin(5,5,5,5))
  title <- ggplot() + ggtitle(paste0(alpha, ' ', s)) + theme_void() + theme(plot.title=element_text(size=24, face="bold",hjust=0.5))
  legend <- get_legend(p0)
  
  res <- wrap_plots(list(title, output), ncol=1, heights=c(0.15, 20))
  
  return(list(res=res, leg=legend))
  
}

# Extract legend from ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Tufte formater  (make sure reshape2 is used)
tufte_sort <- function(df, x='year', y='value', group='group', method='tufte', min.space=0.05, max.space=0.1) {
  ## First rename the columns for consistency
  ids <- match(c(x, y, group), names(df))
  df <- df[,ids]
  names(df) <- c('x', 'y', 'group')
  
  ## Expand grid to ensure every combination has a defined value
  tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
  tmp <- merge(df, tmp, all.y=TRUE)
  df <- mutate(tmp, y=ifelse(is.na(y), 0, y))
  
  ## Cast into a matrix shape and arrange by first column
  tmp <- reshape2::dcast(df, group ~ x, value.var='y')
  ord <- order(tmp[,2])
  tmp <- tmp[ord,]
  
  min.space <- min.space*diff(range(tmp[,-1]))
  max.space <- max.space*diff(range(tmp[,-1]))
  yshift <- numeric(nrow(tmp))
  ## Start at 'bottom' row
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
  tmp <- reshape2::melt(tmp, id=c('group', 'yshift'), variable.name='x', value.name='y')
  ## Store these gaps in a separate variable so that they can be scaled ypos=a*yshift + y
  
  tmp <- transform(tmp, ypos=y + scale*yshift)
  return(tmp)
  
}

#### MAIN CODE ####

# Load data
load(paste0('main_data.RData'))

# Set path
setwd(paste0(git_path, '/Outputs/'))

# SF2: DESC - Temporal distribution of flooding & diets, pooled ####

# Load data
source_f0 <- read.xlsx(xlsxFile='II. Tables/desc_trial_rounds.xlsx')
# Clean data
source_f0 <- source_f0[, grepl('Round|Treatment|PROB|COEF', names(source_f0))]
source_f0 <- melt(source_f0, id.vars=c('Round', 'Treatment'))
source_f0 <- source_f0 %>% filter(!(Round %in% c('Baseline'))) # Remove non probability variables
f1 <- cbind(source_f0, str_split(source_f0$Round, '-', simplify=TRUE))
colnames(f1)[5:6] <- c('year', 'month')
# f1$Round <- factor(f1$Round, levels=c('Baseline', sort(unique(f1$Round))[-25])) # Factor rounds

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
f1$variable <- gsub('dd10r_score_m_COEF', '(A) DDS', f1$variable)
f1$variable <- gsub('dd10r_min_m_PROB', '(B) MDD', f1$variable)
f1$variable <- gsub('dd10r_starch_PROB', '(C) Starchy staples', f1$variable)
f1$variable <- gsub('dd10r_flesh_PROB', '(D) Flesh foods', f1$variable)
f1$variable <- gsub('dd10r_dairy_PROB', '(E) Dairy products', f1$variable)
f1$variable <- gsub('dd10r_eggs_PROB', '(F) Eggs', f1$variable)
f1$variable <- gsub('dd10r_dglv_PROB', '(G) DGLV', f1$variable)
f1$variable <- gsub('dd10r_vita_PROB', '(H) Vitamin A-rich foods', f1$variable)
f1$variable <- gsub('dd10r_othv_PROB', '(I) Other vegetables', f1$variable)
f1$variable <- gsub('dd10r_othf_PROB', '(J) Other fruits', f1$variable)
f1$variable <- gsub('dd10r_legume_PROB', '(K) Legumes', f1$variable)
f1$variable <- gsub('dd10r_nuts_PROB', '(L) Nuts & Seeds', f1$variable)
f1$variable <- gsub('Flood_1Lag_PROB', '(M) Percent Flooded', f1$variable)
f1$variable<- factor(f1$variable, levels=unique(f1$variable))
f1$Treatment <- ifelse(f1$Treatment==0, 'Control', 'HFP')
f1$variable <- factor(f1$variable, levels=c('(A) DDS', '(B) MDD-W', 
                                              '(C) Starchy staples', '(D) Flesh foods','(E) Dairy products',
                                              '(F) Eggs', '(G) DGLV', '(H) Vitamin A-rich foods', '(I) Other vegetables', 
                                              '(J) Other fruits', '(K) Legumes', '(L) Nuts & Seeds',
                                              '(M) Percent Flooded')) # Factor rounds

# Subset the data frame to keep only columns with names not containing the character(s) to remove
(sf2 <- ggplot(f1, aes(x=Round, y=as.numeric(value), color=as.factor(Treatment), group=as.factor(Treatment))) +
    geom_line() +
    labs(x='', y='', color='variable') +
    scale_x_discrete(breaks=c(unique(f1$Round)[seq(1, length(unique(f1$Round)), by=3)]), 
                     guide=guide_axis(angle=45)) + # Rotate  axes labels
    scale_color_manual(values=c('Control'='#3388f7', 'HFP'='#b51731'), name='Trial arm') + 
    facet_wrap(~ variable, nrow=NULL, ncol=NULL, scales='free_y') +
    ggh4x::facetted_pos_scales(y=list(
      variable == '(A) DDS' ~ scale_y_continuous(limits=c(0, 10)),
      variable == '(M) Percent Flooded' ~ scale_y_continuous(limits=c(0, 10)),
      TRUE ~ scale_y_continuous(limits=c(0, 100))
      )) + theme_bw())


ggsave('III. Figures/SF2_time_series.png', sf2, width=35, height=25, units='cm')

# MF2: DESC - Spatial flood distribution across seasons, by cluster ####

# NEED API's (https://www.appsilon.com/post/r-ggmap)
# ggmap::register_google(key='AIzaSyCVzPwqMVzz-f374mq0b-6UfsLXmMCFIU8', write=TRUE) # Use at own risk, it is connected to billing address
ggmap::register_stadiamaps(key='f2f7765b-7259-42c9-a46d-fc1a61dc4375')

# Clean flood data for visualizing
df[df$treatment == 0, 'treatment'] <- 'Control'
df[df$treatment == 1, 'treatment'] <- 'HFP'
flood_ALL <- aggregate(perc_flooded_c ~ c_code+treatment, data=df, FUN=mean) #  pooled
flood_SEASON <-cast(df, c_code+treatment~season_DD, mean, value='perc_flooded_c') # by season
# Merge datasets and send to long format for seasonal facets
sdf_season <- merge(cluster_shp, flood_SEASON, by='c_code') %>%
  pivot_longer(cols=c(`Jan/Feb`, `Mar/Apr`, `May/Jun`,
                        `Jul/Aug`, `Sep/Oct`, `Nov/Dec`), 
               names_to ='season_flood', 
               values_to='perc_flooded_c') %>%
  mutate(season_flood=factor(season_flood, 
                               levels=c('Jan/Feb', 'Mar/Apr', 'May/Jun', 
                                          'Jul/Aug', 'Sep/Oct', 'Nov/Dec')))
# Get seasonal stats for annotations
stats_df <- season_means %>%
  mutate(
    label=paste0(
      '<b>Mean</b> ', sprintf('%5.2f%%', flood_mean*100), '<br>',
      '<b>SD</b>   ', sprintf('%5.2f%%', flood_sd*100), '<br>',
      '<b>Min</b>  ', sprintf('%5.2f%%', flood_min*100), '<br>',
      '<b>Max</b>  ', sprintf('%5.2f%%', flood_max*100)
    )
  )
# NB: Fiddle with the mapper flood limits based on min, max, etc.
summary(sdf_season)

# Get a buffered bounding box for the basemap
poly_box <- st_as_sfc(st_bbox(cluster_shp))
buffered_polygon <- st_buffer(poly_box, dist=1000)  # 1km=1000 meters
bbox <- as.list(st_bbox(buffered_polygon))
# NB: ggmap uses long/lat (NOT lat/long)
basemap <- get_map(c(left=bbox$xmin, 
                     bottom=bbox$ymin, 
                     right=bbox$xmax, 
                     top=bbox$ymax), 
                   source='stadia', maptype='stamen_terrain') #_background

# Get seasonal maps
mf2 <- mapper(basemap, sdf_season, stats_df)

ggsave(paste0('III. Figures/MF2_Seasonal_Flood.png'), mf2, width=20, height=15, units='cm',   bg='white')


# MF3: RES - Marginal effects of interaction model for each DD outcome (1SD flood) ####

source_f0 <- read.xlsx(xlsxFile='III. Figures/Visuals.xlsx', sheet='R_Rel_Diff')

# Get full plot for each dietary outcome
f0 <- marg_effect_full(source_f0, alpha='(A)', d='DDS', legend='Yes', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes)$leg
f1 <- marg_effect_full(source_f0, alpha='(A)', d='DDS', legend='Yes', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes)$res
f2 <- marg_effect_full(source_f0, alpha='(B)', d='MDD', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes)$res
f3 <- marg_effect_full(source_f0, alpha='(C)', d='Flesh foods', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes)$res
f4 <- marg_effect_full(source_f0, alpha='(D)', d='Dairy', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes)$res
f5 <- marg_effect_full(source_f0, alpha='(E)', d='Eggs', legend='No', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes)$res
f6 <- marg_effect_full(source_f0, alpha='(F)', d='DGLV', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes)$res
f7 <- marg_effect_full(source_f0, alpha='(G)', d='Vit. A-rich foods', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes)$res
f8 <- marg_effect_full(source_f0, alpha='(H)', d='Other vegetables', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes)$res
f9 <- marg_effect_full(source_f0, alpha='(I)', d='Other fruits', legend='No', x_axes='Yes', y_axes='Yes', custom_colors, custom_shapes)$res
f10 <- marg_effect_full(source_f0, alpha='(J)', d='Legumes', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes)$res
f11 <- marg_effect_full(source_f0, alpha='(K)', d='Nuts/seeds', legend='No', x_axes='Yes', y_axes='No', custom_colors, custom_shapes)$res

# Wrap plots with legend
leg_plot <- plot_grid(f0, ncol=1)
output <- list(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, leg_plot)
mf3 <- wrap_plots(output, ncol=4) & theme(plot.title.position='plot', plot.margin=margin(5,5,5,5))

# Save
ggsave(paste0('III. Figures/MF3_Marginal_effects.png'), mf3, width=45, height=45, units='cm')


# MF4: RES - Predicted measures of DD outcomes for different flood levels across seasons (pooled Trial arms) ####

source_f0 <- read.xlsx(xlsxFile='III. Figures/Visuals.xlsx', sheet='R_Abs_Flood_Levels')

# Adapt other veg or visualization purposes
source_f0$sig[source_f0$group=='Other vegetables'] <- 'p>0.05'
source_f0$group[source_f0$group=='Dietary diversity scores*'] <- 'Dietary diversity scores'

# Get seasonal plots
f0 <- marginal_means_s(source_f0, '(A)', 'Jan/Feb', x_labs='No')$leg
f1 <- marginal_means_s(source_f0, '(A)', 'Jan/Feb', x_labs='No')$res
f2 <- marginal_means_s(source_f0, '(B)', 'Mar/Apr', x_labs='No')$res
f3 <- marginal_means_s(source_f0, '(C)', 'May/Jun', x_labs='No')$res
f4 <- marginal_means_s(source_f0, '(D)', 'Jul/Aug', x_labs='Yes')$res
f5 <- marginal_means_s(source_f0, '(E)', 'Sep/Oct', x_labs='Yes')$res
f6 <- marginal_means_s(source_f0, '(F)', 'Nov/Dec', x_labs='Yes')$res

# Wrap plots with legend
leg_plot <- plot_grid(f0, ncol=1)
output <- wrap_plots(list(f1, f2, f3, f4, f5, f6), ncol=3) & theme(plot.margin=margin(r=5, l=5, b=50, t=5))
mf4 <- wrap_plots(list(output, leg_plot), ncol=1, heights=c(20, 1)) & theme(plot.margin=margin(r=20, l=5, b=5, t=5))

# Save 
ggsave(paste0('III. Figures/MF4_Marginal_means_season.png'), mf4, width=45, height=40, units='cm')

# MF5: RES - Predicted measures of DD outcomes for different flood levels across seasons and Trial arms ####

source_f0 <- read.xlsx(xlsxFile='III. Figures/Visuals.xlsx', sheet='R_Abs_Flood_Treat_Levels')
# source_f0$group[source_f0$group=='DDS'] <- 'Dietary diversity scores'

# Get seasonal plots
f0 <- marginal_means_s_t(source_f0, alpha='(A)', s='Jan/Feb', highlight=list(F,F,F,T,F,T), legend=T, x_tit=T)$leg
f1 <- marginal_means_s_t(source_f0, alpha='(A)', s='Jan/Feb', highlight=list(F,F,F,T,F,T), legend=F, x_tit=F)$res
f2 <- marginal_means_s_t(source_f0, alpha='(B)', s='Mar/Apr', highlight=list(T,T,F,F,F,T), legend=F, x_tit=F)$res
f3 <- marginal_means_s_t(source_f0, alpha='(C)', s='May/Jun', highlight=list(T,F,F,F,F,T), legend=F, x_tit=F)$res
f4 <- marginal_means_s_t(source_f0, alpha='(D)', s='Jul/Aug', highlight=list(F,F,T,F,F,T), legend=F, x_tit=T)$res
f5 <- marginal_means_s_t(source_f0, alpha='(E)', s='Sep/Oct', highlight=list(F,F,F,F,F,F), legend=F, x_tit=T)$res
f6 <- marginal_means_s_t(source_f0, alpha='(F)', s='Nov/Dec', highlight=list(F,T,F,F,F,F), legend=F, x_tit=T)$res

# Wrap plots with legend
leg_plot <- plot_grid(f0, ncol=1)
output <- wrap_plots(list(f1, f2, f3, f4, f5, f6), ncol=3) & theme(plot.margin=margin(r=5, l=5, b=10, t=5))
mf5 <- wrap_plots(list(output, leg_plot), ncol=1, heights=c(20, 1))

# Save
ggsave(paste0('III. Figures/MF5_Marginal_means_season-trial.png'), mf5, width=50, height=35, units='cm')
