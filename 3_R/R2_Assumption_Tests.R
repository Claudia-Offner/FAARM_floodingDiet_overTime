### ------------------------------------------------------------------------ ### 
### Model selection
### ------------------------------------------------------------------------ ### 

# Clear environment
rm(list = ls())

### IMPORTANT - set file paths to folder locations
data_path <- 'C:/Users/claer14/OneDrive - University of Cambridge/V. Other/Flooding-Diets-HFP/Data/'
git_path  <- 'C:/Users/claer14/Documents/GitHub/FAARM_floodingDiet_overTime/3_R'
setwd(git_path)

#### DEPENDENCIES ####
library(sjPlot); library(car); library(gridExtra); library(glmmTMB)

source('R0_Dependencies.R')

# Function to 
sens_check <- function(df, outcome, name, type, exposure) {
  
  df$treatment <- factor(df$treatment, levels=c(0, 1), labels=c("Control", "HFP"))
  
  if (exposure=='cat'){
    df$Flood_1Lag <- factor(df$Flood_1Lag, levels=c(0, 1, 2, 3), labels=c("None","1SD","2SD", ">2SD"))
  }
  
  # Set the main formula
  fixed <- ' ~ Flood_1Lag*season_flood*treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL'
  
  # Run model depending on dtype
  if(type=='lme'){
    
    model <- lme(
      fixed = as.formula(paste0(outcome, fixed)),
      random = list(wcode = (~1|season_id), c_code = (~1)), # Random effects
      weights = varIdent(form = ~ 1 | wdiet_wt),  # Adding weights
      na.action = na.omit,  # Handle missing data using na.omit
      data = df
    )
    
    # Check assumptions
    (ass_plots <- plot_model(model, type = "diag"))
    
  } else if (type=='glmer'){
    
    model <- glmer(
      formula=as.formula(paste0(outcome, fixed, '+ (1 + wcode|season_id) + (1 | c_code)')),
      weights = wdiet_wt,
      data = df,
      family = binomial(link="logit"), # extracts binomial regression on logit scale
      control=glmerControl(optimizer="bobyqa") # Removes non-convergence warnings
    )
    
    # Check assumptions
    (ass_plots <- plot_model(model, type = "diag")[1])
  }
  
  # Run sensitivity analysis for comparison
  if (exposure=='cat'){
    fib.rg = ref_grid(model, trans='response') # for non-linear flood
  } else{
    fib.rg = ref_grid(model, at=list(Flood_1Lag=c(0, 1, 2, 3))) # for linear flood
  }
  summary(fib.rg, infer = c(TRUE, TRUE)) # get means (whole interaction)
  # summary(emmeans(fib.rg, pairwise ~ Flood_1Lag*season_flood)$emmeans, infer = c(TRUE, TRUE)) # get means (partial interaction)
  # summary(emmeans(fib.rg, pairwise ~ Flood_1Lag*season_flood)$contrasts, infer = c(TRUE, TRUE)) # get contrasts (partial interaction)
  # summary(emmeans(fib.rg, pairwise ~ Flood_1Lag | season_flood), infer = c(TRUE, TRUE)) # get contrasts (partial interaction)
  # summary(emmeans(fib.rg, pairwise ~ Flood_1Lag | treatment), infer = c(TRUE, TRUE)) # get contrasts (partial interaction)
  # summary(emmeans(fib.rg, pairwise ~ treatment | Flood_1Lag | season_flood, trans = "response"), infer = c(TRUE, TRUE)) # get contrasts (partial interaction)
  # (ame1_cont <- summary(contrast(fib.rg, "pairwise", by = c("Flood_1Lag", "season_flood")), infer = c(TRUE, TRUE)))
  
  (em_plot <- emmip(fib.rg, treatment ~ Flood_1Lag | season_flood, style='factor', CIs=TRUE, col = c("black"),
                    linearg = list(), dotarg = list(size = 3), CIarg = list(linetype='solid', alpha = 1, show.legend = FALSE),
                    xlab = "Increase in flooding",  # Modify x-axis label
                    tlab = "Trial arm"))
  # Aesthetics 
  (em_plot <- em_plot + 
      aes(shape=treatment, linetype=treatment, col=season_flood) +
      scale_color_manual(values=custom_colors, name='Season')+
      scale_shape_manual(values=c(15, 17), name='Trial arm') +
      scale_linetype_manual(values=c('dashed', 'solid'), name='Trial arm')+
      labs(title=name, color = "Season", shape = "Trial arm", linetype="Trial arm") + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size=20, face='bold'),  # center title
            legend.title = element_text(size=16, face='bold'), # legend text
            legend.text = element_text(size=14), # legend text
            legend.position = "right",       # legend position
            legend.key.size = unit(1, 'cm'), # legend size
            axis.title = element_text(size=14, face='bold'),
            axis.text = element_text(size=12),)  # Remove x-axis title
  )  
  
  
  return(list(ass_plots, em_plot))
}

# Function to
sens_figures <- function(df, outcome, name, type){
  
  if(type=='glmer'){
    
    # ASSUMPTIONS
    output <- sens_check(df, outcome, name, type='glmer', exposure='cont')
    # Get assumption figures
    assumptions <- output[[1]]
    ggsave(paste0('Sensitivity_Analysis/Model_Assumptions/', outcome, "_assumptions.png"), 
           assumptions[[1]], width=40, height=20, units='cm')
    
    # CATEGORICAL SENSITIVITY
    output <- sens_check(df, outcome, name, type='glmer', exposure='cat')
    # Get sensitivity figures
    sensitivity <- output[[2]]
    ggsave(paste0('Sensitivity_Analysis/Model_Sensitivity/', outcome, "_sens.png"),
           sensitivity, width=20, height=15, units='cm')
    
  } else{
    
    # ASSUMPTIONS
    output <- sens_check(df, outcome, name, type='lme', exposure='cont')
    # Get assumption figures
    assumptions <- output[[1]]
    assumptions <- grid.arrange(assumptions[[1]], assumptions[[2]], assumptions[[3]], ncol=3)
    ggsave(paste0('Sensitivity_Analysis/Model_Assumptions/', outcome, "_assumptions.png"), 
           assumptions, width=30, height=10, units='cm')
    
    # CATEGORICAL SENSITIVITY
    output <- sens_check(df, outcome, name, type='lme', exposure='cat')
    # Get sensitivity figures
    sensitivity <- output[[2]]
    ggsave(paste0('Sensitivity_Analysis/Model_Sensitivity/', outcome, "_sens.png"),
           sensitivity, width=20, height=15, units='cm')
    
    
  }
  
}

#### MAIN CODE ####

# Load data
load(paste0('main_data.RData'))

# Select correct flood exposure
df$Flood_1Lag <- df$Flood_SThresh
level <- flood_cont_levels


#### 1. Sensitivity Analysis ####

# LINEAR MIXED EFFECTS MODELS
sens_figures(df, 'dd10r_score_m', 'WDDS', type='lme')

# LOGISTIC MIXED EFFECTS MODELS
sens_figures(df, 'dd10r_min_m', "MDD", type='glmer')
sens_figures(df, 'dd10r_dairy', "Dairy", type='glmer')
sens_figures(df, 'dd10r_flesh', "Flesh foods", type='glmer')
sens_figures(df, 'dd10r_eggs', "Eggs", type='glmer')
sens_figures(df, 'dd10r_dglv', "DGLV", type='glmer')
sens_figures(df, 'dd10r_vita', "Vitamin A-rich Foods", type='glmer')
sens_figures(df, 'dd10r_othv', "Other vegetables", type='glmer')
sens_figures(df, 'dd10r_othf', "Other fruits", type='glmer')
sens_figures(df, 'dd10r_legume', "Legumes", type='glmer')
sens_figures(df, 'dd10r_nuts', "Nuts and seeds", type='glmer')




