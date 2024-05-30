################################################################################
#### ASSUMPTION TESTS #### 
################################################################################

#### IMPORTANT - set github credentials
# gitcreds::gitcreds_set()

#### IMPORTANT - set file paths to folder locations
setwd('C:/Users/offne/OneDrive - London School of Hygiene and Tropical Medicine/2. Research/B. FAARM/3. Analysis/')

## Suppress warnings & turn off scientific notation
options(warn=-1) # 0 to turn back on
options(scipen=999)

#### IMPORTANT - Run R0_Data_formatting first

# LIBRARY ####
library(sjPlot)
library(car)
library(gridExtra)


# FUNCTIONS ####

custom_colors <- c('Overall'="#2b2a2a",
                   'Jan/Feb'="#ce1126", 
                   'Mar/Apr'="#f2609e", 
                   'May/Jun'="#e66300", #ebac23
                   'Jul/Aug'="#80b517",
                   'Sep/Oct'="#057dcd",
                   'Nov/Dec'="#7849b8")
outcomes_bin <- c("dd10r_min_m", "dd10r_flesh", "dd10r_dairy", "dd10r_eggs",
                  "dd10r_dglv", "dd10r_vita", "dd10r_othv", "dd10r_othf",
                  "dd10r_legume", "dd10r_nuts") 

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
    
    
  } else if (type=='glmer'){
    
    model <- glmer(
      formula=as.formula(paste0(outcome, fixed, '+ (1 + wcode|season_id) + (1 | c_code)')),
      weights = wdiet_wt,
      data = df,
      family = binomial(link="logit"), # extracts binomial regression on logit scale
      control=glmerControl(optimizer="bobyqa") # Removes non-convergence warnings
    )
    
  }
  
  # Check assumptions
  ass_plots <- plot_model(model, type = "diag")
  
  # Run sensitivity analysis for comparison
  if (exposure=='cat'){
    fib.rg = ref_grid(model) # for linear flood
  } else{
    fib.rg = ref_grid(model, at=list(Flood_1Lag=c(0, 1, 2, 3))) # for linear flood
  }
  
  (em_plot <- emmip(fib.rg, treatment ~ Flood_1Lag | season_flood, style='factor', CIs=TRUE, col = c("black"),
                    linearg = list(), dotarg = list(size = 2), CIarg = list(linetype='solid', alpha = 1, show.legend = FALSE),
                    xlab = "Increase in flooding",  # Modify x-axis label
                    tlab = "Trial-arm"))
  # Aesthetics 
  (em_plot <- em_plot + 
          aes(shape=treatment, linetype=treatment, col=season_flood) +
          scale_color_manual(values=custom_colors, name='Season')+
          scale_shape_manual(values=c(15, 17), name='Trial-arm') +
          scale_linetype_manual(values=c('dashed', 'solid'), name='Trial-arm')+
          labs(title=name, color = "Season", shape = "Trial-arm", linetype="Trial-arm") + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)))  # Center the title

  
  return(list(ass_plots, em_plot))
}


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


#### 1. Sensitivity Analysis ####

# LINEAR MIXED EFFECTS MODELS
sens_figures(df, 'dd10r_score_m', 'WDDS', type='lme')

# LOGISTIC MIXED EFFECTS MODELS
for (b in outcomes_bin) {
  
  sens_figures(df, b, type='glmer')
  
}


