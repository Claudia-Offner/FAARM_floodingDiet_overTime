### ------------------------------------------------------------------------ ### 
### Model selection
### ------------------------------------------------------------------------ ### 

#### DEPENDENCIES ####

# Override emmeans internal theme to avoid ggplot2 version conflict
# theme_emm <- function(...) ggplot2::theme_bw(...)
# assignInNamespace("theme_emm", theme_emm, ns = "emmeans")

# Function to fit lme/glmer models
fit_model <- function(df, outcome, type, exposure) {
  
  df$treatment <- factor(df$treatment, levels = c(0, 1), labels = c("Control", "HFP"))
  
  if (exposure == 'cat') {
    df$Flood_1Lag <- factor(df$Flood_1Lag, levels = c(0, 1, 2, 3), 
                            labels = c("None", "1SD", "2SD", ">2SD"))
  }
  
  fixed <- ' ~ Flood_1Lag*season_flood*treatment + dd10r_score_m_BL + ramadan + g_2h_BL + quint2_BL'
  
  if (type == 'lme') {
    model <- lme(
      fixed    = as.formula(paste0(outcome, fixed)),
      random   = list(wcode = (~1|season_id), c_code = (~1)),
      weights  = varIdent(form = ~ 1 | wdiet_wt),
      na.action = na.omit,
      data     = df
    )
  } else if (type == 'glmer') {
    model <- glmer(
      formula = as.formula(paste0(outcome, fixed, '+ (1 + wcode|season_id) + (1 | c_code)')),
      weights = wdiet_wt,
      data    = df,
      family  = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa")
    )
  }
  
  model
}

# Function to build plots from rds files
build_plots <- function(model, outcome, name, type, exposure) {
  
  if (type == 'lme') {
    ass_plots <- plot_model(model, type = "diag")[1:3]
    suffixes  <- c("(A.I) WDDS", "(A.II) WDDS", "(A.III) WDDS")
    
    ass_plots <- lapply(seq_along(ass_plots), function(i) {
      ass_plots[[i]] + 
        labs(title = suffixes[i]) +
        theme(plot.title    = element_text(hjust = 0.5, size = 14, face = 'bold'),
              plot.subtitle = element_blank(),
              strip.text    = element_blank(),
              strip.background = element_blank())
    })
    
  } else {
    ass_plots <- plot_model(model, type = "diag")[1]
    
    ass_plots <- lapply(ass_plots, function(p) {
      p + labs(title = name) +
        theme(plot.title        = element_text(hjust = 0.5, size = 14, face = 'bold'),
              strip.text        = element_blank(),
              strip.background  = element_blank())
    })
  }

  # Estimated-marginal-means interaction plot
  if (exposure == 'cat') {
    fib.rg <- ref_grid(model, trans = 'response')
  } else {
    fib.rg <- ref_grid(model, at = list(Flood_1Lag = c(0, 1, 2, 3)))
  }
  
  em_plot <- emmip(fib.rg, treatment ~ Flood_1Lag | season_flood,
                   style = 'factor', CIs = TRUE,
                   linearg = list(linewidth = 0.8),
                   dotarg  = list(size = 3),
                   CIarg   = list(linetype = 'solid', linewidth = 0.8, 
                                  alpha = 1, show.legend = FALSE),
                   xlab = "Increase in flooding",
                   tlab = "Trial arm")
  
  em_plot <- em_plot +
    aes(shape = treatment, linetype = treatment, col = season_flood) +
    scale_color_manual(values = custom_colors, name = 'Season') +
    scale_shape_manual(values = c(15, 17), name = 'Trial arm') +
    scale_linetype_manual(values = c('dashed', 'solid'), name = 'Trial arm') +
    labs(title = name, color = "Season", shape = "Trial arm", linetype = "Trial arm") +
    facet_wrap(~ season_flood, labeller = labeller(season_flood = label_value)) +
    theme_bw() +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 20, face = 'bold'),
      strip.text   = element_text(size = 16, face = 'bold'),
      legend.title = element_text(size = 16, face = 'bold'),
      legend.text  = element_text(size = 14),
      legend.position  = "right",
      legend.key.size  = unit(1, 'cm'),
      axis.title   = element_text(size = 14, face = 'bold'),
      axis.text    = element_text(size = 14),
      axis.text.x  = element_text(angle = 45, hjust = 1)
    )
  
  list(ass_plots = ass_plots, em_plot = em_plot)
}


#### MAIN CODE (2.16 hours) ####

# Load data
load('main_data.RData')
df$Flood_1Lag <- df$Flood_SThresh


# 1. Sensitivity analysis: Run models ####

# Define all outcomes
outcomes <- list(
  list(outcome = 'dd10r_score_m', name = '(A) WDDS',                type = 'lme'),
  list(outcome = 'dd10r_min_m',   name = '(B) MDD-W',                 type = 'glmer') ,
  list(outcome = 'dd10r_dairy',   name = '(C) Dairy',               type = 'glmer'),
  list(outcome = 'dd10r_flesh',   name = '(D) Flesh foods',         type = 'glmer'),
  list(outcome = 'dd10r_eggs',    name = '(E) Eggs',                type = 'glmer'),
  list(outcome = 'dd10r_dglv',    name = '(F) DGLV',                type = 'glmer'),
  list(outcome = 'dd10r_vita',    name = '(G) Vitamin A-rich Foods', type = 'glmer'),
  list(outcome = 'dd10r_othv',    name = '(H) Other vegetables',    type = 'glmer'),
  list(outcome = 'dd10r_othf',    name = '(I) Other fruits',        type = 'glmer'),
  list(outcome = 'dd10r_legume',  name = '(J) Legumes',             type = 'glmer'),
  list(outcome = 'dd10r_nuts',    name = '(K) Nuts and seeds',      type = 'glmer')
)

models_cont <- lapply(outcomes, function(o)
  fit_model(df, o$outcome, o$type, exposure = 'cont'))

models_cat  <- lapply(outcomes, function(o)
  fit_model(df, o$outcome, o$type, exposure = 'cat'))

names(models_cont) <- names(models_cat) <- sapply(outcomes, `[[`, 'outcome')

# Save
saveRDS(models_cont, 'Sensitivity Results/models_cont.rds')
saveRDS(models_cat,  'Sensitivity Results/models_cat.rds')

# Load (skips re-fitting entirely)
models_cont <- readRDS('Sensitivity Results/models_cont.rds')
models_cat  <- readRDS('Sensitivity Results/models_cat.rds')

# 2. Sensitivity analysis: Build plots ####

plots <- mapply(
  function(m_cont, m_cat, o) {
    ass  <- build_plots(m_cont, o$outcome, o$name, o$type, exposure = 'cont')$ass_plots
    em   <- build_plots(m_cat,  o$outcome, o$name, o$type, exposure = 'cat')$em_plot
    list(ass_plots = ass, em_plot = em)
  },
  m_cont = models_cont,
  m_cat  = models_cat,
  o      = outcomes,
  SIMPLIFY = FALSE
)
names(plots) <- sapply(outcomes, `[[`, 'outcome')

# Assumption figure
all_ass_plots <- unlist(lapply(plots, `[[`, 'ass_plots'), recursive = FALSE)
all_ass_plots <- c(all_ass_plots[1:3], list(plot_spacer(), plot_spacer()), 
                   all_ass_plots[4:length(all_ass_plots)])
all_ass_plots <- wrap_plots(all_ass_plots, ncol = 5)

# Em_plot figure
all_em_plots <- lapply(plots, `[[`, 'em_plot')
all_em_plots <- lapply(all_em_plots, function(p) {
  p + theme(legend.position = "none")
})
combined_em   <- wrap_plots(all_em_plots, ncol = 4)

# Save legend
legend <- get_legend(plots[[1]]$em_plot)


#### EXPORT ####

ggsave('Figures/SF3_all_assumptions.png', all_ass_plots, width = 15, height = 9, dpi = 300)
ggsave('Figures/SF4_all_sensitivity.png', combined_em, width = 25, height = 15, dpi=300)
ggsave('Figures/SF4_all_sensitivity_legend.png', ggdraw(legend), width = 3, height = 6, dpi = 300)

