
#### viz for 299 main effects ####
library(ggmice)
library(tidyverse)
library(jtools)
library(sjPlot)
library(broom)
library(purrr)
library(ggeffects)

k.pool.rslt
####kinder viz ####
#age
k.pool.tbl <- as.data.frame(k.pool.rslt$estimates) #extract coefs manually 
k.pool.tbl$term <- rownames(k.pool.tbl)

k.pool.tbl <- k.pool.tbl %>%
  mutate(
    conf.low = Estimate - 1.96 * Std.Error,
    conf.high = Estimate + 1.96 * Std.Error
  ) #extract conf int manually 


var_names <- c(
  "x2kage_r" = "Age (Months)",
  "x_chsex_r2: FEMALE" = "Gender (Female)",
  "x2povty.L" = "Poverty Level (Linear)",
  "x2povty.Q" = "Poverty Level (Quadratic)",
  "par.ed.k.L" = "Parent Education (Linear)",
  "par.ed.k.Q" = "Parent Education (Quadratic)",
  "par.ed.k.C" = "Parent Education (Cubic)",
  "x1tchapp" = "Fall Kinder ATL",
  "x1prnapp" = "Parent: Approach to Learning",
  "x1inbcnt" = "Parent: Behavioral Concerns",
  "x1attnfs" = "Attentional Focus",
  "a1yrstch" = "Years of Teaching Experience",
  "x1pltot" = "Peer Interaction",
  "x2cnflct" = "Conflict with Teacher",
  "clsnss_scaled" = "Closeness with Teacher",
  "x1pltot:clsnss_scaled" = "Peer Interaction × Closeness"
)



results_with_OR <- results_with_OR %>%
  mutate(term = Variable)  # Add term column for easier splitting

coef_plots <- results_with_OR %>%
  split(.$term) %>%
  map(~ {
    ggplot(.x, aes(x = OR, y = term)) +
      geom_point(size = 3, color = "#2C3E50") +
      geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
      geom_errorbarh(aes(xmin = Lower_CI_OR, xmax = Upper_CI_OR), height = 0.2, color = "#3498DB") +
      geom_text(
        aes(label = round(OR, 2)),  # label is estimate rounded to 2 decimals
        vjust = -0.5,
        hjust = -0.5,                      # position text a bit to the right of the point
        size = 3.5, 
        color = "Black"
      ) +
      scale_x_continuous(
        limits = c(-2, 2),
        breaks = seq(-2.5, 2.5, by = 0.25),# Adjust this range as needed
        expand = c(0, 0)
      ) +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("Effect of", var_names[.x$term]),
        x = "Likelihood of 'High' ATL Rating",
        y = NULL
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })


results_with_OR %>%
  split(.$term) %>%
  map(~ {
    # Compute a nice range that always includes 1.0
    x_min <- min(.x$Lower_CI_OR, 1)
    x_max <- max(.x$Upper_CI_OR, 1)
    range_buffer <- 0.1 * (x_max - x_min)
    plot_min <- max(0, x_min - range_buffer)
    plot_max <- x_max + range_buffer
    
    ggplot(.x, aes(x = OR, y = term)) +
      geom_point(size = 3, color = "#2C3E50") +
      geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
      geom_errorbarh(aes(xmin = Lower_CI_OR, xmax = Upper_CI_OR),
                     height = 0.2, color = "#3498DB") +
      geom_text(
        aes(label = round(OR, 2)),
        vjust = -0.5,
        hjust = -0.3,
        size = 3.5, 
        color = "black"
      ) +
      scale_x_continuous(
        limits = c(plot_min, plot_max),
        breaks = scales::pretty_breaks(n = 5),
        expand = c(0, 0)
      ) +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("Effect of", var_names[.x$term]),
        x = "Likelihood of 'High' ATL Rating in Kindergarten",
        y = NULL
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })

####first grade viz####

fst.pool.tbl <- as.data.frame(fst.pool.rslt$estimates) #extract coefs manually 
fst.pool.tbl$term <- rownames(fst.pool.tbl)

fst.pool.tbl <- fst.pool.tbl %>%
  mutate(
    conf.low = Estimate - 1.96 * Std.Error,
    conf.high = Estimate + 1.96 * Std.Error
  ) #extract conf int manually 


fst_names <- c(
  "x4age" = "Age (Months)",
  "x_chsex_r2: FEMALE" = "Gender (Female)",
  "x4povty_i.L" = "Poverty Level (Linear)",
  "x4povty_i.Q" = "Poverty Level (Quadratic)",
  "fst.par.ed.L" = "Parent Education (Linear)",
  "fst.par.ed.Q" = "Parent Education (Quadratic)",
  "fst.par.ed.C" = "Parent Education (Cubic)",
  "x2tchapp" = "Fall Kinder ATL",
  "x4prnapp" = "Parent: Approach to Learning",
  "x2inbcnt" = "Parent: Behavioral Concerns",
  "x2attnfs" = "Attentional Focus",
  "a4yrstch" = "Years of Teaching Experience",
  "frst.prof.L" = "Peer Interaction",
  "frst.prof.Q" = "Peer Interaction",
  "x4cnflct" = "Conflict with Teacher",
  "clsnss_scaled" = "Closeness with Teacher",
  "frst.prof.L:clsnss_scaled" = "Peer Interaction × Closeness",
  "frst.prof.Q:clsnss_scaled" = "Peer Interaction × Closeness"
)


odds_ratios_df <- odds_ratios_df %>%
  mutate(term = Predictor)

odds_ratios_df %>%
  split(.$term) %>%
  map(~ {
    # Compute a nice range that always includes 1.0
    x_min <- min(.x$CI_Lower, 1)
    x_max <- max(.x$CI_Upper, 1)
    range_buffer <- 0.1 * (x_max - x_min)
    plot_min <- max(0, x_min - range_buffer)
    plot_max <- x_max + range_buffer
    
    ggplot(.x, aes(x = Odds_Ratio, y = term)) +
      geom_point(size = 3, color = "#2C3E50") +
      geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
      geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper),
                     height = 0.2, color = "#3498DB") +
      geom_text(
        aes(label = round(Odds_Ratio, 2)),
        vjust = -0.5,
        hjust = -0.3,
        size = 3.5, 
        color = "black"
      ) +
      scale_x_continuous(
        limits = c(plot_min, plot_max),
        breaks = scales::pretty_breaks(n = 5),
        expand = c(0, 0)
      ) +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("Effect of", fst_names[.x$term]),
        x = "Likelihood of 'High' ATL Rating in 1st Grade",
        y = NULL
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
