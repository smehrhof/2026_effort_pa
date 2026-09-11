# ─────────────────────────────────────────────────────────────
# Plot functions
# ─────────────────────────────────────────────────────────────

# 1. Extract and format model estimates -----------------------

make_forest_data <- function(
    models,
    terms,
    labels,
    levels = names(models),
    predictor_name = NULL
) {
  
  purrr::imap_dfr(
    models,
    ~ broom::tidy(
      .x,
      conf.int = TRUE
    ) %>%
      mutate(model = .y)
  ) %>%
    filter(term %in% terms) %>%
    mutate(
      predictor = dplyr::recode(
        term,
        !!!labels
      ),
      
      # Use model name as predictor if requested
      predictor = if (!is.null(predictor_name)) {
        model
      } else {
        predictor
      },
      
      exp_est = exp(estimate),
      exp_conf_low = exp(conf.low),
      exp_conf_high = exp(conf.high),
      
      significant = p.value < 0.05,
      
      p_label = case_when(
        p.value < 0.001 ~ "p < .001",
        p.value < 0.05 ~ paste0(
          "p = ",
          sprintf("%.3f", p.value)
        ),
        TRUE ~ paste0(
          "p = ",
          sprintf("%.2f", p.value)
        )
      ),
      
      predictor = factor(
        predictor,
        levels = levels
      )
    )
}


# 2. Make forest plot -----------------------------------------

make_forest_plot <- function(
    data,
    colours,
    y_labels = NULL,
    x_label = "Effect ratio (95% CI)",
    x_limits = NULL
) {
  
  p <- ggplot(
    data,
    aes(
      x = exp_est,
      y = predictor,
      colour = predictor
    )
  ) +
    
    # Null effect
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.6
    ) +
    
    # Confidence intervals
    geom_errorbarh(
      aes(
        xmin = exp_conf_low,
        xmax = exp_conf_high
      ),
      height = 0.12,
      linewidth = 0.85,
      alpha = 0.75
    ) +
    
    # Point estimates
    geom_point(
      size = 4,
      alpha = 0.75
    ) +
    
    # Colours
    scale_colour_manual(
      values = colours,
      guide = "none"
    ) +
    
    # X axis
    scale_x_continuous(
      name = x_label,
      limits = x_limits,
      expand = expansion(mult = 0)
    ) +
    
    labs(
      y = NULL
    ) +
    
    # Theme
    theme_classic(
      base_size = 12
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 12,
        hjust = 0
      ),
      plot.subtitle = element_text(
        size = 10,
        colour = "grey45",
        hjust = 0
      ),
      axis.title.x = element_text(
        size = 11,
        margin = margin(t = 8)
      ),
      axis.text.y = element_text(
        size = 11,
        colour = "grey15"
      ),
      axis.text.x = element_text(
        size = 10,
        colour = "grey20"
      ),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(
        colour = "grey92",
        linewidth = 0.4
      ),
      plot.margin = margin(
        12, 60, 12, 12
      )
    ) +
    
    # Keep horizontal forest plot
    coord_flip()
  
  
  # Optional custom y-axis labels
  if (!is.null(y_labels)) {
    
    p <- p +
      scale_y_discrete(
        labels = y_labels
      )
  }
  
  p
}


# 3. Make scatter plot ----------------------------------------

make_scatter_plot <- function(
    data,
    x,
    y,
    colour,
    x_label = "MET-minutes / week",
    y_label = NULL,
    point_alpha = 0.2,
    line_alpha = 0.2
) {
  
  ggplot(
    data,
    aes(
      x = {{ x }},
      y = {{ y }}
    )
  ) +
    
    # Raw observations
    geom_point(
      alpha = point_alpha,
      colour = colour
    ) +
    
    # Regression line + CI
    geom_smooth(
      method = "lm",
      se = TRUE,
      colour = colour,
      fill = colour,
      alpha = line_alpha
    ) +
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    theme_minimal(
      base_size = 12
    ) +
    theme(
      strip.text = element_text(
        face = "bold"
      ),
      legend.position = "none"
    )
}



make_multi_outcome_forest_plot <- function(
    data,
    colours,
    outcome_var = "outcome",
    predictor_var = "predictor",
    y_labels = NULL,
    x_label = "Effect ratio (95% CI)",
    x_limits = NULL
) {
  
  # Get unique predictors from data
  unique_predictors <- unique(data[[predictor_var]])
  
  data <- data %>%
    mutate(
      outcome = factor(
        .data[[outcome_var]],
        levels = c(
          "Walking\nparticipation",
          "Moderate\nparticipation",
          "Vigorous\nparticipation",
          "Walking\namount",
          "Moderate\namount",
          "Vigorous\namount"
        )
      ),
      
      outcome_position = as.numeric(outcome),
      
      # Dynamically assign positions based on unique predictors
      predictor_position = case_when(
        .data[[predictor_var]] == unique_predictors[1] ~ 1,
        .data[[predictor_var]] == unique_predictors[2] ~ 2,
        TRUE ~ NA_real_
      ),
      
      y_position = predictor_position +
        (outcome_position - mean(range(outcome_position))) * 0.12
    )
  
  # Remove rows with NA positions
  data <- data %>% filter(!is.na(y_position))
  
  ggplot(
    data,
    aes(
      x = exp_est,
      y = y_position,
      colour = .data[[outcome_var]]
    )
  ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.6
    ) +
    geom_errorbarh(
      aes(
        xmin = exp_conf_low,
        xmax = exp_conf_high
      ),
      height = 0.08,
      linewidth = 0.85,
      alpha = 0.75
    ) +
    geom_point(
      size = 4,
      alpha = 0.75
    ) +
    scale_colour_manual(
      values = colours,
      name = NULL
    ) +
    scale_x_continuous(
      name = x_label,
      limits = x_limits,
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = 1:length(unique_predictors),
      labels = unique_predictors,
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(y = NULL) +
    theme_classic(base_size = 12) +
    theme(
      axis.title.x = element_text(
        size = 11,
        margin = margin(t = 8)
      ),
      axis.text.y = element_text(
        size = 11,
        colour = "grey15"
      ),
      axis.text.x = element_text(
        size = 10,
        colour = "grey20"
      ),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(
        colour = "grey92",
        linewidth = 0.4
      ),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.direction = "horizontal",
      plot.margin = margin(12, 60, 12, 12)
    ) +
    guides(
      colour = guide_legend(
        nrow = 2,
        byrow = TRUE
      )
    ) +
    coord_flip()
}





