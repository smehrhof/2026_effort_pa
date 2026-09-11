################################################################################################
#####################---------------- SAMPLE DESCRIPTIVES ----------------######################
################################################################################################

### In this script: 
# (0) Set up
# (1) Demographics
# (2) IPAQ descriptives
# (3) Association between IPAQ and other variables
# (4) Make figure

### (0) Set up -----------------------------------------------

setwd(here::here())

# load required packages
librarian::shelf(
  ggplot2, ggpubr, tidyverse, here, janitor,
  lubridate, magrittr, broom.mixed, glmmTMB,
  viridis, PupillometryR, patchwork, scales
)

# source datasets
data <- readRDS("data/processed_data/cleaned_data.RDS")
data_excl <- readRDS("data/processed_data/excluded_data.RDS")


### (1) Demographics -----------------------------------------------

# Included data
data$demographic_data %>%
  summarise(
    n = n(),
    age_mean = sprintf("%.3f", mean(age, na.rm = TRUE)),
    age_sd   = sprintf("%.3f", sd(age, na.rm = TRUE)),
    age_min   = sprintf("%.3f", min(age, na.rm = TRUE)),
    age_max   = sprintf("%.3f", max(age, na.rm = TRUE)),
    ses_median = sprintf("%.3f", median(ses, na.rm = TRUE)),
    ses_q1 = sprintf("%.3f", quantile(ses, 0.25, na.rm = TRUE)),
    ses_q3 = sprintf("%.3f", quantile(ses, 0.75, na.rm = TRUE)),
    neurological_sum = sprintf("%.3f", sum(neurological == 1, na.rm = TRUE)),
    psych_neurdev_sum = sprintf("%.3f", sum(psych_neurdev == 1, na.rm = TRUE))
  )

sum(grepl("Major depressive disorder", 
          data$demographic_data$psych_neurdev_condition, 
          fixed = TRUE), na.rm = TRUE)

sum(
  grepl(
    "Social anxiety disorder|Generalised anxiety disorder",
    data$demographic_data$psych_neurdev_condition
  ),
  na.rm = TRUE
)

data$demographic_data %>%
  tabyl(gender)
data$demographic_data %>%
  tabyl(ethnicity)

# Excluded data
data_excl$demographic_data %>%
  summarise(
    n = n(),
    age_mean = sprintf("%.3f", mean(age, na.rm = TRUE)),
    age_sd   = sprintf("%.3f", sd(age, na.rm = TRUE)),
    age_min   = sprintf("%.3f", min(age, na.rm = TRUE)),
    age_max   = sprintf("%.3f", max(age, na.rm = TRUE)),
    ses_median = sprintf("%.3f", median(ses, na.rm = TRUE)),
    ses_q1 = sprintf("%.3f", quantile(ses, 0.25, na.rm = TRUE)),
    ses_q3 = sprintf("%.3f", quantile(ses, 0.75, na.rm = TRUE)),
    neurological_sum = sprintf("%.3f", sum(neurological == 1, na.rm = TRUE)),
    psych_neurdev_sum = sprintf("%.3f", sum(psych_neurdev == 1, na.rm = TRUE)),
    anti_d_sum = sprintf("%.3f", sum(psych_neurdev == 1, na.rm = TRUE))
  )

sum(grepl("Major depressive disorder", 
          data_excl$demographic_data$psych_neurdev_condition, 
          fixed = TRUE), na.rm = TRUE)

sum(
  grepl(
    paste(
      c("Social anxiety disorder", "Generalised anxiety disorder"),
      collapse = "|"
    ),
    data_excl$demographic_data$psych_neurdev_condition
  ),
  na.rm = TRUE
)

data_excl$demographic_data %>%
  tabyl(gender)
data_excl$demographic_data %>%
  tabyl(ethnicity)


### (2) Task descriptives -----------------------------------------------

# Included data
data$task_meta_data %>%
  summarise(
    mean_completion_time_min = sprintf("%.3f", mean(
      as.numeric(difftime(end_time, start_time, units = "mins")),
      na.rm = TRUE
    )),
    sd_completion_time_min = sprintf("%.3f", sd(
      as.numeric(difftime(end_time, start_time, units = "mins")),
      na.rm = TRUE
    )),
    min_completion_time_min = sprintf("%.3f", min(
      as.numeric(difftime(end_time, start_time, units = "mins")),
      na.rm = TRUE
    )),
    max_completion_time_min = sprintf("%.3f", max(
      as.numeric(difftime(end_time, start_time, units = "mins")),
      na.rm = TRUE
    )),
    click_mean = sprintf("%.3f", mean(clicking_calibration, na.rm = TRUE)),
    click_sd = sprintf("%.3f", sd(clicking_calibration, na.rm = TRUE)),
    click_min = sprintf("%.3f", min(clicking_calibration, na.rm = TRUE)),
    click_max = sprintf("%.3f", max(clicking_calibration, na.rm = TRUE))
  )

# Excluded data
data_excl$task_meta_data %>%
  summarise(
    mean_completion_time_min = sprintf("%.3f", mean(
      as.numeric(difftime(end_time, start_time, units = "mins")),
      na.rm = TRUE
    )),
    sd_completion_time_min = sprintf("%.3f", sd(
      as.numeric(difftime(end_time, start_time, units = "mins")),
      na.rm = TRUE
    )),
    min_completion_time_min = sprintf("%.3f", min(
      as.numeric(difftime(end_time, start_time, units = "mins")),
      na.rm = TRUE
    )),
    max_completion_time_min = sprintf("%.3f", max(
      as.numeric(difftime(end_time, start_time, units = "mins")),
      na.rm = TRUE
    )),
    click_mean = sprintf("%.3f", mean(clicking_calibration, na.rm = TRUE)),
    click_sd = sprintf("%.3f", sd(clicking_calibration, na.rm = TRUE)),
    click_min = sprintf("%.3f", min(clicking_calibration, na.rm = TRUE)),
    click_max = sprintf("%.3f", max(clicking_calibration, na.rm = TRUE))
  )

### (3) Psych descriptives -----------------------------------------------

# Included data
data$questionnaire_data %>%
  summarise(
    shaps_mean = sprintf("%.3f", mean(shaps_sumScore, na.rm = TRUE)),
    shaps_sd   = sprintf("%.3f", sd(shaps_sumScore, na.rm = TRUE)),
    shaps_min   = sprintf("%.3f", min(shaps_sumScore, na.rm = TRUE)),
    shaps_max   = sprintf("%.3f", max(shaps_sumScore, na.rm = TRUE)),
    aes_mean = sprintf("%.3f", mean(aes_sumScore, na.rm = TRUE)),
    aes_sd   = sprintf("%.3f", sd(aes_sumScore, na.rm = TRUE)),
    aes_min   = sprintf("%.3f", min(aes_sumScore, na.rm = TRUE)),
    aes_max   = sprintf("%.3f", max(aes_sumScore, na.rm = TRUE))
  )

# Excluded data
data_excl$questionnaire_data %>%
  summarise(
    shaps_mean = sprintf("%.3f", mean(shaps_sumScore, na.rm = TRUE)),
    shaps_sd   = sprintf("%.3f", sd(shaps_sumScore, na.rm = TRUE)),
    shaps_min   = sprintf("%.3f", min(shaps_sumScore, na.rm = TRUE)),
    shaps_max   = sprintf("%.3f", max(shaps_sumScore, na.rm = TRUE)),
    aes_mean = sprintf("%.3f", mean(aes_sumScore, na.rm = TRUE)),
    aes_sd   = sprintf("%.3f", sd(aes_sumScore, na.rm = TRUE)),
    aes_min   = sprintf("%.3f", min(aes_sumScore, na.rm = TRUE)),
    aes_max   = sprintf("%.3f", max(aes_sumScore, na.rm = TRUE))
  )

### (4) IPAQ descriptives -----------------------------------------------

data$questionnaire_data %>% 
  dplyr::select(c(ipaq_vigorous_days:ipaq_sitting_min, 
                  vigorous_MET_min, moderate_MET_min, 
                  walking_MET_min, total_MET, ipaq_category))

# IPAQ categories
data$questionnaire_data %>%
  count(ipaq_category) %>%
  mutate(prop = n / sum(n))

# Descriptives of continuous variables
data$questionnaire_data %>%
  summarise(
    n = n(),
    total_MET_mean = mean(total_MET, na.rm = TRUE),
    total_MET_sd   = sd(total_MET, na.rm = TRUE),
    total_MET_median = median(total_MET, na.rm = TRUE),
    total_MET_q1 = sprintf("%.3f", quantile(total_MET, 0.25, na.rm = TRUE)),
    total_MET_q3 = sprintf("%.3f", quantile(total_MET, 0.75, na.rm = TRUE)),
    
    vigorous_MET_mean = mean(vigorous_MET_min, na.rm = TRUE),
    vigorous_MET_sd   = sd(vigorous_MET_min, na.rm = TRUE),
    vigorous_MET_median = median(vigorous_MET_min, na.rm = TRUE),
    vigorous_MET_q1 = sprintf("%.3f", quantile(vigorous_MET_min, 0.25, na.rm = TRUE)),
    vigorous_MET_q3 = sprintf("%.3f", quantile(vigorous_MET_min, 0.75, na.rm = TRUE)),
    
    moderate_MET_mean = mean(moderate_MET_min, na.rm = TRUE),
    moderate_MET_sd   = sd(moderate_MET_min, na.rm = TRUE),
    moderate_MET_median = median(moderate_MET_min, na.rm = TRUE),
    moderate_MET_q1 = sprintf("%.3f", quantile(moderate_MET_min, 0.25, na.rm = TRUE)),
    moderate_MET_q3 = sprintf("%.3f", quantile(moderate_MET_min, 0.75, na.rm = TRUE)),
    
    walking_MET_mean = mean(walking_MET_min, na.rm = TRUE),
    walking_MET_sd   = sd(walking_MET_min, na.rm = TRUE),
    walking_MET_median = median(walking_MET_min, na.rm = TRUE),
    walking_MET_q1 = sprintf("%.3f", quantile(walking_MET_min, 0.25, na.rm = TRUE)),
    walking_MET_q3 = sprintf("%.3f", quantile(walking_MET_min, 0.75, na.rm = TRUE))
    
  )

# Descriptives of raw variables
data$questionnaire_data %>%
  summarise(
    vigorous_days_mean  = mean(ipaq_vigorous_days, na.rm = TRUE),
    vigorous_days_sd    = sd(ipaq_vigorous_days, na.rm = TRUE),
    
    moderate_days_mean  = mean(ipaq_moderate_days, na.rm = TRUE),
    moderate_days_sd    = sd(ipaq_moderate_days, na.rm = TRUE),
    
    walking_days_mean   = mean(ipaq_walking_days, na.rm = TRUE),
    walking_days_sd     = sd(ipaq_walking_days, na.rm = TRUE),
    
    sitting_min_mean    = mean(as.numeric(ipaq_sitting_min), na.rm = TRUE),
    sitting_min_sd      = sd(as.numeric(ipaq_sitting_min), na.rm = TRUE),
    sitting_median = median(ipaq_sitting_min, na.rm = TRUE),
    sitting_q1 = sprintf("%.3f", quantile(ipaq_sitting_min, 0.25, na.rm = TRUE)),
    sitting_q3 = sprintf("%.3f", quantile(ipaq_sitting_min, 0.75, na.rm = TRUE))
  )

# Plotting

# total MET
total_MET_plot <- data$questionnaire_data %>%
  filter(!is.na(total_MET)) %>%
  mutate(dummy_x = "Total MET-minutes") %>%
  ggplot(aes(x = dummy_x, y = total_MET)) +
  PupillometryR::geom_flat_violin(
    trim = FALSE, alpha = 0.6, fill = viridis::viridis(5, begin = 0)[1],
    color = NA, position = position_nudge(x = 0.2)
  ) +
  geom_point(
    position = position_jitter(width = 0.12),
    color = viridis::viridis(5, begin = 0)[1],
    size = 1.2, alpha = 0.3
  ) +
  geom_boxplot(
    width = 0.15, outlier.shape = NA, fill = "white", alpha = 0.6
  ) +
  labs(x = " ", y = "MET-minutes / week") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.5))) +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip()

# by activity type

# participation
participation_data <- data$questionnaire_data %>%
  mutate(
    vigorous = if_else(vigorous_MET_min > 0, "Active", "Inactive"),
    moderate = if_else(moderate_MET_min > 0, "Active", "Inactive"),
    walking  = if_else(walking_MET_min  > 0, "Active", "Inactive")
  ) %>%
  select(vigorous, moderate, walking) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "participation_response") %>%
  filter(!is.na(participation_response)) %>%
  count(category, participation_response) %>%
  group_by(category) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(participation_response == "Active") %>%
  mutate(category = factor(category, levels = c("walking", "moderate", "vigorous")))

participation_plot <- ggplot(participation_data) +
  aes(x = category, y = percent, fill = category) +
  geom_col(alpha = 0.8) +
  theme_minimal() +
  labs(
    x = " ",
    y = "% of participation"
  ) +
  scale_x_discrete(labels = c(
    "walking"  = "Walking",
    "moderate" = "Moderate",
    "vigorous" = "Vigorous"
  )) +
  scale_fill_manual(
    values = c(
      "walking"  = viridis::viridis(5, begin = 0)[2],
      "moderate" = viridis::viridis(5, begin = 0)[3],
      "vigorous" = viridis::viridis(5, begin = 0)[4]
    )
  ) +
  theme(legend.position = "none")

# amount walking
walking_MET_plot <- data$questionnaire_data %>%
  filter(!is.na(walking_MET_min)) %>%
  mutate(dummy_x = "Walking MET-minutes") %>%
  ggplot(aes(x = dummy_x, y = walking_MET_min)) +
  PupillometryR::geom_flat_violin(
    trim = FALSE, alpha = 0.6, fill = viridis::viridis(5, begin = 0)[2],
    color = NA, position = position_nudge(x = 0.2)
  ) +
  geom_point(
    position = position_jitter(width = 0.12),
    color = viridis::viridis(5, begin = 0)[2],
    size = 1.2, alpha = 0.3
  ) +
  geom_boxplot(
    width = 0.15, outlier.shape = NA, fill = "white", alpha = 0.6
  ) +
  labs(x = " ", y = "MET-minutes / week") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.5))) +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip()

# amount moderate
moderate_MET_plot <- data$questionnaire_data %>%
  filter(!is.na(moderate_MET_min)) %>%
  mutate(dummy_x = "Moderate MET-minutes") %>%
  ggplot(aes(x = dummy_x, y = moderate_MET_min)) +
  PupillometryR::geom_flat_violin(
    trim = FALSE, alpha = 0.6, fill = viridis::viridis(5, begin = 0)[3],
    color = NA, position = position_nudge(x = 0.2)
  ) +
  geom_point(
    position = position_jitter(width = 0.12),
    color = viridis::viridis(5, begin = 0)[3],
    size = 1.2, alpha = 0.3
  ) +
  geom_boxplot(
    width = 0.15, outlier.shape = NA, fill = "white", alpha = 0.6
  ) +
  labs(x = " ", y = "MET-minutes / week") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.5))) +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip()

# amount vigorous
vigorous_MET_plot <- data$questionnaire_data %>%
  filter(!is.na(vigorous_MET_min)) %>%
  mutate(dummy_x = "Vigorous MET-minutes") %>%
  ggplot(aes(x = dummy_x, y = vigorous_MET_min)) +
  PupillometryR::geom_flat_violin(
    trim = FALSE, alpha = 0.6, fill = viridis::viridis(5, begin = 0)[4],
    color = NA, position = position_nudge(x = 0.2)
  ) +
  geom_point(
    position = position_jitter(width = 0.12),
    color = viridis::viridis(5, begin = 0)[4],
    size = 1.2, alpha = 0.3
  ) +
  geom_boxplot(
    width = 0.15, outlier.shape = NA, fill = "white", alpha = 0.6
  ) +
  labs(x = " ", y = "MET-minutes / week") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.5))) +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip()

# amount sitting
sitting_MET_plot <- data$questionnaire_data %>%
  filter(!is.na(ipaq_sitting_min)) %>%
  mutate(dummy_x = "Sedetary minutes") %>%
  ggplot(aes(x = dummy_x, y = ipaq_sitting_min)) +
  PupillometryR::geom_flat_violin(
    trim = FALSE, alpha = 0.6, fill = viridis::viridis(5, begin = 0)[5],
    color = NA, position = position_nudge(x = 0.2)
  ) +
  geom_point(
    position = position_jitter(width = 0.12),
    color = viridis::viridis(5, begin = 0)[5],
    size = 1.2, alpha = 0.3
  ) +
  geom_boxplot(
    width = 0.15, outlier.shape = NA, fill = "white", alpha = 0.6
  ) +
  labs(x = " ", y = "Minutes / week") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.5))) +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip()

### (3) Association between IPAQ and other variables -----------------------------------------------

covariate_data <- data$demographic_data %>%
  mutate(
    age_z = scale(age)[,1],
    gender = factor(gender)) %>% 
  left_join(data$questionnaire_data %>% 
              mutate(bmi_z = scale(bmi)[,1]) %>% 
              select(subj_id, bmi_z, total_MET))

covariate_data %<>%
  mutate(
    gender = if_else(
      gender == "Non-binary",
      sex,
      gender
    )
  )

covariate_model <- glmmTMB(total_MET ~ age_z + gender + bmi_z,
                       family = tweedie(link = "log"),
                       data = covariate_data)
summary(covariate_model)

# Plot 
forest_data <- tidy(covariate_model, conf.int = TRUE, effects = "fixed") %>%
  filter(term %in% c("age_z", "genderMale", "bmi_z")) %>%
  mutate(
    covariate = case_when(
      term == "age_z"      ~ "Age",
      term == "genderMale" ~ "Gender\n(Male)",
      term == "bmi_z"      ~ "BMI"
    ),
    exp_est       = exp(estimate),
    exp_conf_low  = exp(conf.low),
    exp_conf_high = exp(conf.high),
    significant   = p.value < 0.05,
    p_label       = case_when(
      p.value < 0.001 ~ "p < .001",
      p.value < 0.05  ~ paste0("p = ", sprintf("%.3f", p.value)),
      TRUE            ~ paste0("p = ", sprintf("%.2f",  p.value))
    ),
    covariate = factor(covariate, levels = c("BMI", "Gender\n(Male)", "Age"))
  )

covariate_plot <- ggplot(forest_data,
                         aes(x = exp_est, y = covariate, colour = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = "grey50", linewidth = 0.6) +
  geom_errorbarh(
    aes(xmin = exp_conf_low, xmax = exp_conf_high),
    height = 0.12, linewidth = 0.85, alpha = 0.75) +
  geom_point(size = 4, alpha = 0.75) +
  scale_colour_manual(
    values = c(
      "TRUE"  = viridis::viridis(5, begin = 0)[1],
      "FALSE" = "grey65"), guide = "none") +
  scale_x_continuous(
    name = "Effect ratio (95% CI)") +
  labs( y = NULL) +
  theme_classic(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle      = element_text(size = 10, colour = "grey45", hjust = 0),
    axis.title.x       = element_text(size = 11, margin = margin(t = 8)),
    axis.text.y        = element_text(size = 11, colour = "grey15"),
    axis.text.x        = element_text(size = 10, colour = "grey20"),
    axis.line.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4),
    plot.margin        = margin(12, 60, 12, 12)
  )


### (4) Make figure -----------------------------------------------

# Arrange all plots

line_1 <- (total_MET_plot | covariate_plot | participation_plot)

line_2 <- (walking_MET_plot | moderate_MET_plot | vigorous_MET_plot | sitting_MET_plot)

ipaq_plots <- line_1 / line_2

# output
ggsave(
  "plots/ipaq_plots.png",
  plot = ipaq_plots,
  width = 10,
  height = 6,
  units = "in"
)









