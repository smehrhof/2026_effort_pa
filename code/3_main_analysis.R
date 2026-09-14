##############################################################################################
#######################---------------- MAIN ANALYSIS ----------------########################
##############################################################################################

### In this script: 
# (0) Set up
# (1) Prepare data
# (2) Total MET analysis
# (3) Total MET plots
# (4) Analysis by activity type 
# (5) Activity type plots
# (6) Effort x intensity interaction

### (0) Set up -----------------------------------------------

# load required packages
librarian::shelf(ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr, here, janitor, MatchIt, clr, TMB,
                 writexl, lubridate, magrittr, nlme, broom, scales, viridis, rstatix, ggeffects, ggdist, 
                 patchwork, emmeans, glmmTMB)

# load data
combined_data <- readRDS(here::here("data/combined_data.RDS"))

# Source helper functions
source("code/functions/plotting_funs.R")

### (1) Prepare data -----------------------------------------------

# input natal sex for non-binary individuals (due to low numbers)
combined_data %<>%
  mutate(
    gender = if_else(
      gender == "Non-binary",
      sex,
      gender
    )
  )

# Standardise continuous predictors
combined_data <- combined_data %>%
  mutate(
    kE_z  = scale(kE)[,1],
    kR_z  = scale(kR)[,1],
    a_z   = scale(a)[,1],
    age_z = scale(age)[,1],
    gender = factor(gender), 
    shaps_z = scale(shaps_sumScore)[,1],
    aes_z = scale(aes_sumScore)[,1], 
    bmi_z = scale(bmi)[,1],
    meq_z = scale(meq_sumScore)[,1]
  )

### (2) Total MET analysis -----------------------------------------------

# Using Tweedie GLM with a log link to deal with heavily right-skewed IPAQ data and 0s
# -> treats the zeros and positive continuous values as part of a single distribution
# Controlling all analyses for age, gender, and BMI

# ── Acceptance bias predicting total METs ──
### with SHAPS
a_shaps_glm <- glmmTMB(total_MET ~ a_z * shaps_z + age_z + gender + bmi_z,
                       family = tweedie(link = "log"),
                       data = combined_data)
summary(a_shaps_glm)

# % change in physical activity per SD increase in SHAPS
shaps_a_beta <- summary(a_shaps_glm) %>% coef() %>% .$cond %>% .[3,1]
(shaps_a_beta %>% exp() - 1) * 100

### with AES
a_aes_glm <- glmmTMB(total_MET ~ a_z * aes_z + age_z + gender + bmi_z,
                     family = tweedie(link = "log"),
                     data = combined_data)
summary(a_aes_glm)

# % change in physical activity per SD increase in AES
aes_a_beta <- summary(a_aes_glm) %>% coef() %>% .$cond %>% .[3,1]
(-aes_a_beta %>% exp() - 1) * 100

# ── Effort sensitivity predicting total METs ──
### with SHAPS
kE_shaps_glm <- glmmTMB(total_MET ~ kE_z * shaps_z + age_z + gender + bmi_z,
                        family = tweedie(link = "log"),
                        data = combined_data)
summary(kE_shaps_glm)

# % change in physical activity per SD increase in SHAPS
shaps_kE_beta <- summary(kE_shaps_glm) %>% coef() %>% .$cond %>% .[3,1]
(shaps_kE_beta %>% exp() - 1) * 100

# % change in physical activity per SD increase in effort sensitivity
kE_shaps_beta <- summary(kE_shaps_glm) %>% coef() %>% .$cond %>% .[2,1]
(kE_shaps_beta %>% exp() - 1) * 100

### with AES
kE_aes_glm <- glmmTMB(total_MET ~ kE_z * aes_z + age_z + gender + bmi_z,
                      family = tweedie(link = "log"),
                      data = combined_data)
summary(kE_aes_glm)

# % change in physical activity per SD increase in AES
aes_kE_beta <- summary(kE_aes_glm) %>% coef() %>% .$cond %>% .[3,1]
(-aes_kE_beta %>% exp() - 1) * 100

# % change in physical activity per SD increase in effort sensitivity
kE_aes_beta <- summary(kE_aes_glm) %>% coef() %>% .$cond %>% .[2,1]
(kE_aes_beta %>% exp() - 1) * 100

# ── Reward sensitivity predicting total METs ──
### with SHAPS
kR_shaps_glm <- glmmTMB(total_MET ~ kR_z * shaps_z + age_z + gender + bmi_z,
                        family = tweedie(link = "log"),
                        data = combined_data)
summary(kR_shaps_glm)

# % change in physical activity per SD increase in SHAPS
shaps_kR_beta <- summary(kR_shaps_glm) %>% coef() %>% .$cond %>% .[3,1]
(shaps_kR_beta %>% exp() - 1) * 100

### with AES
kR_aes_glm <- glmmTMB(total_MET ~ kR_z * aes_z + age_z + gender + bmi_z,
                      family = tweedie(link = "log"),
                      data = combined_data)
summary(kR_aes_glm)

# % change in physical activity per SD increase in AES
aes_kR_beta <- summary(kR_aes_glm) %>% coef() %>% .$cond %>% .[3,1]
(aes_kR_beta %>% exp() - 1) * 100

# Total MET minutes specifically relate to effort sensitivity: 
# higher effort sensitivity predicts lower total MET minutes 
# No relationship to either acceptance bias or reward sensitivity


### (3) Total MET plots -----------------------------------------------

#### ----- Forest plots

### Make datasets
# Psychological predictors
models_psych <- list(
  "SHAPS model" = kE_shaps_glm,
  "AES model"   = kE_aes_glm
)

forest_data_psych <- make_forest_data(
  models = models_psych,
  terms = c("shaps_z", "aes_z"),
  labels = c(
    "shaps_z" = "SHAPS",
    "aes_z"   = "AES"
  ),
  levels = c("SHAPS", "AES")
)

# Computational predictors
models_comp <- list(
  "Acceptance bias model"    = a_shaps_glm,
  "Effort sensitivity model" = kE_shaps_glm,
  "Reward sensitivity model" = kR_shaps_glm
)

forest_data_comp <- make_forest_data(
  models = models_comp,
  terms = c("a_z", "kE_z", "kR_z"),
  labels = c(
    "a_z"  = "a",
    "kE_z" = "kE",
    "kR_z" = "kR"
  ),
  levels = c("a", "kE", "kR")
)


### Plot
psych_pred_plot <- make_forest_plot(
  data = forest_data_psych,
  colours = c(
    "SHAPS" = viridis::rocket(5, begin = 0.2, end = 0.8)[1],
    "AES" = viridis::rocket(5, begin = 0.2, end = 0.8)[2]),
  x_limits = c(0.65, 1.35))


comp_pred_plot <- make_forest_plot(
  data = forest_data_comp,
  colours = c(
    "a" = colorspace::lighten(viridis::rocket(5, begin = 0.2, end = 0.8)[3], amount = 0.5),
    "kE" = viridis::rocket(5, begin = 0.2, end = 0.8)[4],
    "kR" = colorspace::lighten(viridis::rocket(5, begin = 0.2, end = 0.8)[5], amount = 0.5)),
  y_labels = c(
    "a"  = "Acceptance\nbias",
    "kE" = "Effort\nsensitivity",
    "kR" = "Reward\nsensitivity"
  ),
  x_limits = c(0.8, 1.2)
)

#### ----- Scatter plots

shaps_total_plot <- make_scatter_plot(
  data = combined_data,
  x = total_MET,
  y = shaps_sumScore,
  colour = viridis::rocket(5, begin = 0.2, end = 0.8)[1],
  y_label = "SHAPS sum score",
  line_alpha = 0.2
)

aes_total_plot <- make_scatter_plot(
  data = combined_data,
  x = total_MET,
  y = aes_sumScore,
  colour = viridis::rocket(5, begin = 0.2, end = 0.8)[2],
  y_label = "AES sum score",
  line_alpha = 0.2
)

kE_total_plot <- make_scatter_plot(
  data = combined_data,
  x = total_MET,
  y = kE,
  colour = viridis::rocket(5, begin = 0.2, end = 0.8)[4],
  y_label = "Effort sensitivity",
  line_alpha = 0.2
)

line_1 <- (psych_pred_plot | comp_pred_plot) +
  plot_layout(widths = c(2, 3))

line_2 <- (shaps_total_plot | aes_total_plot | kE_total_plot)

total_MET_plot <- line_1 / line_2

# output
ggsave(
  "plots/total_met_plots.png",
  plot = total_MET_plot,
  width = 10,
  height = 7,
  units = "in"
)


### (4) Analysis by activity type -----------------------------------------------

# Is the relationship between EBDM and physical activity specific to a type of physical activity?

# When separating MET minutes by physical activity type, distributions reflects overlapping processes:
# - whether one engages in that activity type at all
# - how much they engage in that activity type, given they do

# Analysis: two part model
# - Logistic regression: kE predicting whether someone engages 
# - Gamma GLM: kE predicting how much they engage

# ── Vigorous physical activity ──
# - Logistic: does kE and shaps/aes predict participation? - 
### with SHAPS
kE_shaps_vig_log <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data   = combined_data %>% mutate(participated = as.integer(vigorous_MET_min > 0)),
  family = binomial(link = "logit")
)

summary(kE_shaps_vig_log)

# change participation odds per SD increase in SHAPS
shaps_kE_vig_beta <- summary(kE_shaps_vig_log) %>% coef() %>% .[3,1]
OR <- exp(shaps_kE_vig_beta)
(OR - 1) * 100

### with AES
kE_aes_vig_log <- glm(
  participated ~ kE_z + aes_z + age_z + gender  + bmi_z,
  data   = combined_data %>% mutate(participated = as.integer(vigorous_MET_min > 0)),
  family = binomial(link = "logit")
)

summary(kE_aes_vig_log)

# change participation odds per SD increase in AES
aes_kE_vig_beta <- summary(kE_aes_vig_log) %>% coef() %>% .[3,1]
OR <- exp(-aes_kE_vig_beta)
(OR - 1) * 100

# - Gamma GLM: does kE and shaps/aes predict amount? -
## with SHAPS
kE_shaps_vig_glm <- glm(
  vigorous_MET_min ~ kE_z + shaps_z + age_z + gender  + bmi_z,
  data   = combined_data %>% filter(vigorous_MET_min > 0),
  family = Gamma(link = "log")
)

summary(kE_shaps_vig_glm)

# % change in vigorous activity per SD increase in effort sensitivity
kE_shaps_vig_beta <- summary(kE_shaps_vig_glm) %>% coef() %>% .[2,1]
(kE_shaps_vig_beta %>% exp() - 1) * 100

## with AES
kE_aes_vig_glm <- glm(
  vigorous_MET_min ~ kE_z + aes_z + age_z + gender  + bmi_z,
  data   = combined_data %>% filter(vigorous_MET_min > 0),
  family = Gamma(link = "log")
)

summary(kE_aes_vig_glm)

# % change in vigorous activity per SD increase in effort sensitivity
kE_aes_vig_beta <- summary(kE_aes_vig_glm) %>% coef() %>% .[2,1]
(kE_aes_vig_beta %>% exp() - 1) * 100

# ── Moderate physical activity ──
# - Logistic: does kE and shaps/aes predict participation? -
### with SHAPS
kE_shaps_mod_log <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data   = combined_data %>% mutate(participated = as.integer(moderate_MET_min > 0)),
  family = binomial(link = "logit")
)

summary(kE_shaps_mod_log)

# change participation odds per SD increase in SHAPS 
shaps_kE_mod_beta <- summary(kE_shaps_mod_log) %>% coef() %>% .[3,1]
OR <- exp(shaps_kE_mod_beta)
(OR - 1) * 100

### with AES
kE_aes_mod_log <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data   = combined_data %>% mutate(participated = as.integer(moderate_MET_min > 0)),
  family = binomial(link = "logit")
)

summary(kE_aes_mod_log)

# change participation odds per SD increase in AES
aes_kE_mod_beta <- summary(kE_aes_mod_log) %>% coef() %>% .[3,1]
OR <- exp(-aes_kE_mod_beta)
(OR - 1) * 100

# - Gamma GLM: does kE and shaps/aes predict amount? -
### with SHAPS
kE_shaps_mod_glm <- glm(
  moderate_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data   = combined_data %>% filter(moderate_MET_min > 0),
  family = Gamma(link = "log")
)

summary(kE_shaps_mod_glm)

# % change in moderate activity per SD increase in effort sensitivity
kE_shaps_mod_beta <- summary(kE_shaps_mod_glm) %>% coef() %>% .[2,1]
(kE_shaps_mod_beta %>% exp() - 1) * 100

### with AES
kE_aes_mod_glm <- glm(
  moderate_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data   = combined_data %>% filter(moderate_MET_min > 0),
  family = Gamma(link = "log")
)

summary(kE_aes_mod_glm)

# % change in moderate activity per SD increase in effort sensitivity
kE_aes_mod_beta <- summary(kE_aes_mod_glm) %>% coef() %>% .[2,1]
(kE_aes_mod_beta %>% exp() - 1) * 100

# % change in moderate activity per SD increase in AES
aes_kE_mod_beta <- summary(kE_aes_mod_glm) %>% coef() %>% .[3,1]
(-aes_kE_mod_beta %>% exp() - 1) * 100

# ── Walking ──
# - Logistic: does kE and shaps/aes predict participation? -
### with SHAPS
kE_shaps_walk_log <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data   = combined_data %>% mutate(participated = as.integer(walking_MET_min > 0)),
  family = binomial(link = "logit")
)

summary(kE_shaps_walk_log)

# change participation odds per SD increase in SHAPS
shaps_kE_walk_beta <- summary(kE_shaps_walk_log) %>% coef() %>% .[3,1]
OR <- exp(shaps_kE_walk_beta)
(OR - 1) * 100

### with AES
kE_aes_walk_log <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data   = combined_data %>% mutate(participated = as.integer(walking_MET_min > 0)),
  family = binomial(link = "logit")
)

summary(kE_aes_walk_log)

# change participation odds per SD increase in AES
aes_kE_walk_beta <- summary(kE_aes_walk_log) %>% coef() %>% .[3,1]
OR <- exp(-aes_kE_walk_beta)
(OR - 1) * 100

# - Gamma GLM: does kE and shaps/aes predict amount? -
### with SHAPS
kE_shaps_walk_glm <- glm(
  walking_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data   = combined_data %>% filter(walking_MET_min > 0),
  family = Gamma(link = "log")
)

summary(kE_shaps_walk_glm)

# % change in walking per SD increase in effort sensitivity
kE_shaps_walk_beta <- summary(kE_shaps_walk_glm) %>% coef() %>% .[2,1]
(kE_shaps_walk_beta %>% exp() - 1) * 100

### with AES
kE_aes_walk_glm <- glm(
  walking_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data   = combined_data %>% filter(walking_MET_min > 0),
  family = Gamma(link = "log")
)

summary(kE_aes_walk_glm)

# % change in walking per SD increase in effort sensitivity
kE_aes_walk_beta <- summary(kE_aes_walk_glm) %>% coef() %>% .[2,1]
(kE_aes_walk_beta %>% exp() - 1) * 100

# % change in walking per SD increase in AES
kE_aes_walk_beta <- summary(kE_aes_walk_glm) %>% coef() %>% .[3,1]
(-kE_aes_walk_beta %>% exp() - 1) * 100

# ── Sitting ──
# - Gamma GLM: does kE and shaps/aes predict time spent sitting? -
### with SHAPS
kE_shaps_sit_glm <- glm(
  ipaq_sitting_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data   = combined_data,
  family = Gamma(link = "log")
)

summary(kE_shaps_sit_glm)

# % change in sitting per SD increase in shaps
shaps_kE_sit_beta <- summary(kE_shaps_sit_glm) %>% coef() %>% .[3,1]
(shaps_kE_sit_beta %>% exp() - 1) * 100

### with AES
kE_aes_sit_glm <- glm(
  ipaq_sitting_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data   = combined_data,
  family = Gamma(link = "log")
)

summary(kE_aes_sit_glm)

# % change in sitting per SD increase in aes
aes_kE_sit_beta <- summary(kE_aes_sit_glm) %>% coef() %>% .[3,1]
(-aes_kE_sit_beta %>% exp() - 1) * 100

### (5) Activity type plots -----------------------------------------------

#### ----- Forest plots

# psych variables

forest_data_psych <- bind_rows(
  
  make_forest_data(
    models = list(
      "Walking\nparticipation" = kE_shaps_walk_log,
      "Moderate\nparticipation" = kE_shaps_mod_log,
      "Vigorous\nparticipation" = kE_shaps_vig_log,
      
      "Walking\namount" = kE_shaps_walk_glm,
      "Moderate\namount" = kE_shaps_mod_glm,
      "Vigorous\namount" = kE_shaps_vig_glm,
      
      "Walking\nparticipation" = kE_aes_walk_log,
      "Moderate\nparticipation" = kE_aes_mod_log,
      "Vigorous\nparticipation" = kE_aes_vig_log,
      
      "Walking\namount" = kE_aes_walk_glm,
      "Moderate\namount" = kE_aes_mod_glm,
      "Vigorous\namount" = kE_aes_vig_glm
    ),
    terms = c("shaps_z", "aes_z"),
    labels = c(
      "shaps_z" = "SHAPS",
      "aes_z" = "AES"
    ),
    levels = c(
      "SHAPS", "AES"
    )
  )
  
) %>%
  mutate(
    outcome = factor(
      model,
      levels = c(
        "Walking\nparticipation", 
        "Moderate\nparticipation", 
        "Vigorous\nparticipation",
        "Walking\namount",
        "Moderate\namount",
        "Vigorous\namount"
      )
    )
  )

multi_outcome_psych_plot <- make_multi_outcome_forest_plot(
  data = forest_data_psych,
  colours = c(
    "Walking\nparticipation"  = viridis::mako(6, begin = 0.2, end = 0.8)[1],
    "Moderate\nparticipation" = viridis::mako(6, begin = 0.2, end = 0.8)[2],
    "Vigorous\nparticipation" = viridis::mako(6, begin = 0.2, end = 0.8)[3],
    "Walking\namount"         = viridis::mako(6, begin = 0.2, end = 0.8)[4],
    "Moderate\namount"        = viridis::mako(6, begin = 0.2, end = 0.8)[5],
    "Vigorous\namount"        = viridis::mako(6, begin = 0.2, end = 0.8)[6]
  ),
  x_label = "Effect ratio (95% CI)",
  x_limits = c(0.5, 2)
)

multi_outcome_psych_plot

# comp variables

forest_data_comp <- bind_rows(
  
  make_forest_data(
    models = list(
      "Walking\nparticipation" = kE_shaps_walk_log,
      "Moderate\nparticipation" = kE_shaps_mod_log,
      "Vigorous\nparticipation" = kE_shaps_vig_log,
      
      "Walking\namount" = kE_shaps_walk_glm,
      "Moderate\namount" = kE_shaps_mod_glm,
      "Vigorous\namount" = kE_shaps_vig_glm
    ),
    terms = c("kE_z"),
    labels = c(
      "kE_z" = "Effort\nsensitivity"
    ),
    levels = c(
      "Effort\nsensitivity"
    )
  )
  
) %>%
  mutate(
    outcome = factor(
      model,
      levels = c(
        "Walking\nparticipation", 
        "Moderate\nparticipation", 
        "Vigorous\nparticipation",
        "Walking\namount",
        "Moderate\namount",
        "Vigorous\namount"
      )
    )
  )

multi_outcome_comp_plot <- make_multi_outcome_forest_plot(
  data = forest_data_comp,
  colours = c(
    "Walking\nparticipation"  = viridis::mako(6, begin = 0.2, end = 0.8)[1],
    "Moderate\nparticipation" = viridis::mako(6, begin = 0.2, end = 0.8)[2],
    "Vigorous\nparticipation" = viridis::mako(6, begin = 0.2, end = 0.8)[3],
    "Walking\namount"         = viridis::mako(6, begin = 0.2, end = 0.8)[4],
    "Moderate\namount"        = viridis::mako(6, begin = 0.2, end = 0.8)[5],
    "Vigorous\namount"        = viridis::mako(6, begin = 0.2, end = 0.8)[6]
  ),
  x_label = "Effect ratio (95% CI)",
  x_limits = c(0.75, 1.5)
)

multi_outcome_comp_plot

#### ----- Scatter plots

kE_walk_plot <- make_scatter_plot(
  data = combined_data,
  x = walking_MET_min,
  y = kE,
  colour = viridis::mako(6, begin = 0.2, end = 0.8)[4],
  y_label = "Effort sensitivity",
  x_label = "Walking MET-minutes / week",
  line_alpha = 0.2
)

kE_mod_plot <- make_scatter_plot(
  data = combined_data,
  x = moderate_MET_min,
  y = kE,
  colour = viridis::mako(6, begin = 0.2, end = 0.8)[5],
  y_label = "Effort sensitivity",
  x_label = "Moderate MET-minutes / week",
  line_alpha = 0.2
)

kE_vig_plot <- make_scatter_plot(
  data = combined_data,
  x = vigorous_MET_min,
  y = kE,
  colour = viridis::mako(6, begin = 0.2, end = 0.8)[6],
  y_label = "Effort sensitivity",
  x_label = "Vigorous MET-minutes / week",
  line_alpha = 0.2
)

line_1 <- (multi_outcome_psych_plot | multi_outcome_comp_plot) +
  plot_layout(
    widths = c(3, 2),
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  ) &
  guides(
    colour = guide_legend(nrow = 1)
  )

line_2 <- (kE_walk_plot | kE_mod_plot | kE_vig_plot)

activity_type_plot <- line_1 / line_2

# output
ggsave(
  "plots/activity_type_plots.png",
  plot = activity_type_plot,
  width = 10,
  height = 7,
  units = "in"
)

### (6) Effort x intensity interaction -----------------------------------------------

# Reshape activity amount variables to long format
activity_long <- combined_data %>%
  select(
    subj_id,
    kE_z,
    age_z,
    gender,
    bmi_z,
    walking_MET_min,
    moderate_MET_min,
    vigorous_MET_min
  ) %>%
  pivot_longer(
    cols = c(
      walking_MET_min,
      moderate_MET_min,
      vigorous_MET_min
    ),
    names_to = "activity_type",
    values_to = "activity_amount"
  ) %>%
  mutate(
    activity_type = factor(
      activity_type,
      levels = c(
        "walking_MET_min",
        "moderate_MET_min",
        "vigorous_MET_min"
      ),
      labels = c(
        "Walking",
        "Moderate",
        "Vigorous"
      )
    )
  ) %>%
  # Only include participants who engaged in the respective activity
  filter(activity_amount > 0)


# Mixed-effects Gamma model

kE_activity_amount_model <- glmmTMB(
  activity_amount ~
    kE_z * activity_type +
    age_z + gender + bmi_z +
    (1 | subj_id),
  data = activity_long,
  family = Gamma(link = "log")
)

summary(kE_activity_amount_model)


# Test the kE × activity intensity interaction

kE_activity_amount_model_no_int <- glmmTMB(
  activity_amount ~
    kE_z + activity_type +
    age_z + gender + bmi_z +
    (1 | subj_id),
  data = activity_long,
  family = Gamma(link = "log")
)

anova(
  kE_activity_amount_model_no_int,
  kE_activity_amount_model
)


# Estimate kE slopes for each activity intensity

kE_slopes <- emtrends(
  kE_activity_amount_model,
  ~ activity_type,
  var = "kE_z"
)

summary(
  kE_slopes,
  infer = c(TRUE, TRUE)
)


# Convert kE slopes to percentage change in activity amount

kE_slopes_df <- as.data.frame(kE_slopes) %>%
  mutate(
    percent_change = (exp(kE_z.trend) - 1) * 100
  )

kE_slopes_df


# Pairwise comparisons of kE slopes

kE_slope_contrasts <- contrast(
  kE_slopes,
  method = "pairwise", 
  adjust = "Bonferroni"
)

summary(
  kE_slope_contrasts,
  infer = c(TRUE, TRUE)
)

