##############################################################################################
####################---------------- SENSITIVITY ANALYSIS ----------------####################
##############################################################################################

### In this script: 
# (0) Set up
# (1) Prepare data
# (2) Total MET
# (3) Activity participation
# (4) Activity amount

### (0) Set up -----------------------------------------------

# load required packages
librarian::shelf(
  tidyverse,
  here,
  magrittr,
  glmmTMB
)

# load data
combined_data <- readRDS(here::here("data/processed_data/combined_data.RDS"))


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

data_excl_chrono <- combined_data %>%
  filter(study != "chrono")

data_excl_t2d <- combined_data %>%
  filter(study != "t2d")

### (2) Total MET -----------------------------------------------

### with SHAPS
#---- Full sample ----
# Effort sensitivity
kE_shaps_glm <- glmmTMB(total_MET ~ kE_z * shaps_z + age_z + gender + bmi_z,
                           family = tweedie(link = "log"),
                           data = combined_data)

coefs <- summary(kE_shaps_glm)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Reward sensitivity
kR_shaps_glm <- glmmTMB(total_MET ~ kR_z * shaps_z + age_z + gender + bmi_z,
                        family = tweedie(link = "log"),
                        data = combined_data)

coefs <- summary(kR_shaps_glm)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Acceptance bias
a_shaps_glm <- glmmTMB(total_MET ~ a_z * shaps_z + age_z + gender + bmi_z,
                        family = tweedie(link = "log"),
                        data = combined_data)

coefs <- summary(a_shaps_glm)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# ---- Excluding chronotype study ----
# Effort sensitivity
kE_shaps_glm_s1 <- glmmTMB(total_MET ~ kE_z * shaps_z + age_z + gender + bmi_z,
                           family = tweedie(link = "log"),
                           data = data_excl_chrono)

coefs <- summary(kE_shaps_glm_s1)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Reward sensitivity
kR_shaps_glm_s1 <- glmmTMB(total_MET ~ kR_z * shaps_z + age_z + gender + bmi_z,
                        family = tweedie(link = "log"),
                        data = data_excl_chrono)

coefs <- summary(kR_shaps_glm_s1)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Acceptance bias
a_shaps_glm_s1 <- glmmTMB(total_MET ~ a_z * shaps_z + age_z + gender + bmi_z,
                       family = tweedie(link = "log"),
                       data = data_excl_chrono)

coefs <- summary(a_shaps_glm_s1)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# ---- Excluding t2d study ----
# Effort sensitivity
kE_shaps_glm_s2 <- glmmTMB(total_MET ~ kE_z * shaps_z + age_z + gender + bmi_z,
                        family = tweedie(link = "log"),
                        data = data_excl_t2d)

coefs <- summary(kE_shaps_glm_s2)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Reward sensitivity
kR_shaps_glm_s2 <- glmmTMB(total_MET ~ kR_z * shaps_z + age_z + gender + bmi_z,
                           family = tweedie(link = "log"),
                           data = data_excl_t2d)

coefs <- summary(kR_shaps_glm_s2)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Acceptance bias
a_shaps_glm_s2 <- glmmTMB(total_MET ~ a_z * shaps_z + age_z + gender + bmi_z,
                          family = tweedie(link = "log"),
                          data = data_excl_t2d)

coefs <- summary(a_shaps_glm_s2)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

### with AES
# ---- Full sample ----
kE_aes_glm <- glmmTMB(total_MET ~ kE_z * aes_z + age_z + gender + bmi_z,
                        family = tweedie(link = "log"),
                        data = combined_data)

coefs <- summary(kE_aes_glm)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# ---- Excluding chronotype study ----
kE_aes_glm_s1 <- glmmTMB(total_MET ~ kE_z * aes_z + age_z + gender + bmi_z,
                           family = tweedie(link = "log"),
                           data = data_excl_chrono)

coefs <- summary(kE_aes_glm_s1)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# ---- Excluding t2d study ----
kE_aes_glm_s2 <- glmmTMB(total_MET ~ kE_z * aes_z + age_z + gender + bmi_z,
                           family = tweedie(link = "log"),
                           data = data_excl_t2d)

coefs <- summary(kE_aes_glm_s2)$coefficients$cond

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

### (3) Activity participation -----------------------------------------------

# ---- Full sample ----
### with SHAPS
# Walking
kE_shaps_walk_log <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = combined_data %>% 
    mutate(participated = as.integer(walking_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_walk_log)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Moderate
kE_shaps_mod_log <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = combined_data %>% 
    mutate(participated = as.integer(moderate_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_mod_log)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Vigorous
kE_shaps_vig_log <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = combined_data %>% 
    mutate(participated = as.integer(vigorous_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_vig_log)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

### with AES
# Walking
kE_aes_walk_log <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = combined_data %>% 
    mutate(participated = as.integer(walking_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_walk_log)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Moderate
kE_aes_mod_log <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = combined_data %>% 
    mutate(participated = as.integer(moderate_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_mod_log)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Vigorous
kE_aes_vig_log <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = combined_data %>% 
    mutate(participated = as.integer(vigorous_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_vig_log)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

#---- Excluding chronotype sample ----
### with SHAPS
# Walking
kE_shaps_walk_log_s1 <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% 
    mutate(participated = as.integer(walking_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_walk_log_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Moderate
kE_shaps_mod_log_s1 <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% 
    mutate(participated = as.integer(moderate_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_mod_log_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Vigorous
kE_shaps_vig_log_s1 <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% 
    mutate(participated = as.integer(vigorous_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_vig_log_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

### with AES
# Walking
kE_aes_walk_log_s1 <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% 
    mutate(participated = as.integer(walking_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_walk_log_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Moderate
kE_aes_mod_log_s1 <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% 
    mutate(participated = as.integer(moderate_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_mod_log_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Vigorous
kE_aes_vig_log_s1 <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% 
    mutate(participated = as.integer(vigorous_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_vig_log_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

#---- Excluding t2d sample ----
### with SHAPS
# Walking
kE_shaps_walk_log_s2 <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% 
    mutate(participated = as.integer(walking_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_walk_log_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Moderate
kE_shaps_mod_log_s2 <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% 
    mutate(participated = as.integer(moderate_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_mod_log_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Vigorous
kE_shaps_vig_log_s2 <- glm(
  participated ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% 
    mutate(participated = as.integer(vigorous_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_shaps_vig_log_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

### with AES
# Walking
kE_aes_walk_log_s2 <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% 
    mutate(participated = as.integer(walking_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_walk_log_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Moderate
kE_aes_mod_log_s2 <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% 
    mutate(participated = as.integer(moderate_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_mod_log_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)

# Vigorous
kE_aes_vig_log_s2 <- glm(
  participated ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% 
    mutate(participated = as.integer(vigorous_MET_min > 0)),
  family = binomial(link = "logit")
)

coefs <- summary(kE_aes_vig_log_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|z|)"]
)


### (4) Activity amount -----------------------------------------------

#---- Full sample ----
### with SHAPS
# Walking
kE_shaps_walk_glm <- glm(
  walking_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = combined_data %>% filter(walking_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_walk_glm)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Moderate
kE_shaps_mod_glm <- glm(
  moderate_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = combined_data %>% filter(moderate_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_mod_glm)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Vigorous
kE_shaps_vig_glm <- glm(
  vigorous_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = combined_data %>% filter(vigorous_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_vig_glm)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

### with AES
# Walking
kE_aes_walk_glm <- glm(
  walking_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = combined_data %>% filter(walking_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_walk_glm)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Moderate
kE_aes_mod_glm <- glm(
  moderate_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = combined_data %>% filter(moderate_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_mod_glm)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Vigorous
kE_aes_vig_glm <- glm(
  vigorous_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = combined_data %>% filter(vigorous_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_vig_glm)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)


#---- Excluding chronotype sample ----
### with SHAPS
# Walking
kE_shaps_walk_glm_s1 <- glm(
  walking_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% filter(walking_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_walk_glm_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Moderate
kE_shaps_mod_glm_s1 <- glm(
  moderate_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% filter(moderate_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_mod_glm_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Vigorous
kE_shaps_vig_glm_s1 <- glm(
  vigorous_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% filter(vigorous_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_vig_glm_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

### with AES
# Walking
kE_aes_walk_glm_s1 <- glm(
  walking_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% filter(walking_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_walk_glm_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Moderate
kE_aes_mod_glm_s1 <- glm(
  moderate_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% filter(moderate_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_mod_glm_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Vigorous
kE_aes_vig_glm_s1 <- glm(
  vigorous_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_chrono %>% filter(vigorous_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_vig_glm_s1)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

#---- Excluding t2d sample ----
### with SHAPS
# Walking
kE_shaps_walk_glm_s2 <- glm(
  walking_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% filter(walking_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_walk_glm_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Moderate
kE_shaps_mod_glm_s2 <- glm(
  moderate_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% filter(moderate_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_mod_glm_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Vigorous
kE_shaps_vig_glm_s2 <- glm(
  vigorous_MET_min ~ kE_z + shaps_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% filter(vigorous_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_shaps_vig_glm_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

### with AES
# Walking
kE_aes_walk_glm_s2 <- glm(
  walking_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% filter(walking_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_walk_glm_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Moderate
kE_aes_mod_glm_s2 <- glm(
  moderate_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% filter(moderate_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_mod_glm_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)

# Vigorous
kE_aes_vig_glm_s2 <- glm(
  vigorous_MET_min ~ kE_z + aes_z + age_z + gender + bmi_z,
  data = data_excl_t2d %>% filter(vigorous_MET_min > 0),
  family = Gamma(link = "log")
)

coefs <- summary(kE_aes_vig_glm_s2)$coefficients

data.frame(
  beta = coefs[, "Estimate"],
  lower_95 = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  upper_95 = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
  p = coefs[, "Pr(>|t|)"]
)