##############################################################################################
#######################---------------- MODEL FITTING ----------------########################
##############################################################################################

### In this script: 
# (0) Set up
# (1) Data preparation
# (2) Load models
# (3) Fit models
# (4) Posterior predictive checks
# (5) Combine parameter estimates with main data

### (0) Set up -----------------------------------------------

setwd(here::here())

# Source helper functions
source("code/functions/model_preprocess_fun.R")
source("code/functions/helper_funs.R")
source("code/functions/model_convergence_check_fun.R")
source("code/functions/parameter_estimates_fun.R")
source("code/functions/extract_posterior_predictions_fun.R")

# load required packages
librarian::shelf(
  ggplot2, tidyverse, here, magrittr, cmdstanr,
  viridis, patchwork
)

# source datasets
data <- readRDS("data/cleaned_data.RDS")

# should the modelling be run from scratch or should saved model fits be loaded?
run_modelling <- FALSE 

### (1) Data preparation -----------------------------------------------

if(run_modelling){
 
  # Transform data for modelling
  task_data <- data$task_data %>% 
    filter(phase == "game") %>% 
    dplyr::select(subj_id, trial:choice) %>% 
    rename(effort_a = effort, amount_a = reward) %>%
    add_column(effort_b = 0, amount_b = 0, 
               .after = "amount_a")
  
  # scale effort and reward
  standardization <- function(x, ref = 0, levels = 1:4){
    (x - ref) / sqrt(mean((levels - mean(levels))^2))
  }
  
  task_data %<>% 
    mutate(effort_a = standardization(effort_a, 0, unique(effort_a)), 
           amount_a = standardization(amount_a, 0, unique(amount_a)))
    
  modelling_data <- model_preprocessing(raw_data = task_data, 
                                        subjs = unique(task_data %>% .$subj_id), 
                                        n_subj = length(unique(task_data %>% .$subj_id)), 
                                        t_subjs = aggregate(trial ~ subj_id, FUN = max, data = task_data)[,2], 
                                        t_max = max(aggregate(trial ~ subj_id, FUN = max, data = task_data)[,2]))
   
}

### (2) Load model -----------------------------------------------

m3_parabolic_stan_model <- cmdstanr::cmdstan_model("code/stan/models_parabolic/ed_m3_parabolic.stan")

### (3) Fit models -----------------------------------------------

if(run_modelling){

  # fit target model (parabolic discounting, 3 free parameters)
  m3_para_fit <- m3_parabolic_stan_model$sample(
    data = modelling_data, 
    refresh = 10, chains = 4, parallel_chains = 4, 
    iter_warmup = 2000, iter_sampling = 2000, 
    seed = 1234,
    adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = FALSE, 
    output_dir = NULL
  )
  
  # Convergence check
  m3_para_check <- convergence_check(m3_para_fit, 
                                     params = c("kE", "kR", "a"), 
                                     Rhat = TRUE, ess = TRUE,
                                     trace_plot = TRUE, rank_hist = FALSE)
  m3_para_check$trace_plot
  saveRDS(list(m3_para_check$Rhat, m3_para_check$ess), 
          here::here("data/model_fits/m3_para_check.RDS"))
  
  ggsave(
    "plots/model_convergence_plots.png",
    plot = m3_para_check$trace_plot,
    width = 12,
    height = 6,
    units = "in"
  )
  
  
  # Parameter estimates
  m3_para_params <- get_params(subj_id = unique(task_data$subj_id), 
                               model_fit = m3_para_fit, 
                               n_subj = length(unique(task_data$subj_id)), 
                               n_params = 3, 
                               param_names = c("kE", "kR", "a"))
  saveRDS(m3_para_params, 
          here::here("data/model_fits/m3_para_params.RDS"))
  
  # Posterior Predictions (for target model only)
  m3_para_PPC_dat <- posterior_predictions(csv_paths = m3_para_fit$output_files(),
                                           n_chains = 4,
                                           n_iter = (2000),
                                           n_subj = length(unique(task_data$subj_id)),
                                           n_trials = 64,
                                           real_dat = task_data) 
  
  saveRDS(m3_para_PPC_dat, 
          here::here("data/model_fits/m3_para_PPC.RDS"))
  
} else {
  
  #load saved model fits and diagnostics
  m3_para_check <- readRDS(here::here("data/model_fits/m3_para_check.RDS"))
  
  m3_para_params <- readRDS(here::here("data/model_fits/m3_para_params.RDS"))
  
  m3_para_PPC_dat <- readRDS(here::here("data/model_fits/m3_para_PPC.RDS"))
  
}


### (4) Posterior predictive checks -----------------------------------------------

# by effort
indiv_plot_effort_dat <- m3_para_PPC_dat$posterior_predictions_effort
indiv_plot_effort_dat$effort_a <- as.factor(indiv_plot_effort_dat$effort_a)

R_squared_effort <- cor(
  indiv_plot_effort_dat$observation,
  indiv_plot_effort_dat$prediction_mean
)^2

# plot
indiv_plot_effort <- ggplot(indiv_plot_effort_dat, aes(x=observation, y=prediction_mean, color=effort_a, shape=effort_a)) +
  geom_point(size=2, alpha=0.35) +
  geom_errorbar(aes(ymin=prediction_hdi_lower, ymax=prediction_hdi_higher), width=.025, alpha=0.1) +
  scale_color_manual(values=viridis::viridis(4), 
                     labels=1:4) + 
  scale_shape_manual(values=c(15, 16, 17, 18), 
                     labels=1:4) + 
  xlim(0,1) + ylim(0,1) +
  geom_abline(linetype = 3) +
  ylab("Predicted (± 95% HDI)") + xlab("Observed") +
  ggtitle(bquote("Across effort levels:"~R^{2}==.(R_squared_effort))) +
  guides(color=guide_legend(title="Effort/Reward level")) +
  guides(shape=guide_legend(title="Effort/Reward level")) +
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  theme(panel.background = element_blank(), axis.line.x = element_blank()) +
  annotate(geom = "segment", y = 0, yend = 1, x = -Inf, xend = -Inf) +
  annotate(geom = "segment", y = -Inf, yend = -Inf, x = 0, xend = 1) 

# by reward
indiv_plot_reward_dat <- m3_para_PPC_dat$posterior_predictions_reward
indiv_plot_reward_dat$amount_a <- as.factor(indiv_plot_reward_dat$amount_a)

R_squared_reward <- cor(
  indiv_plot_reward_dat$observation,
  indiv_plot_reward_dat$prediction_mean
)^2

# plot
indiv_plot_reward <- ggplot(indiv_plot_reward_dat, aes(x=observation, y=prediction_mean, color=amount_a, shape=amount_a)) +
  geom_point(size=2, alpha=0.35) +
  geom_errorbar(aes(ymin=prediction_hdi_lower, ymax=prediction_hdi_higher), width=.025, alpha=0.1) +
  scale_color_manual(values=viridis::viridis(4), 
                     labels=1:4) + 
  scale_shape_manual(values=c(15, 16, 17, 18), 
                     labels=1:4) + 
  xlim(0,1) + ylim(0,1) +
  geom_abline(linetype = 3) +
  ylab("Predicted (± 95% HDI)") + xlab("Observed") +
  ggtitle(bquote("Across reward levels:"~R^{2}==.(R_squared_effort))) +
  guides(color=guide_legend(title="Effort/Reward level")) +
  guides(shape=guide_legend(title="Effort/Reward level")) +
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  theme(panel.background = element_blank(), axis.line.x = element_blank()) +
  annotate(geom = "segment", y = 0, yend = 1, x = -Inf, xend = -Inf) +
  annotate(geom = "segment", y = -Inf, yend = -Inf, x = 0, xend = 1) 


ppc_plot_output <- (indiv_plot_effort | indiv_plot_reward)

# output
ggsave(
  "plots/ppc_plots.png",
  plot = ppc_plot_output,
  width = 10,
  height = 5,
  units = "in"
)

### (5) Combine parameter estimates with main data -----------------------------------------------

# transform individual parameter estimate to a wide format
params_wide <- m3_para_params$individual_params %>%
  filter(parameter %in% c("kE", "kR", "a")) %>%
  pivot_wider(id_cols = subj_id, names_from = parameter, values_from = estimate)

# combine with relevant variables from main dataset
combined_data <- data$demographic_data %>%
  dplyr::select(subj_id, age, gender, sex, mdd_current, mdd_past, study) %>%
  left_join(
    data$questionnaire_data %>%
      dplyr::select(c(subj_id, 
                      ipaq_vigorous_days:ipaq_sitting_min, 
                      vigorous_MET_min:total_MET, ipaq_category,
                      findrisc_sumScore, shaps_sumScore, aes_sumScore, meq_sumScore, bmi)),
    by = "subj_id"
  ) %>%
  left_join(params_wide, by = "subj_id") %>% 
  left_join(data$task_data %>% 
              filter(phase == "game") %>%
              group_by(subj_id) %>% 
              summarise(mean_choice = mean(choice))) 
  
# save combined dataset

saveRDS(combined_data, 
        here::here("data/combined_data.RDS"))










