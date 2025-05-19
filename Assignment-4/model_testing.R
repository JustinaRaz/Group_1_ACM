pacman::p_load("tidyverse", "ggplot2", "cmdstanr", "brms", "tidyr", "dplyr", 
               "stringr", "posterior", "bayesplot")

setwd("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-4")
source("u_func.R")

# Priors:
c_priors <- c(0,1)
w_priors <- rep(1/5,5)

#Load the data:
sim_data <- read_csv("data/informed_c1.csv")
emp_data <- read.table("data/AlienData.txt", header = TRUE, sep = ",")

data_l <- assignment_4_data(data_split = sim_data,
                            c_priors = c_priors,
                            w_priors = w_priors)


#============================= Fitting Stan model: SIMULATED DATA

file <- file.path("model/a4_jus.stan")

# Compile the model
mod <- cmdstan_model(file, 
                     cpp_options = list(stan_threads = TRUE), 
                     stanc_options = list("O1")) 

time_1 <-   Sys.time()

# Model fitting: data simulation

samples <- mod$sample(
  data = data_l, 
  seed = 123, 
  chains = 4,
  parallel_chains = 4,
  threads_per_chain = 2,
  iter_warmup = 1000,
  iter_sampling = 2000, 
  refresh = 100,
  max_treedepth = 20,
  adapt_delta = 0.99,
  init = 0,
)

saveRDS(samples, file = "data/samples_simulated.rds")

# Traceplots for simulated data

draws_sim <- samples$draws(
  variables = c("c", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]"),
  format = "draws_array"
)

traceplot_sim <- mcmc_trace(draws_sim, facet_args = list(ncol = 2)) +
  ggtitle("Simulated Data - Traceplot") +
  theme_light(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("plots/traceplot_simulated.png", traceplot_sim, width = 12, height = 8)


# Visualization
estimates <- samples$draws(
  variables = c("w_prior[1]","w_prior[2]", "w_prior[3]", "w_prior[4]", "w_prior[5]", "c_prior", 
                "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "c"),
  inc_warmup = FALSE,
  format = "df"
)

estimates_long <- estimates %>%
  select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "value") %>%
  mutate(
    group = case_when(
      grepl("w_prior\\[(\\d+)\\]", parameter) ~ paste0("w[", sub(".*\\[(\\d+)\\]", "\\1", parameter), "]"),
      grepl("w\\[(\\d+)\\]", parameter) ~ paste0("w[", sub(".*\\[(\\d+)\\]", "\\1", parameter), "]"),
      parameter %in% c("c", "c_prior") ~ "c",
      TRUE ~ parameter
    ),
    type = case_when(
      grepl("prior", parameter) ~ "Prior",
      TRUE ~ "Posterior"
    )
  )

vline_data <- tibble(
  group = c("w[1]", "w[2]", "w[3]", "w[4]", "w[5]"),
  xintercept = c(0.3, 0.1333, 0.3, 0.1333, 0.1333)
)

#=========== Plotting densities separately to be able to adjust y axis only for weights

c_s <- estimates_long %>%
  filter(parameter %in% c("c", "c_prior"))

estimates_long_no_c_sim <- estimates_long %>%
  filter(!(parameter %in% c("c", "c_prior")))


plot_c_s <- ggplot(c_s, aes(x = value, fill = type)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", linewidth = 0.5) +
  labs(title = "c", x = "Value", y = "Density", color = "Group", fill = "") +
  theme_classic() +
  scale_fill_manual(values = c("Prior" = "steelblue", "Posterior" = "darkorange")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("plots/c_simulated.png", plot = plot_c_s, width = 7, height = 5)

plot_weights_s <- ggplot(estimates_long_no_c_sim, aes(x = value, fill = type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~group, scales = "fixed") +
  ylim(c(0, 15)) +
  geom_vline(data = vline_data, aes(xintercept = xintercept), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = c("Prior" = "steelblue", "Posterior" = "darkorange")) +
  labs(x = "Value", y = "Density", fill = "") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("plots/weigths_simulated.png", plot = plot_weights_s, width = 7, height = 5)

# ------------------------------ Running the model on the empirical data:

emp_data <- emp_data[emp_data$test != 1, ]
colnames(emp_data)[colnames(emp_data) == "response"] <- "sim_response" # This just renames the response column - to be suitable with the function

# Choosing 5 first participants from session 1:

f_data <- emp_data %>%
  filter(subject %in% c("1", "2", "3", "4", "5"), session == 1) %>%
  filter(condition == 2)

# Separating "stimulus" column into 5 separate column (to make it like in the simulated data):

f_data <- f_data %>%
  mutate(stimulus_clean = str_remove(stimulus, ".jpg")) %>%  
  mutate(stimulus_split = str_split(stimulus_clean, "", simplify = TRUE)) %>%  
  mutate(
    f1 = as.integer(stimulus_split[, 1]),
    f2 = as.integer(stimulus_split[, 2]),
    f3 = as.integer(stimulus_split[, 3]),
    f4 = as.integer(stimulus_split[, 4]),
    f5 = as.integer(stimulus_split[, 5])
  ) %>%
  select(-stimulus_clean, -stimulus_split)

# Adjust response column:

f_data <- f_data %>%
  mutate(sim_response = case_when(
    sim_response == 3 ~ 1,
    sim_response == 4 ~ 1,
    sim_response == 1 ~ 0,
    sim_response == 2 ~ 0
  ))

# Adjust as the simulated data was adjusted:

data_l_emp <- assignment_4_data(data_split = f_data,
                            c_priors = c_priors,
                            w_priors = w_priors)

# Model fitting: empirical data

samples_emp <- mod$sample(
  data = data_l_emp, 
  seed = 123, 
  chains = 4,
  parallel_chains = 4,
  threads_per_chain = 2,
  iter_warmup = 1000,
  iter_sampling = 2000, 
  refresh = 100,
  max_treedepth = 20,
  adapt_delta = 0.99,
  init = 0,
)

saveRDS(samples_emp, file = "data/samples_empirical.rds")

# Traceplot for empirical data
draws_emp <- samples_emp$draws(
  variables = c("c", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]"),
  format = "draws_array"
)

traceplot_emp <- mcmc_trace(draws_emp, facet_args = list(ncol = 2)) +
  ggtitle("Empirical Data - Traceplot") +
  theme_light(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("plots/traceplot_empirical.png", traceplot_emp, width = 12, height = 8)

# Visualization
estimates_emp <- samples_emp$draws(
  variables = c("w_prior[1]","w_prior[2]", "w_prior[3]", "w_prior[4]", "w_prior[5]", "c_prior", 
                "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "c"),
  inc_warmup = FALSE,
  format = "df"
)

estimates_long_emp <- estimates_emp %>%
  select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "value") %>%
  mutate(
    group = case_when(
      grepl("w_prior\\[(\\d+)\\]", parameter) ~ paste0("w[", sub(".*\\[(\\d+)\\]", "\\1", parameter), "]"),
      grepl("w\\[(\\d+)\\]", parameter) ~ paste0("w[", sub(".*\\[(\\d+)\\]", "\\1", parameter), "]"),
      parameter %in% c("c", "c_prior") ~ "c",
      TRUE ~ parameter
    ),
    type = case_when(
      grepl("prior", parameter) ~ "Prior",
      TRUE ~ "Posterior"
    )
  )

#=========== Plotting densities separately to be able to adjust y axis only for weights

c_e <- estimates_long_emp %>%
  filter(parameter %in% c("c", "c_prior"))

estimates_long_no_c_emp <- estimates_long_emp %>%
  filter(!(parameter %in% c("c", "c_prior")))


plot_c_e <- ggplot(c_e, aes(x = value, fill = type)) +
  geom_density(alpha = 0.6) +
  labs(title = "c", x = "Value", y = "Density", color = "Group", fill = "") +
  theme_classic() +
  scale_fill_manual(values = c("Prior" = "steelblue", "Posterior" = "darkorange")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("plots/c_empirical.png", plot = plot_c_e, width = 7, height = 5)

plot_weights_e <- ggplot(estimates_long_no_c_emp, aes(x = value, fill = type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~group, scales = "fixed") +
  ylim(c(0, 15)) +
  scale_fill_manual(values = c("Prior" = "steelblue", "Posterior" = "darkorange")) +
  labs(x = "Value", y = "Density", fill = "") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("plots/weigths_empirical.png", plot = plot_weights_e, width = 7, height = 5)

# Function used in ppchecks:

set.seed(123)

untangle_estimates <- function(df, n_subj, nturn){
  
  df_2 <- df 
  #filter by turn
  df_slice <- df_2 %>% 
    pivot_longer(cols = seq(1,nturn*n_subj,by = 1))
  df_slice <- df_slice %>% 
    #turn is derived from index so "..[1,12]" is the value for subject 1 at turn 12
    mutate(turn_ID = as.integer(str_extract(name,"(\\d+)(?!.*\\d)")),
           ID = as.integer(str_extract(str_extract(name,"^\\D*(\\d*)"),
                                       "\\d+"))
    )
  
  return(df_slice)
}


prior_predictions <- samples_emp$draws(
  variables = "prior_preds",
  inc_warmup = FALSE,
  format = "df"
)

pp_vis <- untangle_estimates(prior_predictions, n_subj = 5, nturn = 96)

pp_vis %>% 
  group_by(name) %>% 
  reframe(sample(value,1))

pp_vis <- pp_vis %>% 
  group_by(name,ID, turn_ID,) %>% 
  reframe(draw = sample(value,1))
pp_vis$category <- as.vector(data_l$cat_one)
pp_vis <- pp_vis %>% 
  mutate(correct = ifelse(category == draw,1,0) )

break_l_1 = seq(0,1, by = 0.2)
break_l_2 = break_l_1 *  96

pp_vis %>% 
  ggplot(aes(x = correct, fill = as.factor(correct))) +
  geom_histogram(alpha = 0.5, stat = "count", bins = 2)+
  facet_wrap(~ID)+
  ggtitle("Prior Predictions\n% of correct categorization  (blue: correct)") +
  geom_hline(yintercept = 96/2, linetype = 2) +
  scale_y_continuous(breaks = break_l_2,
                     labels =  break_l_1 *100) +
  scale_x_discrete(breaks = c(0,1)) +
  scale_fill_manual(values = c("darkorange","steelblue"),
                    guide="none") +
  ylab("") +
  xlab("") +
  theme_classic()

#######################################
posterior_predictions <- samples_emp$draws(
  variables = "posterior_preds",
  inc_warmup = FALSE,
  format = "df"
)


pp_2_vis <- untangle_estimates(posterior_predictions, n_subj = 5, nturn = 96)

pp_2_vis <- pp_2_vis %>% 
  group_by(name,ID, turn_ID,) %>% 
  reframe(draw = sample(value,1))
pp_2_vis$category <- as.vector(data_l$cat_one)
pp_2_vis <- pp_2_vis %>% 
  mutate(correct = ifelse(category == draw,1,0) )

break_l_1 = seq(0,1, by = 0.2)
break_l_2 = break_l_1 *  96
pp_2_vis %>% 
  ggplot(aes(x = correct, fill = as.factor(correct))) +
  geom_histogram(alpha = 0.5, stat = "count", bins = 2)+
  facet_wrap(~ID)+
  ggtitle("Posterior Predictions\n% of correct categorization  (blue: correct)") +
  geom_hline(yintercept = 96/2, linetype = 2) +
  scale_y_continuous(breaks = break_l_2,
                     labels =  break_l_1 *100) +
  scale_x_discrete(breaks = c(0,1)) +
  scale_fill_manual(values = c("darkorange","steelblue"),
                    guide="none") +
  ylab("") +
  xlab("") +
  theme_classic()