pacman::p_load("tidyverse", "ggplot2", "cmdstanr", "brms", "tidyr", "dplyr", "stringr")

setwd("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-4")

assignment_4_data <- function(data_split, w_priors, c_priors, response){
  
  current_split <- data_split
    
  
  ##### restructure obs
  template = array(NA,c(5,96,5))
  ### stim_id
  f1 = t(matrix(as.integer(current_split$f1),
                nrow = length(unique(current_split$trial))))
  f2 = t(matrix(as.integer(current_split$f2),
                nrow = length(unique(current_split$trial))))
  f3 = t(matrix(as.integer(current_split$f3),
                nrow = length(unique(current_split$trial))))
  f4 = t(matrix(as.integer(current_split$f4),
                nrow = length(unique(current_split$trial))))
  f5 = t(matrix(as.integer(current_split$f5),
                nrow = length(unique(current_split$trial))))
  ### 
  for (k in 1:5){
    for (i in 1:96){
      template[k,i, 1] = f1[k,i]
      template[k,i, 2] = f2[k,i]
      template[k,i, 3] = f3[k,i]
      template[k,i, 4] = f4[k,i]
      template[k,i, 5] = f5[k,i]
    }
  }
  
  data_l <- list(
    n_subj = 5,
    ntrials = 96,
    nfeatures = 5,
    cat_one = t(matrix(as.integer(current_split$dangerous),
                       nrow = length(unique(current_split$trial)))),
    y = t(matrix(as.integer(current_split$response),
                 nrow = length(unique(current_split$trial)))),
    obs = template,
    w_prior_values = w_priors,
    c_prior_values = c_priors)
  
  return(data_l)
}

c_priors <- c(0,1)
w_priors <- rep(1/5,5)
data_l <- assignment_4_data(data_split = informed_c1,
                            c_priors = c_priors,
                            w_priors = w_priors,
                            response = sim_response)


###################### stan it up
file <- file.path("a4.stan")

# Compile the model
mod <- cmdstan_model(file, 
                     # this specifies we can parallelize the gradient estimations on multiple cores
                     cpp_options = list(stan_threads = TRUE), 
                     # this is a trick to make it faster
                     stanc_options = list("O1")) 

# The following command calls Stan with specific options.
time_1 <-   Sys.time()

samples <- mod$sample(
  data = data_l, # the data :-)
  seed = 123,  # a seed, so I always get the same results
  chains = 4,  # how many chains should I fit (to check whether they give the same results)
  parallel_chains = 4, # how many of the chains can be run in parallel?
  threads_per_chain = 2, # distribute gradient estimations within chain across multiple cores
  iter_warmup = 1000,  # warmup iterations through which hyperparameters (steps and step length) are adjusted
  iter_sampling = 2000, # total number of iterations
  refresh = 100,
  # how often to show that iterations have been run
  #output_dir = "simmodels", # saves the samples as csv so it can be later loaded
  max_treedepth = 20, # how many steps in the future to check to avoid u-turns
  adapt_delta = 0.99,
  init = 0,# how high a learning rate to adjust hyperparameters during warmup
)


# Visualization
estimates <- samples$draws(
  variables = c("w_prior[1]","w_prior[2]", "w_prior[3]", "w_prior[4]", "w_prior[5]", "c_prior", 
                "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "c"),
  inc_warmup = FALSE,
  format = "df"
)

estimates$c <- estimates$c/2

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
  group = c("w[1]", "w[3]", "w[2]", "w[5]", "w[4]", "c"),
  xintercept = c(3/9, 3/9, 1/9, 1/9, 1/9, inv_logit_scaled(1))
)

ggplot(estimates_long, aes(x = value, fill = type)) +
  geom_density(alpha = 0.6) +
  geom_vline(data = vline_data, aes(xintercept = xintercept), 
             color = "red", linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~group, scales = "free") +
  scale_fill_manual(values = c("Prior" = "steelblue", "Posterior" = "darkorange")) +
  labs(x = "Value", y = "Density", fill = "") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# ------------------------------ Running the model on the empirical data:

data <- read.table("AlienData.txt", header = TRUE, sep = ",")

data <- data[data$test != 1, ]

# Choosing 5 first participants from session 1:

f_data <- data %>%
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
  mutate(response = case_when(
    response == 3 ~ 1,
    response == 4 ~ 1,
    response == 1 ~ 0,
    response == 2 ~ 0
  ))

# Adjust as the simulated data was adjusted:

data_l_real <- assignment_4_data(data_split = f_data,
                            c_priors = c_priors,
                            w_priors = w_priors,
                            response = response)

samples_real <- mod$sample(
  data = data_l_real, # the data :-)
  seed = 123,  # a seed, so I always get the same results
  chains = 4,  # how many chains should I fit (to check whether they give the same results)
  parallel_chains = 4, # how many of the chains can be run in parallel?
  threads_per_chain = 2, # distribute gradient estimations within chain across multiple cores
  iter_warmup = 1000,  # warmup iterations through which hyperparameters (steps and step length) are adjusted
  iter_sampling = 2000, # total number of iterations
  refresh = 100,
  # how often to show that iterations have been run
  #output_dir = "simmodels", # saves the samples as csv so it can be later loaded
  max_treedepth = 20, # how many steps in the future to check to avoid u-turns
  adapt_delta = 0.99,
  init = 0,# how high a learning rate to adjust hyperparameters during warmup
)

saveRDS(samples_real, file = "samples_real.rds")


#samples_real <- readRDS("samples_real.rds")

