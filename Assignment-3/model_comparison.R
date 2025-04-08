library(loo)

setwd("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-3")
source("u_func.R")
source("comparison_functions.R")


#------------------------------------------------ Simulate data/load data:

n_subj <- 10
n_image <- 150
sim_dat <- n_subj_blank_slate_trust(n_subj = n_subj,
                                    n_image = n_image)
data_l <- df_data_to_list(sim_dat)

# Loading the data
df <- read_csv("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-3/data/Simonsen_clean.csv")

# Adjusting the type of variables
df$ID <- as.integer(df$ID)
df$FaceID <- as.integer(df$FaceID)
df$FirstRating <- as.integer(df$FirstRating)
df$GroupRating <- as.integer(df$GroupRating )
df$SecondRating <- as.integer(df$SecondRating)

# Adjusting column names
names(df)[1] <- "ID"
names(df)[2] <- "FACE_ID"
names(df)[3] <- "F_R"
names(df)[4] <- "G_R"
names(df)[5] <- "S_R"

# Selecting only necessary data
df <- df[, 1:5]

# Transforming df to a list
df <- df_data_to_list(df)

df_priors <- append(df, c(prior_a_shape = 6, prior_a_rate=2))

#---------------------------------------- Load the models for comparison:

model_1 <- file.path("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-3/models/blank_slate_beta.stan")
model_2 <- file.path("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-3/prior_trust_beta.stan")
model_test <- file.path("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-3/test_model_comparison_2.stan")

# Compile the model


mod_1 <- compile_model(model_1)
mod_2 <- compile_model(model_2)
mod_test <- compile_model(model_test)


#---------------------------------------- Call Stan with specific options:

samples_1 <- stan_sampling(model = mod_1, data = df) #n of chains = n of parallel chains.

write_rds(samples_1, "samples_m1.rds")

samples_2 <- mod_2$sample(
  data = df_priors,
  seed = 123,
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

write_rds(samples_2, "samples_m2.rds")

samples_test <- stan_sampling(model = mod_test, data = df) #n of chains = n of parallel chains.

#------------------------- Traceplots

draws <- samples_2$draws(
  variables = c("prior_alpha", "F_R_p_alpha"),
  inc_warmup = FALSE,
  format = "df"
)

# prior alpha

var_name <- "prior_alpha[1]"

df_trace <- prior_posterior_updates %>%
  select(value = all_of(var_name), iteration = .iteration, chain = .chain)

# Plot the trace plot
ggplot(df_trace, aes(x = iteration, y = value, color = factor(chain))) +
  geom_line(alpha = 0.8) +
  labs(
    title = "Trace Plot for prior_alpha[1]",
    x = "Iteration",
    y = "Sampled Value",
    color = "Chain"
  ) +
  theme_minimal()

var_name <- "prior_alpha[2]"

df_trace <- prior_posterior_updates %>%
  select(value = all_of(var_name), iteration = .iteration, chain = .chain)

# Plot the trace plot
ggplot(df_trace, aes(x = iteration, y = value, color = factor(chain))) +
  geom_line(alpha = 0.8) +
  labs(
    title = "Trace Plot for prior_alpha[2]",
    x = "Iteration",
    y = "Sampled Value",
    color = "Chain"
  ) +
  theme_minimal()

var_name <- "F_R_p_alpha[1,1]"

df_trace <- prior_posterior_updates %>%
  select(value = all_of(var_name), iteration = .iteration, chain = .chain)

# Plot the trace plot
ggplot(df_trace, aes(x = iteration, y = value, color = factor(chain))) +
  geom_line(alpha = 0.8) +
  labs(
    title = "Trace Plot for F_R_p_alpha[1,1]",
    x = "Iteration",
    y = "Sampled Value",
    color = "Chain"
  ) +
  theme_minimal()

var_name <- "F_R_p_alpha[1,2]"

df_trace <- prior_posterior_updates %>%
  select(value = all_of(var_name), iteration = .iteration, chain = .chain)

# Plot the trace plot
ggplot(df_trace, aes(x = iteration, y = value, color = factor(chain))) +
  geom_line(alpha = 0.8) +
  labs(
    title = "Trace Plot for F_R_p_alpha[1,2]",
    x = "Iteration",
    y = "Sampled Value",
    color = "Chain"
  ) +
  theme_minimal()


 #---------------------------------- Compute LOO for each model and scenario:

loo_m1 <- compute_loo(samples_1)
loo_m2 <- compute_loo(samples_2)
loo_test <- compute_loo(samples_test)

# Compare models
loo_comparison <- loo_compare(loo_m1, loo_test)
print(loo_comparison)

comparison <- compare_scenario_models(loo_m1, loo_test, "Comparison")

#----------------------------- Checking the reliability of our LOO estimates:

diagnostics <- bind_rows(
  check_pareto_k(loo_m1, "Blank slate beta model"),
  check_pareto_k(loo_test, "Test model")
)

#----------------------------- Display diagnostics table:

knitr::kable(diagnostics, 
             digits = 3,
             caption = "PSIS-LOO Reliability Diagnostics")
