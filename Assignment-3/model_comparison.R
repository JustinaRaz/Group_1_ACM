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

df <- read_csv("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-3/data/Simonsen_clean.csv")

df$ID <- as.integer(df$ID)
df$FaceID <- as.integer(df$FaceID)
df$FirstRating <- as.integer(df$FirstRating)
df$GroupRating <- as.integer(df$GroupRating )
df$SecondRating <- as.integer(df$SecondRating)

names(df)[1] <- "ID"
names(df)[2] <- "FACE_ID"
names(df)[3] <- "F_R"
names(df)[4] <- "G_R"
names(df)[5] <- "S_R"


df <- df[, 1:5]

df <- df_data_to_list(df)

#---------------------------------------- Load the models for comparison:

model_1 <- file.path("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-3/models/blank_slate_beta.stan")
model_2 <- file.path("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-3/prior_trust_beta.stan")

# Compile the model


mod_1 <- compile_model(model_1)
mod_2 <- compile_model(model_2)


#---------------------------------------- Call Stan with specific options:

samples_1 <- stan_sampling(model = mod_1, data = df) #n of chains = n of parallel chains.
df_priors <- append(df, c(prior_a_shape = 6, prior_a_rate=2))
samples_2 <- stan_sampling(model = mod_2, data = df_priors)

 #---------------------------------- Compute LOO for each model and scenario:

loo_m1 <- compute_loo(samples_1)
loo_m2 <- compute_loo(samples_2)

loo_weighted_m1 <- compute_loo(samples_1)
loo_weighted_m2 <- compute_loo(samples_2)

# Compare models
loo_comparison <- loo_compare(loo_simple, loo_weighted)
print(loo_comparison)

#----------------------------- Checking the reliability of our LOO estimates:

diagnostics <- bind_rows(
  check_pareto_k(loo_m1, "Simple - M1"),
  check_pareto_k(loo_m2, "Simple - M2"),
  check_pareto_k(loo_weighted_m1, "Weighted - M1"),
  check_pareto_k(loo_weighted_m2, "Weighted - M2")
)

#----------------------------- Display diagnostics table:

knitr::kable(diagnostics, 
             digits = 3,
             caption = "PSIS-LOO Reliability Diagnostics")

#----------------------------- Perform comparisons for each scenario:

comparison_m1 <- compare_scenario_models(
  loo_m1, loo_weighted_m1, "M1"
)

comparison_m2 <- compare_scenario_models(
  loo_m2, loo_weighted_m2, "M2"
)

#----------------------------- Combine comparison results:

both_comparisons <- bind_rows(
  comparison_m1$comparison,
  comparison_m2$comparison
)

both_weights <- bind_rows(
  comparison_m1$weights,
  comparison_m2$weights
)

#----------------------------- Plots:

p1 <- plot_elpd_differences(both_comparisons) # ELPD differences
p2 <- plot_weights(both_weights) # model weights

#----------------------------- Create a summary table of results:

summary_table <- both_weights %>%
  pivot_wider(names_from = model, values_from = weight) %>%
  mutate(
    winning_model = case_when(
      `Simple Bayesian` > `Weighted Bayesian` ~ "Simple Bayesian",
      `Weighted Bayesian` > `Simple Bayesian` ~ "Weighted Bayesian",
      TRUE ~ "Tie"
    ),
    weight_difference = abs(`Simple Bayesian` - `Weighted Bayesian`),
    evidence_strength = case_when(
      weight_difference < 0.1 ~ "Weak",
      weight_difference < 0.3 ~ "Moderate",
      weight_difference < 0.6 ~ "Strong",
      TRUE ~ "Very Strong"
    )
  )

#----------------------------- Display summary table:

knitr::kable(summary_table, 
             digits = 3,
             caption = "Summary of Model Comparison Results")

#--------------------------------------------------------------  Comparison code from section 11.22

loo_m1_compare <- samples_1$loo()
loo_m2_compare <- samples_2$loo()

# Compare models
loo_comparison <- loo_compare(loo_m1_compare, loo_m2_compare)
print(loo_comparison)

# Calculate model weights
model_weights <- loo_model_weights(list(
  "M1" = loo_m1_compare,
  "M2" = loo_m2_compare
))

# Print model weights
print(model_weights)

# Create a visualization of model comparison
model_comp_data <- tibble(
  model = names(model_weights),
  weight = as.numeric(model_weights)
)

p_model_comp <- ggplot(model_comp_data, aes(x = model, y = weight, fill = model)) +
  geom_col() +
  geom_text(aes(label = scales::percent(weight, accuracy = 0.1)), 
            vjust = -0.5, size = 5) +
  labs(
    title = "Model Comparison Using LOO-CV",
    subtitle = "Higher weights indicate better predictive performance",
    x = NULL,
    y = "Model Weight"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_model_comp)


# Compare model performance by agent type

# Calculate pointwise ELPD values for each model
elpd_m1 <- loo_m1_compare$pointwise[, "elpd_loo"]
elpd_m2 <- loo_m2_compare$pointwise[, "elpd_loo"]

# Aggregate by agent
elpd_by_agent <- multilevel_sim_data %>%
  dplyr::select(agent_id, model_type) %>%
  distinct() %>%
  mutate(
    elpd_simple = NA_real_,
    elpd_weighted = NA_real_
  )

# Calculate ELPD sums by agent
for (j in 1:nrow(elpd_by_agent)) {
  agent <- elpd_by_agent$agent_id[j]
  # Find rows for this agent
  agent_rows <- which(multilevel_sim_data$agent_id == agent)
  # Sum ELPD values for this agent
  elpd_by_agent$elpd_simple[j] <- sum(elpd_simple[agent_rows])
  elpd_by_agent$elpd_weighted[j] <- sum(elpd_weighted[agent_rows])
}

# Calculate ELPD difference (positive = weighted model is better)
elpd_by_agent <- elpd_by_agent %>%
  mutate(
    elpd_diff = elpd_weighted - elpd_simple,
    better_model = ifelse(elpd_diff > 0, "Weighted", "Simple")
  )

# Create visualization of model preference by agent type

p_agent_comp <- ggplot(elpd_by_agent, aes(x = agent_id, y = elpd_diff, color = model_type)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Model Preference by Agent Type",
    subtitle = "Positive values favor the weighted model; negative values favor the simple model",
    x = "True Agent Type",
    y = "ELPD Difference (Weighted - Simple)",
    color = "Preferred Model"
  ) +
  theme_minimal()


print(p_agent_comp)