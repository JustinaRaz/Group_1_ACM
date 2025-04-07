library(loo)
library(ggplot2)

# ---------------- Function to compile the model

compile_model <- function(model_path) {
  cmdstanr::cmdstan_model(model_path,
                          cpp_options = list(stan_threads = TRUE),
                          stanc_options = list("O1"))
}

# ---------------- Function to run sampling

stan_sampling <- function(model,
                         data,
                         seed = 123,
                         chains = 1,
                         parallel_chains = NULL,
                         threads_per_chain = 1,
                         iter_warmup = 0,
                         iter_sampling = 100,
                         refresh = 200,
                         fixed_param = TRUE,
                         adapt_engaged = 0) {
  
  if (is.null(parallel_chains)) {
    parallel_chains <- chains
  }
  
  model$sample(
    data = data,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = refresh,
    fixed_param = fixed_param,
    adapt_engaged = adapt_engaged
  )
}

# ---------------- Function to extract log-likelihood and compute LOO

compute_loo <- function(model_fit) {
  # Extract log-likelihood values
  log_lik <- model_fit$draws("log_lik", format = "matrix")
  
  # Compute LOO-CV using PSIS
  loo_result <- loo(log_lik)
  return(loo_result)
}

# ---------------- Function to check Pareto k diagnostics

check_pareto_k <- function(loo_result, model_name) {
  # Extract Pareto k values
  pareto_k <- loo_result$diagnostics$pareto_k
  
  # Count problematic k values
  n_k_high <- sum(pareto_k > 0.7)
  n_k_medium <- sum(pareto_k > 0.5 & pareto_k <= 0.7)
  
  # Proportion of problematic observations
  prop_problematic <- (n_k_high + n_k_medium) / length(pareto_k)
  
  # Create diagnostic summary
  summary_df <- tibble(
    model = model_name,
    total_obs = length(pareto_k),
    k_high = n_k_high,
    k_medium = n_k_medium,
    prop_problematic = prop_problematic,
    reliability = case_when(
      prop_problematic == 0 ~ "Excellent",
      prop_problematic < 0.05 ~ "Good",
      prop_problematic < 0.1 ~ "Fair",
      TRUE ~ "Poor"
    )
  )
  
  return(summary_df)
}


#  ---------------- Function to compare models and create visualization

compare_scenario_models <- function(loo_simple, loo_weighted, scenario_name) {
  # Compare models
  comparison <- loo_compare(loo_simple, loo_weighted)
  
  # Calculate model weights
  weights <- loo_model_weights(list(
    "Simple Bayesian" = loo_simple,
    "Weighted Bayesian" = loo_weighted
  ))
  
  # Print comparison
  cat("\nModel comparison for", scenario_name, "scenario:\n")
  print(comparison)
  
  # Print weights
  cat("\nModel weights for", scenario_name, "scenario:\n")
  print(weights)
  
  # Create comparison dataframe
  comparison_df <- as.data.frame(comparison)
  comparison_df$model <- rownames(comparison_df)
  rownames(comparison_df) <- NULL
  comparison_df$scenario <- scenario_name
  
  # Create weights dataframe
  weights_df <- tibble(
    model = names(weights),
    weight = as.numeric(weights),
    scenario = scenario_name
  )
  
  # Return both dataframes
  return(list(comparison = comparison_df, weights = weights_df))
}

#  ---------------- Function to plot ELPD differences

plot_elpd_differences <- function(data) {
  
  ggplot(data, 
         aes(x = model, y = elpd_diff, fill = model)) +
    geom_col() +
    geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                      ymax = elpd_diff + se_diff), 
                  width = 0.2) +
    facet_wrap(~ scenario, scales = "free_y") +
    labs(
      title = "Model Comparison via LOO-CV",
      subtitle = "Higher ELPD difference is better; error bars show ±1 SE",
      x = NULL,
      y = "ELPD Difference"
    ) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#  ---------------- Function to plot model weights

plot_weights <- function(data) {
  ggplot(data, 
         aes(x = model, y = weight, fill = model)) +
    geom_col() +
    geom_text(aes(label = scales::percent(weight, accuracy = 0.1)), 
              vjust = -0.5, size = 4) +
    facet_wrap(~ scenario) +
    labs(
      title = "Model Weights Based on LOO-CV",
      subtitle = "Higher weights indicate better predictive performance",
      x = NULL,
      y = "Model Weight"
    ) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ylim(0, 1)
}

