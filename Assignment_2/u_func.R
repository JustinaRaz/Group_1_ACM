###############
#libraries
library(cmdstanr)
library(LaplacesDemon)
library(stringr)
library(tidyverse)
#Agents
#random agent with bias
favourite_hand_agent <- function(bias,noise){
  
  noise_or_not <- rbinom(1,1,noise)
  
  guess <- ifelse(noise_or_not == 0,
                  rbinom(1,1,bias),
                  rbinom(1,1,0.5)
  )
  
  return(guess)
}
#Reinforcement leearning agent
RL_guessr <- function(prev_outcome,Q_prev,choice_prev,
                      alpha,theta){
  
  #rename for "arm" space: 1 is left, 2 is right
  prev_outcome <- prev_outcome + 1
  #only 1 arm, estimating right
  #so they should be recoded to loss and win in the reward sense
  outcome  <- ifelse(prev_outcome == choice_prev,1,0)
  Qt <- c(NA,NA)
  Q <- c(NA,NA)
  exp_p <- c(NA,NA)
  p <- c(NA,NA)
  #rW: Qt = Qt-1 + a(rt-1 - Qt-1)
  # Rescorla wagner learninig for 2 hands
  for (i in 1:2){
    Qt[i] <- Q_prev[i] + alpha*(outcome - Q_prev[i])
    
    #only previous choice is updated,
    Q[i] <- ifelse(i==choice_prev,
                   Qt[i],
                   Q_prev[i])
    
    exp_p[i] <- exp(theta*Q[i])
  }
  
  for (i in 1:2){
    p[i] <- exp_p[i]/sum(exp_p)
  }
  
  #-1 to return from [1,2] "arm" space to [0,1] left right space
  guess <- rcat(1,p)-1
  
  #order guess, previous Q
  return(c(guess,Q))
  
}
#######################
#data creation and model function
data_model_RL <-function(seed,turns,random_agent_bias,
                         random_agent_noise,
                         RL_alpha,
                         RL_theta,
                         model,
                         chains = 4,
                         cores = 4,
                         iter_w = 1000,
                         iter_s = 2000){
  
  ########################## create data
  set.seed(seed)
  bias <- random_agent_bias
  noise <- random_agent_noise
  theta <- RL_theta
  alpha <- RL_alpha
  
  #initiate game
  r <- array(NA,c(2,turns))
  #hider
  r[1,1] <- favourite_hand_agent(bias,noise)
  #guessr
  Q <- array(NA, c(2,turns))
  Q[1,1] <- 0.5
  Q[2,1] <- 0.5
  r[2,1] <- favourite_hand_agent(0.5,0.5)
  #game
  
  for (t in 2:turns){
    #hider
    r[1,t] <- favourite_hand_agent(bias,noise)
    #guessr
    g_res <- RL_guessr(prev_outcome = r[1,t-1],
                       Q_prev = Q[,t-1],
                       choice_prev = r[2,t-1] + 1,
                       alpha = alpha,
                       theta = theta)
    
    #guessr Q
    r[2,t] <- g_res[1]
    Q[1,t] <- g_res[2]
    Q[2,t] <- g_res[3]
    
  }
  #save data
  data <- list(
    n = turns,
    h = r[2,],
    oh =r[1,],
    k = 2
  )
  
  ####################################### model it
  mod <- model
  # The following command calls Stan with specific options.
  samples <- mod$sample(
    data = data, # the data :-)
    seed = seed,  # a seed, so I always get the same results
    chains = chains,  # how many chains should I fit (to check whether they give the same results)
    parallel_chains = chains, # how many of the chains can be run in parallel?
    threads_per_chain = cores/chains, # distribute gradient estimations within chain across multiple cores
    iter_warmup = iter_w,  # warmup iterations through which hyperparameters (steps and step length) are adjusted
    iter_sampling = iter_s, # total number of iterations
    refresh = 500,  # how often to show that iterations have been run
    #output_dir = "simmodels", # saves the samples as csv so it can be later loaded
    max_treedepth = 20, # how many steps in the future to check to avoid u-turns
    adapt_delta = 0.99, # how high a learning rate to adjust hyperparameters during warmup
  )
  
  
  
  ####################################### parameter extraction
  alpha_prior_RL <-samples$draws(
    variables = "alpha_prior",
    inc_warmup = FALSE,
    format = "df"
  )$alpha_prior
  
  theta_prior_RL <-samples$draws(
    variables = "theta_prior",
    inc_warmup = FALSE,
    format = "df"
  )$theta_prior
  
  alpha_est_RL <-samples$draws(
    variables = "alpha_p",
    inc_warmup = FALSE,
    format = "df"
  )$alpha_p
  
  theta_est_RL <-samples$draws(
    variables = "theta_l",
    inc_warmup = FALSE,
    format = "df"
  )$theta_l  
  
  ###prior and posterior predictive checks
  #draw a value from each 8000 estimates for 0s and ones for each round
  # all variable
  samples_varnames <- colnames(samples$draws(format = "df"))
  ##pp 1
  prior_pred_varnames <- na.omit(str_extract(samples_varnames,
                                             "^prior_preds\\[2,.*"))
  
  #extract prior_predictions
  prior_predictions <- samples$draws(
    variables = prior_pred_varnames,
    inc_warmup = FALSE,
    format = "df"
  )
  ##### draw from priors
  prior_preds <- array(NA,turns)
  for (i in 1:turns){
    prior_preds[i] <- sample(prior_predictions[[i]],1)
  }
  ##pp 2
  posterior_pred_varnames <- na.omit(str_extract(samples_varnames,
                                                 "^posterior_preds.*"))
  
  #extract prior_predictions
  posterior_predictions <- samples$draws(
    variables = posterior_pred_varnames,
    inc_warmup = FALSE,
    format = "df"
  )
  ##### draw from posteriors
  posterior_preds <- array(NA,turns)
  for (i in 1:turns){
    posterior_preds[i] <- sample(posterior_predictions[[i]],1)
  }
  
  ########################################## make output
  list_params <- list(
    seed = seed,
    turns = turns,
    bias_RA = bias,
    noise_RA = noise,
    true_alpha = alpha,
    true_theta = theta,
    alpha_prior = alpha_prior_RL,
    theta_prior = theta_prior_RL,
    alpha_posterior = alpha_est_RL,
    theta_posterior = theta_est_RL,
    outcomes = r[2,],
    prior_predictions = prior_preds,
    posterior_predictions = posterior_preds
  ) 
  
  
  return(list_params)
  
}
