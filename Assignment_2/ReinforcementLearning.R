#############
#libraries
library(cmdstanr)
library(LaplacesDemon)
library(stringr)
library(tidyverse)
###############
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
#data creation
set.seed(123)
turns <- 120
bias <- 0.7
noise <- 0
theta <- 1
alpha <- 0.8
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
  n = 120,
  h = r[2,],
  oh =r[1,],
  k = 2
)

################################ 
#RL_logodds model
file <- file.path("models/RL_logodds.stan")

# Compile the model
mod <- cmdstan_model(file, 
                     # this specifies we can parallelize the gradient estimations on multiple cores
                     cpp_options = list(stan_threads = TRUE), 
                     # this is a trick to make it faster
                     stanc_options = list("O1")) 

# The following command calls Stan with specific options.
samples <- mod$sample(
  data = data, # the data :-)
  seed = 123,  # a seed, so I always get the same results
  chains = 4,  # how many chains should I fit (to check whether they give the same results)
  parallel_chains = 4, # how many of the chains can be run in parallel?
  threads_per_chain = 1, # distribute gradient estimations within chain across multiple cores
  iter_warmup = 1000,  # warmup iterations through which hyperparameters (steps and step length) are adjusted
  iter_sampling = 2000, # total number of iterations
  refresh = 200,  # how often to show that iterations have been run
  #output_dir = "simmodels", # saves the samples as csv so it can be later loaded
  max_treedepth = 20, # how many steps in the future to check to avoid u-turns
  adapt_delta = 0.99, # how high a learning rate to adjust hyperparameters during warmup
)

############
#prior visualization
priors <- samples$draws(
  variables = c("alpha_prior","theta_prior"),
  inc_warmup = FALSE,
  format = "df"
)

priors %>% 
  pivot_longer(cols = c(1,2)) %>% 
  ggplot(aes(x = value))+
  geom_density()+
  theme_classic()+
  facet_wrap(~name, scales = "free")
##################################################
############# prior predictive check
## get prior_predictions for RL agent
# get varnames that start with prior_pred[2,] 
#get all varnames
samples_varnames <- samples$summary()$variable
#get prior varnames
prior_pred_varnames <- na.omit(str_extract(samples_varnames,
                                           "^prior_preds\\[2,.*"))
#extract prior_predictions
prior_predictions <- samples$draws(
  variables = prior_pred_varnames,
  inc_warmup = FALSE,
  format = "df"
)
#pivot long, add turn values as separate columns
pp_vis <- prior_predictions %>% 
  pivot_longer(cols = seq(1,120,by = 1))
pp_vis <- pp_vis %>% 
  #turn is derived from index so "..[1,12]" is the value at turn 12
  mutate(turn = as.integer(str_extract(name,"(\\d+)(?!.*\\d)")))
####################
#look at distribution of prior based hand choices for RL 
pp_vis %>% 
  ggplot(aes(x = value, group = name))+
  geom_density(color="steelblue") +
  theme_classic()

#look at how the mean choices change across all chains as turns increase
pp_vis %>% 
  group_by(name) %>%
  reframe( mu =mean(value)) %>% 
  mutate(turn = as.integer(str_extract(name,"(\\d+)(?!.*\\d)"))) %>% 
  ggplot(aes(x=turn, y=mu))+
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = 2,
             alpha = 0.8, col = "darkorange") +
  theme_classic()


#####################################
###### Prior posterior updates
prior_posterior_updates <- samples$draws(
  variables = c("alpha_prior","theta_prior",
                "alpha_p","theta_l"),
  inc_warmup = FALSE,
  format = "df"
)

#pivot longer and rename to fit ggplot
prior_posterior_updates <- prior_posterior_updates %>% 
  pivot_longer(cols = c(1,2,3,4))
#
prior_posterior_updates %>% 
  #first regex removes everything befor "_"
  #second removes everyhing after "_"
  mutate(varname = gsub("\\_(.*)","",name),
         type = gsub("^[^_]*.","",name),
         type = ifelse( type == "prior",type,"posterior")
  ) %>% 
  ggplot(aes(x = value, group = type, fill = type))+
  geom_density(alpha= 0.5)+
  scale_fill_manual(values = c("darkorange","steelblue"))+
  theme_classic()+
  facet_wrap(~varname, scales = "free")

##################
###Posterior Predictive Checks
## get posterior_predictions for RL agent
# get varnames that start with posterior_pred 
posterior_pred_varnames <- na.omit(str_extract(samples_varnames,
                                           "^posterior_preds.*"))
#extract posterior_predictions
posterior_predictions <- samples$draws(
  variables = posterior_pred_varnames,
  inc_warmup = FALSE,
  format = "df"
)
#pivot long, add turn values as separate columns
pp2_vis <- posterior_predictions %>% 
  pivot_longer(cols = seq(1,120,by = 1))
pp2_vis <- pp2_vis %>% 
  mutate(turn = as.integer(str_extract(name,"(\\d+)(?!.*\\d)")))
####################
#look at distribution of posterior based choices 
pp2_vis %>% 
  ggplot(aes(x = value, group = name))+
  geom_density(color="steelblue") +
  theme_classic()

#look at how the mean choices change across all chains as turns increase
pp2_vis %>% 
  group_by(name) %>%
  reframe( mu =mean(value)) %>% 
  mutate(turn = as.integer(str_extract(name,"(\\d+)(?!.*\\d)"))) %>% 
  ggplot(aes(x=turn, y=mu))+
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = 2,
             alpha = 0.8, col = "darkorange") +
  theme_classic()
