library(cmdstanr)
# biased random with increasing bias !
#random agent with bias
favourite_hand_agent <- function(bias,noise){
  
  noise_or_not <- rbinom(1,1,noise)
  
  guess <- ifelse(noise_or_not == 0,
                  rbinom(1,1,bias),
                  rbinom(1,1,0.5)
  )
  
  return(guess)
}

turns <- 100
r <- array(NA,turns)
bias_v <- array(NA,turns)
b_inc <- 1.005
#initialize first 
bias_v[1] <- 0.55
r[1] <- favourite_hand_agent(bias= bias_v[1],
                             noise = 0)

for (i in 2:turns){
  bias_v[i] <- bias_v[i-1] * b_inc
  r[i] <- favourite_hand_agent(bias = bias_v[i],
                               noise = 0)
  
  
}
##### RL agent
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

############################ model

data <- list(
  n = turns,
  h = r
)
file <- file.path("models/bias_change_by_turn.stan")

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

samples$summary(c("bias[1]","bias_inc"))

########################### does log-odds scale help?


turns <- 40
r <- array(NA,turns)
bias_v <- array(NA,turns)
bias_lg <- array(NA,turns)
#for log odds scale
b_inc <- 1.1
#initialize first 
bias_v[1] <- 0.55
bias_lg[1] <- logit(bias_v[1])


r[1] <- favourite_hand_agent(bias = invlogit(bias_lg[1]),
                             noise = 0)

for (i in 2:turns){
  
  bias_lg[i] <- bias_lg[i-1] * b_inc
  
  r[i] <- favourite_hand_agent(bias = invlogit(bias_lg[i]),
                               noise = 0)
  
  
}
data <- list(
  n = turns,
  h = r
)
file <- file.path("models/bias_by_turn_logodds.stan")

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

samples$summary(c("bias[1]","bias_inc"))

########################## Test RL
#randomly sampling bias and noise
turns <- 120
bias <- 0.7
noise <- 0
theta <- 1
alpha <- 0.8
#initiate game
r <- array(NA,c(2,turns))
#hider params
#run_id <- str_glue("r{j}a{a}t{k}")
r[1,1] <- favourite_hand_agent(bias,noise)
#guessr
Q <- array(NA, c(2,turns))
Q[1,1] <- 0.5
Q[2,1] <- 0.5
r[2,1] <- favourite_hand_agent(0.5,0.5)



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


data <- list(
  n = 120,
  h = r[2,],
  oh =r[1,],
  k = 2
)

#######################################
file <- file.path("models/RL.stan")

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

samples$summary(c("theta","alpha","p[1,120]","p[2,120]"))


################################ RL_logodds
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

samples$summary(c("theta","alpha","theta_l","alpha_p"))
