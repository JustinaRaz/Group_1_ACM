library(cmdstanr)
file <- file.path("models/biased_random.stan")
fileh <- file.path("models/cont_height.stan")


#make data

source("../Assignment-1/agents.R")

turn <- 120
r <- array(NA,turn)

for(i in 1:turn){
  
  r[i] <- favourite_hand_agent(0.7,0)
  
}
data <- list(
  n = turn,
  h = r
)

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

################ 

h_data <- list(
  n = 40,
  h = rnorm(40,180,5)
)

# Compile the model
modh <- cmdstan_model(fileh, 
                     # this specifies we can parallelize the gradient estimations on multiple cores
                     cpp_options = list(stan_threads = TRUE), 
                     # this is a trick to make it faster
                     stanc_options = list("O1")) 

# The following command calls Stan with specific options.
samples_h <- modh$sample(
  data = h_data, # the data :-)
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

samples_h$summary()

#curve function is very good for quickly checking distribuitons,
#and how their prameters affect their density 
curve(dgamma(x,2,0.4), from = 0, to = 10)
