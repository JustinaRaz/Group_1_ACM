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

