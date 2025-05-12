library(tidyverse)
library(cmdstanr)
library(brms)

assignment_4_data <- function(data_split, w_priors, c_priors){
  
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
    y = t(matrix(as.integer(current_split$sim_response),
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
                            w_priors = w_priors)


###################### stan it up
file <- file.path("./a4.stan")

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
  iter_warmup = 100,  # warmup iterations through which hyperparameters (steps and step length) are adjusted
  iter_sampling = 200, # total number of iterations
  refresh = 100,
  # how often to show that iterations have been run
  #output_dir = "simmodels", # saves the samples as csv so it can be later loaded
  max_treedepth = 20, # how many steps in the future to check to avoid u-turns
  adapt_delta = 0.99,
  init = 0,# how high a learning rate to adjust hyperparameters during warmup
)

time_2 <-   Sys.time()

time_2-time_1
