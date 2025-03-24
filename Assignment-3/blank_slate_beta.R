library(brms)
library(cmdstanr)
library(tidyverse)

source("u_func.R")
##############################

# THE BLANK SLATE BETA AGENT + MODEL

##############################
# Assumptions: 

#- trust based on a face can be interpreted [0;1], 
#      0 = complete distrust, 1 = complete trust
#- participants have no prior tendency for trust, 
#      which influences choice
#- everything can be derived directly from data
#- no individual differences among participants


# The 7 step scale [1,8] used to measure Trust is assumed
# to be a proxy measurement for the underlying 
# trust representation.

# The percieved face trust density is assumed to be distributed:
# Beta(alpha = face rating - 1, beta = 8 - face rating).

# The group rating interpreted is also interpreted:
# Beta(alpha = group rating - 1, beta = 8 - group rating).
# alpha + beta = 8

# Possible combinations are: 
# Beta(0,7), -- Distribution only has 0s 
# Beta(1,6), 
# Beta(2,5),
# ...
# Beta(6,1)
# Beta(7,0) -- Distribution only has 1s

# After the information integration,
# second rating is distributed:
# Beta(alpha= first rating alpha + group rating alpha, 
#      beta = first rating beta + group rating beta)

############ Example trust update
# 1, obtain data
# First rating = 4, group rating = 2

# 2, update information
# first rating trust ~ beta(4,3) 
# group rating trust ~ beta(2,5)
# second rating trust ~ beta(4+2,3+5)

# 3, sample updated trust
# second rating draw = rbeta(1,4+2,3+5)

# 4, predict second rating on a 1-8 scale
# resale second rating draw[0,1] to likert[1,8]
# round recaled value to integer

########## Illustrating trust update
set.seed(1)
facerating <- rbeta(1000,4,3)
grouprating <- rbeta(1000,2,5)
secondrating <- rbeta(1000,4+2,3+5)

df_vis <- tibble(type= rep(c("F_R","G_R","S_R"),each = 1000),
                 vals = c(facerating,grouprating,secondrating)
)

df_vis %>% 
  ggplot(aes(x = vals, group = type, fill = type))+
  geom_density(alpha=.5)+
  theme_classic()

##################################
#simulate data 

n_subj <- 10

sim_dat <- n_subj_blank_slate_trust(n_subj = 10,
                                    n_image = 150)

###################################
# convert data 

data_l <- df_data_to_list(sim_dat)

####################################
# model
# since it has no parameters, the model can be made without estimation
# it is basically the same as the generative mechanism, sampling from 
# a beta distribution
posteriors <- blank_slate_model(data_l)
posteriors$posterior_preds


#######################################################
# stan it up
file <- file.path("models/blank_slate_beta.stan")

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

