library(brms)
library(cmdstanr)
library(tidyverse)
library(gridExtra)

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
n_image <- 150
sim_dat <- n_subj_blank_slate_trust(n_subj = n_subj,
                                    n_image = n_image)

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
  fixed_param = TRUE,
  data = data_l, # the data :-)
  seed = 123,  # a seed, so I always get the same results
  chains = 1,  # how many chains should I fit (to check whether they give the same results)
  parallel_chains = 1, # how many of the chains can be run in parallel?
  threads_per_chain = 1, # distribute gradient estimations within chain across multiple cores
  iter_warmup = 0,  # warmup iterations through which hyperparameters (steps and step length) are adjusted
  iter_sampling = 100, # total number of iterations
  refresh = 200,
  adapt_engaged =0# how often to show that iterations have been run
  #output_dir = "simmodels", # saves the samples as csv so it can be later loaded
  #max_treedepth = 20, # how many steps in the future to check to avoid u-turns
  #adapt_delta = 0.99, # how high a learning rate to adjust hyperparameters during warmup
)

#######################################
################# prior predictive checks
#extract prior_predictions
prior_predictions <- samples$draws(
  variables = "prior_pred_S_R",
  inc_warmup = FALSE,
  format = "df"
)


#
pp_vis <- untangle_estimates(prior_predictions,
                             nsubj = n_subj, 
                             nturn = n_image)

pp_1 <- pp_vis %>% 
  ### sample 100 values from prior dists
group_by(ID,FACE_ID) %>% 
  reframe(draw = sample(value,100)) %>% 
  ggplot(aes(x=draw, group = ID))+
  geom_density(alpha= .5, color = "steelblue")+
  scale_x_continuous(breaks = seq(1,8, by = 1)) +
  ggtitle("100 Prior Prediction Draws")+
  xlab("")+
  theme_classic()

pp_2 <- pp_vis %>% 
  ### sample 100 values from prior dists
  group_by(ID,FACE_ID) %>% 
  reframe(mcv = mean(value)) %>% 
  ggplot(aes(y=mcv, x = FACE_ID))+
  geom_point(alpha= .5, color = "steelblue")+
  geom_line(alpha= .5,color = "steelblue")+
  geom_hline(yintercept = mean(1:8), linetype = 2) +
  ggtitle("Predicted Prior Distribution Means \nby Subject and Image") +
  ylab("Mean")+
  facet_wrap(~ID)+
  theme_classic()

#######################################
################# posterior predictive checks
#extract prior_predictions
posterior_predictions <- samples$draws(
  variables = "posterior_preds",
  inc_warmup = FALSE,
  format = "df"
)


#
pp_2_vis <- untangle_estimates(posterior_predictions, nsubj = nsubj, nturn = n_image)

pp_3 <-pp_2_vis %>% 
  ### sample 100 values from prior dists
  group_by(ID,FACE_ID) %>% 
  reframe(draw = sample(value,100)) %>% 
  ggplot(aes(x=draw, group = ID))+
  geom_density(alpha= .5, color = "steelblue")+
  scale_x_continuous(breaks = seq(1,8, by = 1)) +
  ggtitle("100 Posterior Prediction Draws")+
  xlab("")+
  theme_classic()

pp_4 <-pp_2_vis %>% 
  ### sample 100 values from prior dists
  group_by(ID,FACE_ID) %>% 
  reframe(mcv = mean(value)) %>% 
  ggplot(aes(y=mcv, x = FACE_ID))+
  geom_point(alpha= .5, color = "steelblue")+
  geom_line(alpha= .5,color = "steelblue")+
  geom_hline(yintercept = mean(1:8), linetype = 2) +
  ggtitle("Predicted Posterior Distribution Means \nby Subject and Image") +
  ylab("Mean")+
  facet_wrap(~ID)+
  theme_classic()
################################
#big plot

grid.arrange(pp_1,pp_3,
             pp_2,pp_4)



