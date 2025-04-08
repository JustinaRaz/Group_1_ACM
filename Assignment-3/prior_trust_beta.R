library(brms)
library(cmdstanr)
library(tidyverse)
library(gridExtra)

source("u_func.R")
##############################

#PRIOR TRUST BETA AGENT + MODEL

##############################
# Assumptions: 

#- trust based on a face can be interpreted [0;1], 
#      0 = complete distrust, 1 = complete trust
#- participants have a prior tendency for trust, 
#      which is roughly on the same scale as the faces
#- participants differ in their prior trust
#- First rating is already integrated from trust and first image


# Prior trust should be roughly on the same scale, so [0;7] for alpha
# and Beta, with alpha + beta = 7
# this is achieved by sampling alpha from a gamma distribution and beta = 7-alpha
# if beta is < 0 it is rounded down


#illustrate priors and integration






########## Illustrating trust update
set.seed(1)


##################################
#simulate data 

n_subj <- 10
n_image <- 150
# simualting
sim_dat <- n_subj_prior_trust_multilevel(n_subj,
                                      n_image,
                                      trust_a_gamma_shape = 6,
                                      trusta_a_gamma_rate = 2,
)


###################################
# convert data 

data_l <- df_data_to_list(sim_dat)

#add priors
data_l <- append(data_l,
                 c(prior_a_shape=6,prior_a_rate=2)
)

###################### stan it up
file <- file.path("models/prior_trust_beta.stan")

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
  iter_warmup = 1000,  # warmup iterations through which hyperparameters (steps and step length) are adjusted
  iter_sampling = 2000, # total number of iterations
  refresh = 100,
  # how often to show that iterations have been run
  #output_dir = "simmodels", # saves the samples as csv so it can be later loaded
  max_treedepth = 20, # how many steps in the future to check to avoid u-turns
  adapt_delta = 0.99,
  init = 0,# how high a learning rate to adjust hyperparameters during warmup
)

time_2 <-   Sys.time()

time_2- time_1
## before :35 mins, 63% divergences
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





