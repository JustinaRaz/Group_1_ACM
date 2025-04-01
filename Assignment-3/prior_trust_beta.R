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
# if alpha is > 7 it is rounded down


#illustrate priors and integration



# simualting
test <- n_subj_prior_trust_multilevel(n_subj = 10,
                                     n_image = 150,
                                     trust_a_gamma_shape = 6,
                                     trusta_a_gamma_rate = 2,
                                  )


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
                                    n_image = n_image
)

sim_dat_2 <- n_subj_blank_slate_trust_non_bias(n_subj = n_subj,
                                               n_image = n_image
)

###################################
# convert data 

data_l <- df_data_to_list(sim_dat)
data_l_2 <- df_data_to_list(sim_dat_2)
