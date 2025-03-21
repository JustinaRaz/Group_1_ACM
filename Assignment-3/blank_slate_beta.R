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
# model
# since it has no parameters, the model can be made without estimation
# it is basically the same as the generative mechanism, sampling from 
# a beta distribution

data <- df_data_to_list(sim_dat)
n_draws <- 8000
i <- 1
j <- 1
####################################
#define model
blank_slate_model <- function(data, n_draws = 8000){
  
  # since  the outcome is always a perfect beta distribution,
  # sampling it 8000 times is a bit of an overkill, but it is kept 
  # for consistency with the more complicated models
  
  posterior <- array(NA, c(length(data$ID), #each subject
                               length(data$FACE_ID), #each image
                               n_draws)) #posterior dist
  
  posterior_pred <- array(NA, c(length(data$ID), #each subject
                               length(data$FACE_ID))) #each image
  #pre progress bar
  print("Starting up...")
  for (i in 1:length(data$ID)){
    for ( j in 1:length(data$FACE_ID)){
      
      #get alpha (-1 for the 0,7 conversion)
      S_R_alpha <- (data$F_R[i,j]-1) + (data$G_R[i,j]-1)
      #get beta ( 8 - for the 0,7 conversion)
      S_R_beta <- (8-data$F_R[i,j]) + (8-data$G_R[i,j])
      #make posterior on 0-1 scale
      posterior[i,j,] <- rbeta(n_draws,S_R_alpha,S_R_beta)
      
      for (k in 1:n_draws){
        #rescale to 1-8
        posterior[i,j,k] <- rescale_rate( posterior[i,j,k])
      }
      
      posterior_pred[i,j] <- sample(posterior[i,j,],1)
    
    }
    
    #progress bar
    pc <- as.character(round(i/length(data$ID)*100,1))
    print(c(pc," % done"))
  }
  
  results <- list(
    posteriors = posterior,
    posterior_preds = posterior_pred
  )
  
  return(results)
}

#######################################################

posteriors <- blank_slate_model(data_l)

posteriors$posterior_preds





