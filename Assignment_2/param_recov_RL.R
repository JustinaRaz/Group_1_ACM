#############
#libraries
library(cmdstanr)
library(LaplacesDemon)
library(stringr)
library(tidyverse)
library(see)
source("u_func.R")
######################## load model
#RL_logodds model
file <- file.path("models/RL_logodds.stan")

# Compile the model
mod <- cmdstan_model(file, 
                     # this specifies we can parallelize the gradient estimations on multiple cores
                     cpp_options = list(stan_threads = TRUE), 
                     # this is a trick to make it faster
                     stanc_options = list("O1")) 

############################################### test predictive checks
#turns <- 120
#output <- data_model_RL(seed=123,
#                      turns=turns,
#                      random_agent_bias = 0.7,
#                      random_agent_noise = 0,
#                      RL_alpha = 0.8,
#                      RL_theta = 1,
#                      model = mod)



#pp_checks(output)
#################################################
#try to tun it thrice

timer_1 <- Sys.time()

seed_list <- seq(1,30, by = 1)
for (i in seed_list){
set.seed(i)
alpha_r <- runif(1,0,1)
theta_r <- rlnorm(1,2,1.4)
bias_r <- runif(1,0,1)
    output <- data_model_RL(seed=i,
                                 turns=120,
                                 random_agent_bias = bias_r,
                                 random_agent_noise = 0,
                                 RL_alpha = alpha_r,
                                 RL_theta = theta_r,
                                 model = mod)
    
    
    # if doesn't exist, make it, if does, append to it
    if (exists("l1")){
      l1 <- append(l1,list(output))
    } else {
      l1 <- list(output)
    }
 
}



############ full posteriors by seed and true value
for (e in 1:length(seed_list)){
if (exists("df_fin")){
  df_temp <- tibble(
    seed = l1[[e]]$seed,
    posterior_alpha = l1[[e]]$alpha_posterior,
    true_alpha = l1[[e]]$true_alpha,
    posterior_theta = l1[[e]]$theta_posterior,
    true_theta = l1[[e]]$true_theta,
    bias = l1[[e]]$bias_RA
  )
  df_fin <- rbind(df_fin,df_temp)
} else {
  df_fin <- tibble(
    seed = l1[[e]]$seed,
    posterior_alpha = l1[[e]]$alpha_posterior,
    true_alpha = l1[[e]]$true_alpha,
    posterior_theta = l1[[e]]$theta_posterior,
    true_theta = l1[[e]]$true_theta,
    bias = l1[[e]]$bias_RA
  )
  
  }
}


timer_2 <- Sys.time()
print(timer_2 - timer_1)

### Max posterior density
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}



########### MPD vis

color_help <- c("steelblue","darkorange","steelblue")

df_fin %>% 
  group_by(seed,true_theta, true_alpha, bias) %>% 
  #get point estimates from distributions
  reframe(est_theta = MPD(posterior_theta),
          est_alpha = MPD(posterior_alpha)) %>% 
  #pivot longer for visualization
  pivot_longer(cols = c(true_alpha, true_theta),
               names_to = "full_name",
               values_to = "true_val") %>% 
  pivot_longer(cols = c(est_alpha, est_theta),
               names_to = "full_name_est",
               values_to = "est_val") %>% 
  #filter out wrong combinations, if after _ 1= for full name and est, drop it
  filter(gsub("^[^_]*.","",full_name) == gsub("^[^_]*.","",full_name_est)) %>% 
  #create types 
  #second removes everyhing after "_"
  mutate(type = gsub("^[^_]*.","",full_name)) %>% 
  #cheat a bit by rounding bias?
  
  ## ggplot it
  ggplot(aes(x=true_val,y=est_val,color= bias))+
  geom_point() +
  geom_smooth(method = "lm") +
  #refline
  geom_abline(intercept = 0, slope = 1, color= "red")+
  #change color 
  scale_color_gradientn(colours = color_help)+
  facet_wrap(~type, scales = "free") +
  theme_classic()

  