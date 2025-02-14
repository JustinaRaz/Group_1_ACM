#random biased agent

#usefullibs
library(tidyverse)
library(LaplacesDemon)

#random agent with bias
favourite_hand_agent <- function(bias,noise){
  
  noise_or_not <- rbinom(1,1,noise)
  
  guess <- ifelse(noise_or_not == 0,
                  rbinom(1,1,bias),
                  rbinom(1,1,0.5)
  )
  
  return(guess)
}

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


#win stay lose shift no noise
wsls_agent_guessr <- function(opp_choice,choice_prev){
  
  #transition from 0 left 1 right domain to 0 lose 1 win domain
  outcome <- ifelse(opp_choice == choice_prev,1,0)
  
  next_choice <- ifelse(outcome == 1, choice_prev, 1-choice_prev)
  
  return(next_choice)
  
}

#win stay lose shift with noise
wsls_agent_guessr_noise <- function(opp_choice,choice_prev,noise){
  
  
  #transition from 0 left 1 right domain to 0 lose 1 win domain
  outcome <- ifelse(opp_choice == choice_prev,1,0)
  
  next_choice <- ifelse(outcome == 1, choice_prev, 1-choice_prev)
  
  #add noise
  noise_or_not <- rbinom(1,1,noise)
  choice <- ifelse(noise_or_not == 1,
                   rbinom(1,1,0.5),
                   next_choice)
  
  return(choice)
  
}

#loop through all bias and wsls
bias_list <- seq(0,1, by = 0.1)
noise_list <- seq(0,0.5, by = 0.1)


# simulate turns for reinforced learning 
for (i in bias_list){
  for (j in noise_list){ 
    #initiate game
    trial <- 120
    r <- array(NA,c(2,120))
    #hider params
    bias <- i
    noise <- j
    run_id <- str_glue("b{i}n{j}")
    #guessr params 
    alpha <- 0.6
    theta <- 3
    Q <- array(NA, c(2,120))
    Q[1,1] <- 0.5
    Q[2,1] <- 0.5
    #hider
    r[1,1] <- favourite_hand_agent(bias,noise)
    #guessr
    r[2,1] <- favourite_hand_agent(bias,noise)
    
    for (t in 2:trial){
      #hider
      r[1,t] <- favourite_hand_agent(bias,noise)
      
      #guessr
      g_res <-RL_guessr(prev_outcome = r[1,t-1],
                                      Q_prev = Q[,t-1],
                                      choice_prev = r[2,t-1] + 1,
                                      alpha = alpha,
                                      theta = theta)
      
      #guessr Q
      r[2,t] <- g_res[1]
      Q[1,t] <- g_res[2]
      Q[2,t] <- g_res[3]
      
    }
    if (i == bias_list[1] && j == noise_list[1]){
      df_fin_2 <-tibble(Turn = rep((1:120),2),
                        Participant = rep(c("H","G"), each = 120),
                        Hand = c(r[1,],r[2,]),
                        bias = rep(i,120*2),
                        noise = rep(j,120*2),
                        run = run_id
      )
    }else{
      df_temp <-tibble(Turn = rep((1:120),2),
                       Participant = rep(c("H","G"), each = 120),
                       Hand = c(r[1,],r[2,]),
                       bias = rep(i,120*2),
                       noise = rep(j,120*2),
                       run = run_id)
      
      
      df_fin_2 <- rbind(df_fin_2,df_temp)
    }
    
  }}


#who won
#sum(ifelse(r[1,] == r[2,],1,-1))


for (i in bias_list){
  for(j in noise_list){
    
    
    #initiate game
    trial <- 120
    r <- array(NA,c(2,120))
    #hider params
    bias <- i
    noise <- j
    run_id <- str_glue("b{i}n{j}")
    r[1,1] <- favourite_hand_agent(bias,noise)
    #guessr
    r[2,1] <- favourite_hand_agent(bias,noise)
    
    for (t in 2:trial){
      #hider
      r[1,t] <- favourite_hand_agent(bias,noise)
      
      #guessr
      r[2,t] <- wsls_agent_guessr(opp_choice = r[1,t-1],
                             choice_prev =r[2,t-1])
      
      
    }
    
    if (i == bias_list[1] && j == noise_list[1]){
      df_fin <-tibble(Turn = rep((1:120),2),
                      Participant = rep(c("H","G"), each = 120),
                      Hand = c(r[1,],r[2,]),
                      bias = rep(i,120*2),
                      noise = rep(j,120*2),
                      run = run_id
      )
    }else{
      df_temp <-tibble(Turn = rep((1:120),2),
                       Participant = rep(c("H","G"), each = 120),
                       Hand = c(r[1,],r[2,]),
                       bias = rep(i,120*2),
                       noise = rep(j,120*2),
                       run = run_id)
      
      
      df_fin <- rbind(df_fin,df_temp)
    }
    
  }}

#visualize individually

df_fin %>%
  #select only guessr
  #filter(Participant == "H") %>% 
  group_by(Participant,run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),Turn, bias,noise) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic()+
  facet_grid(Participant~noise)

df_fin_2 %>%
  #select only guessr
  #filter(Participant == "H") %>% 
  group_by(Participant,run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),Turn, bias, noise) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic()+
  facet_grid(Participant~noise)

#visualixe together

df_wsls <- df_fin %>% 
  mutate(mod = "wsls")
df_rl <- df_fin_2 %>% 
  mutate(mod = "rl")
df_full <- rbind(df_wsls,df_rl)

df_full %>%
  #select only guesst models
  filter(Participant == "G") %>% 
  group_by(mod,run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),Turn, bias, noise) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic()+
  facet_grid(mod~noise)
