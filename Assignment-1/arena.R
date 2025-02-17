source("agents.R")


#WSLS vs bias random

# simulate turns for reinforced learning 
turns <- 150
runs <- 1000

  for (j in 1:runs){
    
    #randomly sampling bias and noise
    bias <- runif(1,0,1)
    noise <- runif(1,0,0.5)

        #initiate game
        r <- array(NA,c(2,turns))
        #hider params
        run_id <- str_glue("r{j}")
        r[1,1] <- favourite_hand_agent(bias,noise)
        #guessr
        r[2,1] <- favourite_hand_agent(0.5,0.5)
        
        for (t in 2:turns){
          #hider
          r[1,t] <- favourite_hand_agent(bias,noise)
          #guessr
          r[2,t] <- wsls_agent_guessr(opp_choice = r[1,t-1],
                                      choice_prev = r[2,t-1])
          
        }
        
        #find score, + means guesser won
        score <- sum(ifelse(r[1,] == r[2,],1,-1))
        
        
        if (j == 1){
          df_fin <-tibble(run = run_id,
                          score = score,
                          noise = noise,
                          bias = bias
          )
        }else{
          df_temp <-tibble(run = run_id,
                          score = score,
                          noise = noise,
                          bias = bias )
          
          
          df_fin <- rbind(df_fin,df_temp)
        }
     
  }   

#sum(ifelse(r[1,] == r[2,],1,-1))

color_help <- c("steelblue","darkorange","steelblue")
df_fin %>% 
  #make bias into bands
  mutate(bias = round(bias,1)) %>% 
  ggplot(aes(x=noise,y = score, group = bias, col = bias)) +
  scale_color_gradientn(colours = color_help)+
  geom_point()+
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic()


######################### WSLS with noise
wsls_n_list <- seq(0,0.5, by = 0.1)
for (j in 1:runs){
  for (n in wsls_n_list) {
  #randomly sampling bias and noise
  bias <- runif(1,0,1)
  noise <- runif(1,0,0.5)
  wsls_noise <- n
  
  #initiate game
  r <- array(NA,c(2,turns))
  #hider params
  run_id <- str_glue("r{j}n{n}")
  r[1,1] <- favourite_hand_agent(bias,noise)
  #guessr
  r[2,1] <- favourite_hand_agent(0.5,0.5)
  
  for (t in 2:turns){
    #hider
    r[1,t] <- favourite_hand_agent(bias,noise)
    #guessr
    r[2,t] <- wsls_agent_guessr_noise(opp_choice = r[1,t-1],
                                      choice_prev = r[2,t-1],
                                      noise = wsls_noise)
    
  }
  
  #find score, + means guesser won
  score <- sum(ifelse(r[1,] == r[2,],1,-1))
  
  
  if (j == 1&& n == 0){
    df_fin <-tibble(run = run_id,
                    score = score,
                    noise = noise,
                    wsls_noise = wsls_noise,
                    bias = bias
    )
  }else{
    df_temp <-tibble(run = run_id,
                     score = score,
                     noise = noise,
                     wsls_noise = wsls_noise,
                     bias = bias )
    
    
    df_fin <- rbind(df_fin,df_temp)
  }
  
}
}
color_help <- c("steelblue","darkorange","steelblue")
df_fin %>% 
  #make bias and wsls noise into bands
  mutate(bias = round(bias,1)) %>% 
  ggplot(aes(x=noise,y = score, group = bias, col = bias)) +
  scale_color_gradientn(colours = color_help)+
  geom_point()+
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic() +
  facet_wrap(~wsls_noise)

################################# RL 
alpha_list <- c(0.3,0.6,0.9)
theta_list <- c(0.1,1,10)
for (j in 1:runs){
  for (a in alpha_list) {
    for (k in theta_list){
    #randomly sampling bias and noise
    bias <- runif(1,0,1)
    noise <- runif(1,0,0.5)
    theta <- k
    alpha <- a
    #initiate game
    r <- array(NA,c(2,turns))
    #hider params
    run_id <- str_glue("r{j}a{a}t{k}")
    r[1,1] <- favourite_hand_agent(bias,noise)
    #guessr
    Q <- array(NA, c(2,turns))
    Q[1,1] <- 0.5
    Q[2,1] <- 0.5
    r[2,1] <- favourite_hand_agent(0.5,0.5)
    
    
    
    for (t in 2:turns){
      #hider
      r[1,t] <- favourite_hand_agent(bias,noise)
      #guessr
      g_res <- RL_guessr(prev_outcome = r[1,t-1],
                         Q_prev = Q[,t-1],
                          choice_prev = r[2,t-1] + 1,
                          alpha = alpha,
                          theta = theta)
      
      #guessr Q
      r[2,t] <- g_res[1]
      Q[1,t] <- g_res[2]
      Q[2,t] <- g_res[3]
      
    }
    
    #find score, + means guesser won
    score <- sum(ifelse(r[1,] == r[2,],1,-1))
    
    
    if (j == 1 && a == alpha_list[1] && k == theta_list[1]){
      df_fin <-tibble(run = run_id,
                      score = score,
                      noise = noise,
                      theta = theta,
                      alpha = alpha,
                      bias = bias
      )
    }else{
      df_temp <-tibble(run = run_id,
                       score = score,
                       noise = noise,
                       theta = theta,
                       alpha = alpha,
                       bias = bias )
      
      
      df_fin <- rbind(df_fin,df_temp)
    }
    
  }
  }
}
color_help <- c("steelblue","darkorange","steelblue")
df_fin %>% 
  #make bias and wsls noise into bands
  mutate(bias = round(bias,1)) %>% 
  ggplot(aes(x=noise,y = score, group = bias, col = bias)) +
  scale_color_gradientn(colours = color_help)+
  geom_point()+

  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic() +
  facet_grid(alpha~theta)
