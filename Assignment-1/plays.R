source("agents.R")

########################basic random bias
bias_list <- seq(0,1, by = 0.1)

for (i in bias_list){
  
  #initiate game
  trial <- 150
  r <- array(NA,150)
  #params
  bias <- i
  run_id <- str_glue("b{i}")
  for (t in 1:trial){
  r[t] <- favourite_hand_agent(bias = bias, noise = 0)
}
  if (i == bias_list[1]){
    df_fin <-tibble(Turn = 1:150,
                    Hand = r,
                    bias = rep(i,150),
                    #noise = rep(j,120),
                    run = run_id
    )
  }else{
    df_temp <-tibble(Turn = 1:150,
                     Hand = r,
                     bias = rep(i,150),
                     #noise = rep(j,120),
                     run = run_id)
    
    
    df_fin <- rbind(df_fin,df_temp)
  }
  
  
}

df_fin %>%
  #select only guessr
  #filter(Participant == "H") %>% 
  group_by(run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),Turn, bias) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = bias_list, alpha = 0.2, linetype = 3) +
  #scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic()
  #facet_grid(Participant~noise)



df_fin %>%
  #select only guessr
  #filter(Participant == "H") %>% 
  group_by(run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),Turn, bias, noise) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = bias_list, alpha = 0.2, linetype = 3) +
  #scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic() +
  facet_wrap(~noise)
  

####################basic random bias + noise
bias_list <- seq(0,1, by = 0.1)
noise_list <- seq(0,0.5, by = 0.1)
for (i in bias_list){
  for (j in noise_list){
    #initiate game
    trial <- 150
    r <- array(NA,150)
    #params
    bias <- i
    noise <- j
    run_id <- str_glue("b{i}n{j}")
    for (t in 1:trial){
      r[t] <- favourite_hand_agent(bias = bias, noise = j)
    }
    if (i == bias_list[1]){
      df_fin <-tibble(Turn = 1:150,
                      Hand = r,
                      bias = rep(i,150),
                      noise = rep(j,150),
                      run = run_id
      )
    }else{
      df_temp <-tibble(Turn = 1:150,
                       Hand = r,
                       bias = rep(i,150),
                       noise = rep(j,150),
                       run = run_id)
      
      
      df_fin <- rbind(df_fin,df_temp)
    }
    
    
  }
}

df_fin %>%
  #select only guessr
  #filter(Participant == "H") %>% 
  group_by(run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),Turn, bias, noise) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = bias_list, alpha = 0.2, linetype = 3) +
  #scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic() +
  facet_wrap(~noise)


##########################wsls no noise

for (i in bias_list){
  for(j in noise_list){
    
    
    #initiate game
    trial <- 150
    r <- array(NA,c(2,trial))
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
      df_fin <-tibble(Turn = rep((1:trial),2),
                      Participant = rep(c("H","G"), each = trial),
                      Hand = c(r[1,],r[2,]),
                      bias = rep(i,trial*2),
                      noise = rep(j,trial*2),
                      run = run_id
      )
    }else{
      df_temp <-tibble(Turn = rep((1:trial),2),
                       Participant = rep(c("H","G"), each = trial),
                       Hand = c(r[1,],r[2,]),
                       bias = rep(i,trial*2),
                       noise = rep(j,trial*2),
                       run = run_id)
      
      
      df_fin <- rbind(df_fin,df_temp)
    }
    
  }}

df_fin %>%
  #select only guessr
  filter(Participant == "G") %>% 
  group_by(run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),Turn, bias, noise) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = bias_list, alpha = 0.2, linetype = 3) +
  #scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic() +
  facet_wrap(~noise)

########################## wsls with noise
wsls_n_list <- c(0,0.25,0.5)
for (i in bias_list){
  for(j in noise_list){
    for(k in wsls_n_list){
    
    #initiate game
    trial <- 150
    r <- array(NA,c(2,trial))
    #hider params
    bias <- i
    noise <- j
    wsls_noise <- k
    run_id <- str_glue("b{i}n{j}wslsn{k}")
    r[1,1] <- favourite_hand_agent(bias,noise)
    #guessr
    r[2,1] <- favourite_hand_agent(bias,noise)
    
    for (t in 2:trial){
      #hider
      r[1,t] <- favourite_hand_agent(bias,noise)
      
      #guessr
      r[2,t] <- wsls_agent_guessr_noise(opp_choice = r[1,t-1],
                                        choice_prev =r[2,t-1],
                                        noise = wsls_noise)
      
      
    }
    
    if (i == bias_list[1] &&
        j == noise_list[1] &&
        k == wsls_n_list[1]){
      df_fin <-tibble(Turn = rep((1:trial),2),
                      Participant = rep(c("H","G"), each = trial),
                      Hand = c(r[1,],r[2,]),
                      bias = rep(i,trial*2),
                      noise = rep(j,trial*2),
                      wsls_noise = rep(k,trial*2),
                      run = run_id
      )
    }else{
      df_temp <-tibble(Turn = rep((1:trial),2),
                       Participant = rep(c("H","G"), each = trial),
                       Hand = c(r[1,],r[2,]),
                       bias = rep(i,trial*2),
                       noise = rep(j,trial*2),
                       wsls_noise = rep(k,trial*2),
                       run = run_id)
      
      
      df_fin <- rbind(df_fin,df_temp)
    }
    
  }}}

df_fin %>%
  #select only guessr
  filter(Participant == "G") %>% 
  group_by(run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),
          Turn, bias, noise, wsls_noise) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = bias_list, alpha = 0.2, linetype = 3) +
  #scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic() +
  facet_grid(wsls_noise~noise)

########################### Reinforcement learning 
# simulate turns for reinforced learning 
alpha_list = c(0.3,0.6,0.9)
theta_list = c(0.1,1,10)
noise_list = c(0,0.25,0.5)

for (i in bias_list){
  for (j in noise_list){ 
    for (k in alpha_list){
      for (g in theta_list){
      
    #initiate game
    trial <- 150
    r <- array(NA,c(2,trial))
    #hider params
    bias <- i
    noise <- j
    run_id <- str_glue("b{i}n{j}a{k}t{g}")
    #guessr params 
    alpha <- k
    theta <- g
    Q <- array(NA, c(2,trial))
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
                        alpha = k,
                        theta = g)
      
      #guessr Q
      r[2,t] <- g_res[1]
      Q[1,t] <- g_res[2]
      Q[2,t] <- g_res[3]
      
    }
    if (i == bias_list[1] && j == noise_list[1] &&
        k == alpha_list[1] && g == theta_list[1]){
      df_fin_2 <-tibble(Turn = rep((1:trial),2),
                        Participant = rep(c("H","G"), each = trial),
                        Hand = c(r[1,],r[2,]),
                        bias = rep(i,trial*2),
                        noise = rep(j,trial*2),
                        a = rep(k,trial*2),
                        theta = rep(g,trial*2),
                        run = run_id
      )
    }else{
      df_temp <-tibble(Turn = rep((1:trial),2),
                       Participant = rep(c("H","G"), each = trial),
                       Hand = c(r[1,],r[2,]),
                       bias = rep(i,trial*2),
                       noise = rep(j,trial*2),
                       a = rep(k,trial*2),
                       theta = rep(g,trial*2),
                       run = run_id)
      
      
      df_fin_2 <- rbind(df_fin_2,df_temp)
    }
    
      }}}}


df_fin_2 %>%
  #select only guessr
  filter(Participant == "G") %>% 
  filter(a == 0.3) %>% 
  group_by(run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),
          Turn, bias, noise, a, theta) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  ggtitle("alpha = 0.3") +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = bias_list, alpha = 0.2, linetype = 3) +
  #scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic() +
  facet_grid(theta~noise)

df_fin_2 %>%
  #select only guessr
  filter(Participant == "G") %>% 
  filter(a == 0.6) %>% 
  group_by(run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),
          Turn, bias, noise, a, theta) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  ggtitle("alpha = 0.6") +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = bias_list, alpha = 0.2, linetype = 3) +
  #scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic() +
  facet_grid(theta~noise)

df_fin_2 %>%
  #select only guessr
  filter(Participant == "G") %>% 
  filter(a == 0.9) %>% 
  group_by(run) %>% 
  reframe(rate = (cumsum(Hand)/ seq_along(Hand)),
          Turn, bias, noise, a, theta) %>% 
  
  ggplot(aes(x = Turn, y = rate, col = bias, group = bias)) +
  geom_line()+
  ggtitle("alpha = 0.9") +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = bias_list, alpha = 0.2, linetype = 3) +
  #scale_color_gradient(low = "darkorange", high = "steelblue")+
  theme_classic() +
  facet_grid(theta~noise)
