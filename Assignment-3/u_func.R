library(brms)
library(cmdstanr)
library(tidyverse)

#a function for calculating artifical group ratings
assign_GR <- function(FaceRating){
  #get relative feedback
  F_B <- sample(c(-3,-2,0,2,3),1)
  #assign as group rating, round to 0 and 8
  G_R <- FaceRating + F_B
  G_R <- ifelse(G_R < 1,1,
         ifelse(G_R > 8, 8, G_R))
  
  return(G_R)
}

#a function for calculating artifical group ratings with biasing to the center
center_biased_G_R <- function(F_R){
  
  face_update <- case_when(F_R == 1  || F_R == 2~ sample(c(0,2,3),1),
                           F_R == 3 ~ sample(c(-2,0,2,3),1),
                           F_R == 4 || F_R == 5 ~ sample(c(-3,-2,0,2,3),1),
                           F_R == 6 ~ sample(c(-3,-2,0,2),1),
                           F_R == 7 || F_R == 8 ~ sample(c(-3,-2,0),1)
  )
  G_R <- F_R + face_update
  
  return(G_R)
}


# rescale to [1,8] scale and round to integer from beta dist values
rescale_rate <- function (x, nx1 = 1, nx2 = 8,
                     minx = 0, maxx = 1) 
{ nx = nx1 + (nx2 - nx1) * (x - minx)/(maxx - minx)
  nx <- round(nx,0)
return(nx)
}

# a blank slate agent, no prior trust
blank_slate_trust <- function(subject_ID,n_image,
                              min_scale=1,max_scale=8,
                              condition=NA){
  
  #one condition
  cond <- condition
  #one participant
  s <- subject_ID
  #150 images
  i <- 1:n_image
  #random options
  opts <- seq(min_scale,max_scale, by = 1 )
  
  #create it
  df <- tibble(
    ID = rep(s,length(i)),
    FACE_ID = i,
    F_R = NA,
    F_R_beta = NA,
    G_R = NA,
    G_R_beta = NA,
    S_R_beta = NA,
    S_R_draw = NA,
    S_R = NA,
    CONDITION = cond
  ) %>% 
    #fill it by row,
    #Beta(a,b) a = rating - 1 so it becomes [0,7]
    #          b = max - rating so it is also [0,7] and a+b = 7
    rowwise() %>% 
    mutate( F_R = sample(opts,1),
            F_R_beta = list(c(F_R-1,max_scale-F_R)),
            G_R = assign_GR(F_R),
            G_R_beta = list(c(G_R-1,max_scale-G_R)),
            # add up alphas and betas to get second rating beta dist
            S_R_beta= list(c(F_R_beta[1]+G_R_beta[1],
                             F_R_beta[2]+G_R_beta[2])),
            #draw from the respective beta distributions
            S_R_draw = rbeta(1,S_R_beta[1],S_R_beta[2]),
            # rescale to [1,8] scale and round to integer
            S_R = rescale_rate(x = S_R_draw)
    ) %>% 
    #select data relevant columns
    select(ID, FACE_ID, F_R, G_R, S_R)
  
  
  return(df)
}

# blank slate trust for n number of participants
n_subj_blank_slate_trust <-function(n_subj, n_image,
                                    min_scale=1,max_scale=8,
                                    condition=NA){
for (i in 1:n_subj){
  
  if (exists("df_fin",inherits=FALSE)){
    df_temp <- blank_slate_trust(subject_ID = i ,n_image,
                                min_scale,max_scale,
                                condition)
    
    df_fin <- rbind(df_fin,df_temp)
  } else {
  df_fin <- blank_slate_trust(subject_ID = i,n_image,
                          min_scale,max_scale,
                          condition)
  }
  
}
  
return(df_fin)
}


#generate a single agent with prior trust, first rating already integrates the prior
prior_trust_no_multilevel <- function(subject_ID,
                                      n_image,
                                      prior_alpha,
                                      min_scale=1,max_scale=8,
                                      condition=NA){
  
  
  prior_beta <- (max_scale-1)-prior_alpha
  #one condition
  cond <- condition
  #one participant
  s <- subject_ID
  #150 images
  i <- 1:n_image
  #random options
  opts <- seq(min_scale,max_scale, by = 1 )
  
  #create it
  df <- tibble(
    ID = rep(s,length(i)),
    FACE_ID = i,
    p_beta = list(c(prior_alpha,prior_beta)),
    p_trust = NA,
    F_R_beta = NA,
    F_R = NA,
    G_R = NA,
    G_R_beta = NA,
    S_R_beta = NA,
    S_R_draw = NA,
    S_R = NA,
    CONDITION = cond
  ) 
  
  #prior trust 
  
  df <- df %>% 
    rowwise() %>% 
    mutate(p_trust = rbeta(1,
                           p_beta[1],
                           p_beta[2])) %>% 
    ungroup()
  
  
  #first rating comes from integration
  df <- df %>% 
    rowwise() %>%
    mutate( 
      #face is percieved
      fra = sample(opts,1) -1,
      frb = (max_scale-1) - fra,
      no_prior = rescale_rate(rbeta(1,fra,frb)),
      
      #integration
      F_R_u_trust = rbeta(1,p_beta[1]+ fra, 
                          p_beta[2] + frb),
      
      #rating is assigned
      F_R = rescale_rate(F_R_u_trust),
      F_R_beta = list(c(F_R-1,max_scale-F_R)),
      
      
      #using the slightly extremity heavy group rating assingment 
      G_R = assign_GR(F_R),
      G_R_beta = list(c(G_R-1,max_scale-G_R)),
      # add up alphas and betas to get second rating beta dist
      S_R_beta= list(c(F_R_beta[1]+G_R_beta[1],
                       F_R_beta[2]+G_R_beta[2])),
      #draw from the respective beta distributions
      S_R_draw = rbeta(1,S_R_beta[1],S_R_beta[2]),
      # rescale to [1,8] scale and round to integer
      S_R = rescale_rate(x = S_R_draw)
    ) %>% 
    #select data relevant columns
    select(ID, FACE_ID, p_beta, no_prior, F_R, G_R, S_R)
  
  
  return(df)
}

n_subj_prior_trust_multilevel <- function(n_subj, n_image,
                                          trust_a_gamma_shape,
                                          trusta_a_gamma_rate,
                                          min_scale=1,max_scale=8,
                                          condition=NA){
  for (i in 1:n_subj){
    
    #calculate individual prior trust alphas
    ind_p_trust_a <- round(rgamma(1,trust_a_gamma_shape,trusta_a_gamma_rate),3)
    ind_p_trust_a <- ifelse(ind_p_trust_a > (maxval-1), (maxval-1), ind_p_trust_a)
    
    
    if (exists("df_fin",inherits=FALSE)){
      df_temp <- prior_trust_no_multilevel(subject_ID = i ,n_image,
                                           prior_alpha = ind_p_trust_a,
                                           min_scale,max_scale,
                                           condition)
      
      df_fin <- rbind(df_fin,df_temp)
    } else {
      df_fin <- prior_trust_no_multilevel(subject_ID = i,n_image,
                                          prior_alpha = ind_p_trust_a,
                                          min_scale,max_scale,
                                          condition)
    }
    
  }
  
  return(df_fin)
  
  
}

# a blank slate agent, no prior trust, G_R pulls towards center
blank_slate_trust_center_bias <- function(subject_ID,n_image,
                              min_scale=1,max_scale=8,
                              condition=NA){
  
  #one condition
  cond <- condition
  #one participant
  s <- subject_ID
  #150 images
  i <- 1:n_image
  #random options
  opts <- seq(min_scale,max_scale, by = 1 )
  
  #create it
  df <- tibble(
    ID = rep(s,length(i)),
    FACE_ID = i,
    F_R = NA,
    F_R_beta = NA,
    G_R = NA,
    G_R_beta = NA,
    S_R_beta = NA,
    S_R_draw = NA,
    S_R = NA,
    CONDITION = cond
  ) %>% 
    #fill it by row,
    #Beta(a,b) a = rating - 1 so it becomes [0,7]
    #          b = max - rating so it is also [0,7] and a+b = 7
    rowwise() %>% 
    mutate( F_R = sample(opts,1),
            F_R_beta = list(c(F_R-1,max_scale-F_R)),
            G_R = center_biased_G_R(F_R),
            G_R_beta = list(c(G_R-1,max_scale-G_R)),
            # add up alphas and betas to get second rating beta dist
            S_R_beta= list(c(F_R_beta[1]+G_R_beta[1],
                             F_R_beta[2]+G_R_beta[2])),
            #draw from the respective beta distributions
            S_R_draw = rbeta(1,S_R_beta[1],S_R_beta[2]),
            # rescale to [1,8] scale and round to integer
            S_R = rescale_rate(x = S_R_draw)
    ) %>% 
    #select data relevant columns
    select(ID, FACE_ID, F_R, G_R, S_R)
  
  
  return(df)
}

# blank slate trust for n number of participants
n_subj_blank_slate_trust_center_bias <-function(n_subj, n_image,
                                    min_scale=1,max_scale=8,
                                    condition=NA){
  for (i in 1:n_subj){
    
    if (exists("df_fin",inherits=FALSE)){
      df_temp <- blank_slate_trust_center_bias(subject_ID = i ,n_image,
                                   min_scale,max_scale,
                                   condition)
      
      df_fin <- rbind(df_fin,df_temp)
    } else {
      df_fin <- blank_slate_trust_center_bias(subject_ID = i,n_image,
                                  min_scale,max_scale,
                                  condition)
    }
    
  }
  
  return(df_fin)
}

#function for wrangling data to stan acceptable list format
# F_R[subject,image]
df_data_to_list <- function(df){
  data <- list(
    minval = 1,
    maxval = 8,
    s = length(unique(df$ID)),
    n = length(unique(df$FACE_ID)),
    F_R = t(matrix(as.integer(df$F_R), nrow = length(unique(df$FACE_ID)))),
    G_R = t(matrix(as.integer(df$G_R), nrow = length(unique(df$FACE_ID)))),
    S_R =t(matrix(as.integer(df$S_R), nrow = length(unique(df$FACE_ID))))
    
  )
  
  return(data)
}

blank_slate_model <- function(data, n_draws = 8000){
  
  # since  the outcome is always a perfect beta distribution,
  # sampling it 8000 times is a bit of an overkill, but it is kept 
  # for consistency with the more complicated models
  
  posterior <- array(NA, c(data$s, #each subject
                          data$n, #each image
                           n_draws))
   #posterior dist
  
  posterior_pred <- array(NA, c(data$s, #each subject
                                data$n
                                )) #each image
  #pre progress bar
  print("Starting up...")
  for (i in 1:data$s){
    for ( j in 1:data$n){
      
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
    pc <- as.character(round(i/data$s*100,1))
    print(c(pc," % done"))
  }
  
  results <- list(
    posteriors = posterior,
    posterior_preds = posterior_pred
  )
  
  return(results)
}

## a function for taking a wide version of data and formatting it long
## f.x. prior_prediction[1,1], prior_rediction[1,2]...
## becomes subject | turn | name
#              1   |  1   | prior_prediction[1,1]
#              1   |  2   | prior_prediction[1,2]
untangle_estimates <- function(df, nsubj, nturn){
  
  df_2 <- df 
  #filter by turn
  df_slice <- df_2 %>% 
    pivot_longer(cols = seq(1,nturn*n_subj,by = 1))
  df_slice <- df_slice %>% 
    #turn is derived from index so "..[1,12]" is the value for subject 1 at turn 12
    mutate(FACE_ID = as.integer(str_extract(name,"(\\d+)(?!.*\\d)")),
           ID = as.integer(str_extract(str_extract(name,"^\\D*(\\d*)"),
                                       "\\d+"))
    )
  
  return(df_slice)
}