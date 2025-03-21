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

#function for wrangling data to stan acceptable list format
# F_R[subject,image]
df_data_to_list <- function(df){
  data <- list(
    ID = unique(df$ID),
    FACE_ID = unique(df$FACE_ID),
    F_R = t(matrix(as.integer(df$F_R), nrow = length(unique(df$FACE_ID)))),
    G_R = t(matrix(as.integer(df$G_R), nrow = length(unique(df$FACE_ID)))),
    S_R =t(matrix(as.integer(df$S_R), nrow = length(unique(df$FACE_ID))))
    
  )
  
  return(data)
}
