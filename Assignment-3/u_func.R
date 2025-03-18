library(brms)
library(tidyverse)

sim_data <- function(){
  
}

assign_GR <- function(FaceRating){
  #get relative feedback
  F_B <- sample(c(-3,-2,0,2,3),1)
  #assign as group rating, round to 0 and 8
  G_R <- FaceRating + F_B
  G_R <- ifelse(G_R < 0,0,
         ifelse(G_R > 8, 8, G_R))
  
  return(G_R)
}
