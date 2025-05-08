# Distance 
distance <- function(vect1, vect2, w) {
  return(sum(w * abs(vect1 - vect2)))
}

# Similarity
similarity <- function(distance, c) {
  return(exp(-c * distance))
}

### generative model ###
gcm <- function(w, c, obs, cat_one, quiet = TRUE) {
  # create an empty list to save probability of saying "1" for each trial
  r <- c()
  
  ntrials <- nrow(obs)
  
  for (i in 1:ntrials) {
    # If quiet is FALSE, print every ten trials
    if (!quiet && i %% 10 == 0) {
      print(paste("i =", i))
    }
    # if this is the first trial, or there any category with no exemplars seen yet, set the choice to random
    if (i == 1 || sum(cat_one[1:(i - 1)]) == 0 || sum(cat_one[1:(i - 1)]) == (i - 1)) {
      r <- c(r, .5)
    } else {
      similarities <- c()
      # for each previously seen stimulus assess distance and similarity
      for (e in 1:(i - 1)) {
        sim <- similarity(distance(obs[i, ], obs[e, ], w), c)
        similarities <- c(similarities, sim)
      }
      # Calculate prob of saying "1" by dividing similarity to 1 by the sum of similarity to 1 and to 2
      numerator <- mean(similarities[cat_one[1:(i - 1)] == 1])
      denominator <- mean(similarities[cat_one[1:(i - 1)] == 1]) + mean(similarities[cat_one[1:(i - 1)] == 0])
      r <- c(r, numerator / denominator)
    }
  }
  
  return(rbinom(ntrials, 1, r))
}

# Function to simulate responses with different parameter settings
simulate_responses <- function(agent, w, c) {
  
  observations <- experiment %>%
    dplyr::select(c("height", "position"))
  
  category <- experiment$category
  
  if (w == "equal") {
    weight <- rep(1 / 2, 2)
  } else if (w == "skewed1") {
    weight <- c(0, 1)
  } else if (w == "skewed2") {
    weight <- c(0.1, 0.9)
  }
  
  # simulate responses
  responses <- gcm(
    weight,
    c,
    observations,
    category
  )
  
  tmp_simulated_responses <- experiment %>%
    mutate(
      trial = seq(nrow(experiment)),
      sim_response = responses,
      correct = ifelse(category == sim_response, 1, 0),
      performance = cumsum(correct) / seq_along(correct),
      c = c,
      w = w,
      agent = agent
    )
  
  return(tmp_simulated_responses)
}

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


# Function to simulate responses with different parameter settings
simulate_responses <- function(agent, w, c) {
  
  observations <- experiment %>%
    dplyr::select(c("height", "position"))
  
  category <- experiment$category
  
  if (w == "equal") {
    weight <- rep(1 / 2, 2)
  } else if (w == "skewed1") {
    weight <- c(0, 1)
  } else if (w == "skewed2") {
    weight <- c(0.1, 0.9)
  }
  
  # simulate responses
  responses <- gcm(
    weight,
    c,
    observations,
    category
  )
  
  tmp_simulated_responses <- experiment %>%
    mutate(
      trial = seq(nrow(experiment)),
      sim_response = responses,
      correct = ifelse(category == sim_response, 1, 0),
      performance = cumsum(correct) / seq_along(correct),
      c = c,
      w = w,
      agent = agent
    )
  
  return(tmp_simulated_responses)
}