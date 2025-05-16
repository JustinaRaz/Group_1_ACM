# Functions

# Simulates responses for different parameter settings:

simulate_responses <- function(agent, w, c, sim_data) {
  
  observations <- sim_data %>%
    dplyr::select(c("f1", "f2", "f3", "f4", "f5"))
  
  category <- sim_data$dangerous
  
  if (w == "equal") {
    weight <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  }
  else if (w == "informed") {
    weight <- c(0.3, 0.1333, 0.3, 0.1333, 0.1333)
  }
  
  # simulate responses
  responses <- gcm(
    weight,
    c,
    observations,
    category
  )
  
  tmp_simulated_responses <- sim_data %>%
    mutate(
      sequence = seq(nrow(sim_data)),
      sim_response = responses,
      correct = ifelse(category == sim_response, 1, 0),
      performance = cumsum(correct) / seq_along(correct),
      c = c,
      w = w,
      agent = agent
    )
  
  return(tmp_simulated_responses)
}

# Distance 
distance <- function(vect1, vect2, w) {
  return(sum(w * abs(vect1 - vect2)))
}

# Similarity
similarity <- function(distance, c) {
  return(exp(-c * distance))
}

gcm <- function(w, c, obs, cat_one, quiet = FALSE) {
  # create an empty list to save probability of saying "1" for each trial
  r <- c()
  ## explanations
  ## obs <- [n,x,x,x,x], n = nr(or)id of obs, xs = features.
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


assignment_4_data <- function(data_split, w_priors, c_priors){
  
  current_split <- data_split
  
  
  ##### restructure obs
  template = array(NA,c(5,96,5))
  ### stim_id
  f1 = t(matrix(as.integer(current_split$f1),
                nrow = length(unique(current_split$trial))))
  f2 = t(matrix(as.integer(current_split$f2),
                nrow = length(unique(current_split$trial))))
  f3 = t(matrix(as.integer(current_split$f3),
                nrow = length(unique(current_split$trial))))
  f4 = t(matrix(as.integer(current_split$f4),
                nrow = length(unique(current_split$trial))))
  f5 = t(matrix(as.integer(current_split$f5),
                nrow = length(unique(current_split$trial))))
  ### 
  for (k in 1:5){
    for (i in 1:96){
      template[k,i, 1] = f1[k,i]
      template[k,i, 2] = f2[k,i]
      template[k,i, 3] = f3[k,i]
      template[k,i, 4] = f4[k,i]
      template[k,i, 5] = f5[k,i]
    }
  }
  
  data_l <- list(
    n_subj = 5,
    ntrials = 96,
    nfeatures = 5,
    cat_one = t(matrix(as.integer(current_split$dangerous),
                       nrow = length(unique(current_split$trial)))),
    y = t(matrix(as.integer(current_split$sim_response),
                 nrow = length(unique(current_split$trial)))),
    obs = template,
    w_prior_values = w_priors,
    c_prior_values = c_priors)
  
  return(data_l)
}