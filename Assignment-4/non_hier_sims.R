
source("func.R")
library("dplyr")
library("tidyverse")
library("dplyr")
library("future")
library("furrr")
library("tidyr")


# Distance 
# measure the weighted distance between vectors?
distance <- function(vect1, vect2, w) {
  return(sum(w * abs(vect1 - vect2)))
}

# Similarity
# returns a value ]0,1[,
# small c means they are similar regardless of distance
similarity <- function(distance, c) {
  return(exp(-c * distance))
}


################
# so similarity is 0-5
### generative model ###
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
simulate_responses <- function(agent, w, c) {
  
  observations <- sim_data %>%
    dplyr::select(c("f1", "f2", "f3", "f4", "f5"))
  
  category <- sim_data$dangerous
  
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


######################### get simualting
#---------------------- Specifications :
set.seed(1234)
subjects <- 1
sessions <- 1
stimuli <- 32
cycles <- 3
features <- 5

trials_per_session <- stimuli * cycles

#---------------------- Generating stimuli - this one is used for the BIG dataset, just randomized each time:

stimulus <- 1:stimuli # for stimulus id
stimuli_df <- as.data.frame(matrix(sample(0:1, stimuli * features, replace = TRUE), # samples 0s and 1s 32 x 5 times, allows multiple 0 and 1 values to be sampled.
                                   nrow = stimuli, # all values are arranged into 32 rows.
                                   ncol = features)) # all values are arranged into 5 columns.
colnames(stimuli_df) <- paste0("f", 1:5) # adds f to feature columns
stimuli_df <- cbind(stimulus, stimuli_df) # combines to a df


data_per_session <- function(subject, session) {
  
  set.seed(subject + sample(1:10000, 1)) # maybe ensures more randomness
  
  set.seed(1000 + subject * 10 + session)
  
  cycle_1 <- stimuli_df[sample(nrow(stimuli_df)), ]
  cycle_2 <- stimuli_df[sample(nrow(stimuli_df)), ]
  cycle_3 <- stimuli_df[sample(nrow(stimuli_df)), ]
  
  cycle_1$cycle <- 1 # add an index to mark each cycle
  cycle_2$cycle <- 2
  cycle_3$cycle <- 3
  
  session_df <- rbind(cycle_1, cycle_2, cycle_3)
  session_df$subject <- subject
  session_df$session <- session
  session_df$trial <- 1:nrow(session_df)
  
  session_df <- session_df[, c("subject", "session", "trial", "cycle", "stimulus", paste0("f", 1:features))]
  
  return(session_df)
}

#------------------------- Full data (across participants and sessions)

sim_data <- do.call(rbind, lapply(1:subjects, function(subj) {
  do.call(rbind, lapply(1:sessions, function(sess) {
    data_per_session(subj, sess)
  }))
}))

sim_data <- sim_data %>%
  mutate(dangerous = case_when(
    session == 1 & f1 == 1 & f3 == 1 ~ 1,
    session == 2 & f4 == 1 ~ 1,
    session == 3 & f4 == 1 & f5 == 1 ~ 1,
    TRUE ~ 0
  ))

rownames(sim_data) <- NULL

sim_data <- sim_data %>%
  mutate(category = case_when(
    dangerous == 0 ~ 1, # when stimulus is not dangerous, the true response should be 1
    dangerous == 1 ~ 2 # when stimulus is dangerous, the true response should be 2
  ))

######################## check %s
dist_stimuli <- sim_data %>% 
  group_by(session,dangerous,) %>% 
  reframe(n()/cycles/subjects)

####################### add response 

sim_data <- sim_data %>% 
  select(!category)

######## 5 non hier agents, 1 ruleset, equal w, c = 1 
for (i in 1:5){
  
  if (i == 1){
  fin_sim_response <- simulate_responses(agent = "equal_c1",
                                           w = "equal",
                                           c = 1)
  fin_sim_response <- fin_sim_response %>% 
    mutate(subject = i)
  } else {
  temp_sim_response <- simulate_responses(agent = "equal_c1",
                                          w = "equal",
                                          c = 1)
  temp_sim_response <- temp_sim_response %>% 
    mutate(subject = i)
  
  fin_sim_response <- rbind(fin_sim_response, temp_sim_response)
  }
  
}
#and rename
equal_c1 <- fin_sim_response

######## 5 non hier agents, 1 ruleset, equal w, c = 0.5 
for (i in 1:5){
  
  if (i == 1){
    fin_sim_response <- simulate_responses(agent = "equal_c0.5",
                                           w = "equal",
                                           c = 0.5)
    fin_sim_response <- fin_sim_response %>% 
      mutate(subject = i)
  } else {
    temp_sim_response <- simulate_responses(agent = "equal_c0.5",
                                            w = "equal",
                                            c = 0.5)
    temp_sim_response <- temp_sim_response %>% 
      mutate(subject = i)
    
    fin_sim_response <- rbind(fin_sim_response, temp_sim_response)
  }
  
}
#and rename
equal_c0.5 <- fin_sim_response

######## 5 non hier agents, 1 ruleset, skewed w, c = 1 
for (i in 1:5){
  
  if (i == 1){
    fin_sim_response <- simulate_responses(agent = "skewed_c1",
                                           w = "skewed1",
                                           c = 1)
    fin_sim_response <- fin_sim_response %>% 
      mutate(subject = i)
  } else {
    temp_sim_response <- simulate_responses(agent = "skewed_c1",
                                            w = "skewed1",
                                            c = 1)
    temp_sim_response <- temp_sim_response %>% 
      mutate(subject = i)
    
    fin_sim_response <- rbind(fin_sim_response, temp_sim_response)
  }
  
}
#and rename
skewed_c1 <- fin_sim_response

######## 5 non hier agents, 1 ruleset, skewed w, c = 0.5 
for (i in 1:5){
  
  if (i == 1){
    fin_sim_response <- simulate_responses(agent = "skewed_c0.5",
                                           w = "skewed1",
                                           c = 0.5)
    fin_sim_response <- fin_sim_response %>% 
      mutate(subject = i)
  } else {
    temp_sim_response <- simulate_responses(agent = "skewed_c0.5",
                                            w = "skewed1",
                                            c = 0.5)
    temp_sim_response <- temp_sim_response %>% 
      mutate(subject = i)
    
    fin_sim_response <- rbind(fin_sim_response, temp_sim_response)
  }
  
}
#and rename
skewed_c0.5 <- fin_sim_response

full_sim_response <- rbind(equal_c1,equal_c0.5,skewed_c1,skewed_c0.5)


