setwd("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-4")
source("func.R")
install.packages("furrr")
install.packages("future")

library("dplyr")
library("tidyverse")
library("dplyr")
library("future")
library("furrr")
library("tidyr")

set.seed(2000) # Reproducibility

#------------------------ Data simulation : Description

# There are 32 possible stimuli. They are presented 3 times in total, and during each time, they are all presented in the random order.
# Stimuli are conceptualized as 5 dimensional vectors of 0s and 1s.
# Stimuli can be categorized either by being dangerous or non dangerous (1 and 0, respectively).
# Response 3 and 4 indicate danger (1), response 1 and 4 indicates no danger (0).
# During first iteration : Danger depends on alien having spots AND eyes in stalks [feature 1 and 3 both being].
# During second session : Danger depends on arms being up (feature 4 being; so 1.).
# During third session : Danger depends on arms being up AND green color (feature 4 AND 5 being; so both = 1). 
# The experiment also contrasted dyads (condition 1) with individuals (condition 2), but that's less relevant for the simulation.

#---------------------- Specifications :

subjects <- 25
sessions <- 3
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

#----------------------- Simulate responses
# Function to simulate responses with different parameter settings

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

regenerate_simulations <- TRUE

if (regenerate_simulations) {
  
  # simulate responses across parameter combinations
  
  plan(multisession, workers = availableCores())
  
  param_df <- dplyr::tibble(
    expand_grid(
      agent = unique(sim_data$subject), # changed this line from the Riccardo's code
      c = seq(.1, 2, 0.2),
      w = c("equal", "skewed1", "skewed2")
    )
  )
  
  simulated_responses <- future_pmap_dfr(param_df,
                                         simulate_responses,
                                         .options = furrr_options(seed = TRUE)
  )
  
  # Save model fits
  write_csv(simulated_responses, "simulated_responses.csv")
  
  cat("Simulation saved.\n")
}

# Load simulated responses:
simulated_responses <- read_csv("simulated_responses.csv")


#----------------------- Empirical data

data <- read.table("AlienData.txt", header = TRUE, sep = ",")
data_l <- df_data_to_list(data)


#---------------------- Inspecting empirical data

