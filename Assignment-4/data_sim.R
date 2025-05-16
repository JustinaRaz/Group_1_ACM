pacman::p_load("tidyverse", "ggplot2", "cmdstanr", "brms", "tidyr", "dplyr", "stringr")
source("u_func.R")
set.seed(123)

# Load the data and extract the features that are already there:

setwd("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-4")

emp_data <- read.table("data/AlienData.txt", header = TRUE, sep = ",")

features <- emp_data %>%
  mutate(stimulus = str_remove(stimulus, ".jpg")) %>%    
  distinct(stimulus, .keep_all = FALSE) %>%              # keeps unique values
  mutate(chars = str_split(stimulus, "", simplify = TRUE)) %>% 
  transmute(                                              
    f1 = as.integer(chars[, 1]),
    f2 = as.integer(chars[, 2]),
    f3 = as.integer(chars[, 3]),
    f4 = as.integer(chars[, 4]),
    f5 = as.integer(chars[, 5])
  )

features <- features %>% #only simulating the rule for the first session
  mutate(dangerous = ifelse(f1 == 1 & f3 == 1, 1, 0))

table(features$dangerous) # not equal amount, so we will choose 

#==================================== Creating dangerous stimuli 
# Since there in the data set are 11 only, we extract those, and choose 5 extra at random:

dangerous_stimuli <- features %>%
  filter(dangerous == 1)

extra_dangerous <- dangerous_stimuli %>%
  slice_sample(n = 5)

dangerous_stimuli <- bind_rows(dangerous_stimuli, extra_dangerous)

#==================================== Creating non-dangerous stimuli 
non_dangerous_stimuli <- features %>%
  filter(dangerous == 0) %>%
  slice_sample(n = 16) #randomly chooses 16 non_dangerous feature combinations.

#==================================== Combining both 

final_features <- rbind(dangerous_stimuli, non_dangerous_stimuli)

table(final_features$dangerous) # Equal amount!

#==================================== Dataset with 1 participant, across 96 trials and 1 session 

agents <- 5
n_trials <- 96
stimulus <- nrow(final_features) # as ID

# Simulate data structure as for 1 participant:

d_1 <- final_features[sample(nrow(final_features)), ] # first cycle
d_2 <- d_1[sample(nrow(d_1)), ] # second cycle
d_3 <- d_2[sample(nrow(d_2)), ] # third cycle

data_participant <- rbind(d_1, d_2, d_3)
data_participant$trial <- 1:n_trials

sim_data <- data_participant

rownames(sim_data) <- NULL

#==================================== Simulating responses

#These datasets are used for PLOTTING different parameter settings and their impact on performance only.
#The dataset sim_data is used to generate the rest of the participant's

# The code below simulates data for when weights are equal, and c = 1
equal_c1 <- data.frame()

for (i in 1:agents){
    
  if (i == 1){
    fin_sim_response <- simulate_responses(agent = "equal_c1",
                                           w = "equal",
                                           c = 1,
                                           sim_data = sim_data)
    fin_sim_response <- fin_sim_response %>% 
      mutate(subject = i)
    
    equal_c1 <- fin_sim_response
    
  } else {
    
    # Need to shuffle data again - each participant should see the same stimuli in different order.
    
    d_1 <- final_features[sample(nrow(final_features)), ] # first cycle
    d_2 <- d_1[sample(nrow(d_1)), ] # second cycle
    d_3 <- d_2[sample(nrow(d_2)), ] # third cycle
    
    data_participant <- rbind(d_1, d_2, d_3)
    data_participant$subject <- i
    data_participant$trial <- 1:n_trials
    
    temp_sim_response <- simulate_responses(agent = "equal_c1",
                                            w = "equal",
                                            c = 1,
                                            sim_data = data_participant)
    
    temp_sim_response <- temp_sim_response %>% 
      mutate(subject = i)
    
    equal_c1 <- rbind(equal_c1, temp_sim_response)
  }
  
}

write_csv(equal_c1, "data/equal_c1.csv")

# The code below simulates data for when weights are equal, and c = 0.1

equal_c01 <- data.frame()

for (i in 1:agents){
  
  if (i == 1){
    fin_sim_response <- simulate_responses(agent = "equal_c01",
                                           w = "equal",
                                           c = 0.1,
                                           sim_data = sim_data)
    fin_sim_response <- fin_sim_response %>% 
      mutate(subject = i)
    
    equal_c01 <- fin_sim_response
    
  } else {
    
    # Need to shuffle data again - each participant should see the same stimuli in different order.
    
    d_1 <- final_features[sample(nrow(final_features)), ] # first cycle
    d_2 <- d_1[sample(nrow(d_1)), ] # second cycle
    d_3 <- d_2[sample(nrow(d_2)), ] # third cycle
    
    data_participant <- rbind(d_1, d_2, d_3)
    data_participant$subject <- i
    data_participant$trial <- 1:n_trials
    
    temp_sim_response <- simulate_responses(agent = "equal_c01",
                                            w = "equal",
                                            c = 0.1,
                                            sim_data = data_participant)
    
    temp_sim_response <- temp_sim_response %>% 
      mutate(subject = i)
    
    equal_c01 <- rbind(equal_c01, temp_sim_response)
  }
  
}

write_csv(equal_c01, "data/equal_c01.csv")

# The code below simulates data for when weights are different, and c = 1

informed_c1 <- data.frame()

for (i in 1:agents){
  
  if (i == 1){
    fin_sim_response <- simulate_responses(agent = "informed_c1",
                                           w = "informed",
                                           c = 1,
                                           sim_data = sim_data)
    fin_sim_response <- fin_sim_response %>% 
      mutate(subject = i)
    
    informed_c1 <- fin_sim_response
    
  } else {
    
    # Need to shuffle data again - each participant should see the same stimuli in different order.
    
    d_1 <- final_features[sample(nrow(final_features)), ] # first cycle
    d_2 <- d_1[sample(nrow(d_1)), ] # second cycle
    d_3 <- d_2[sample(nrow(d_2)), ] # third cycle
    
    data_participant <- rbind(d_1, d_2, d_3)
    data_participant$subject <- i
    data_participant$trial <- 1:n_trials
    
    temp_sim_response <- simulate_responses(agent = "informed_c1",
                                            w = "informed",
                                            c = 1,
                                            sim_data = data_participant)
    
    temp_sim_response <- temp_sim_response %>% 
      mutate(subject = i)
    
    informed_c1 <- rbind(informed_c1, temp_sim_response)
  }
  
}

write_csv(informed_c1, "data/informed_c1.csv")

# The code below simulates data for when weights are different, and c = 0.1

informed_c01 <- data.frame()

for (i in 1:agents){
  
  if (i == 1){
    fin_sim_response <- simulate_responses(agent = "informed_c01",
                                           w = "informed",
                                           c = 0.1,
                                           sim_data = sim_data)
    fin_sim_response <- fin_sim_response %>% 
      mutate(subject = i)
    
    informed_c01 <- fin_sim_response
    
  } else {
    
    # Need to shuffle data again - each participant should see the same stimuli in different order.
    
    d_1 <- final_features[sample(nrow(final_features)), ] # first cycle
    d_2 <- d_1[sample(nrow(d_1)), ] # second cycle
    d_3 <- d_2[sample(nrow(d_2)), ] # third cycle
    
    data_participant <- rbind(d_1, d_2, d_3)
    data_participant$subject <- i
    data_participant$trial <- 1:n_trials
    
    temp_sim_response <- simulate_responses(agent = "informed_c01",
                                            w = "informed",
                                            c = 0.1,
                                            sim_data = data_participant)
    
    temp_sim_response <- temp_sim_response %>% 
      mutate(subject = i)
    
    informed_c01 <- rbind(informed_c01, temp_sim_response)
  }
  
}

write_csv(informed_c01, "data/informed_c01.csv")

#==================================== Data plotting:

full_sim_response <- rbind(equal_c1, equal_c01, informed_c1, informed_c01)

full_sim_response %>% 
  group_by(agent, subject) %>%
  reframe(median(performance))

write.csv(full_sim_response, "data/full_sim_response.csv", row.names = FALSE)

ggplot(full_sim_response, aes(x = trial, y = performance, color = as.factor(subject))) +
  geom_line() +
  facet_grid(c ~ w, labeller = label_both) +
  labs(
    x = "Trial",
    y = "Performance",
    color = "Subject"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
    #panel.grid.major.y = element_line(color = "grey80"),  # horizontal grid lines
    #panel.grid.minor.y = element_blank()  # optional: hide minor grid lines
  )
