setwd("/Users/justina/Desktop/Aarhus_Uni/Master/Semester-2/ACM/A-4")


library("tidyverse")
library("dplyr")
library("ggplot2")


data <- read.table("AlienData.txt", header = TRUE, sep = ",")

data <- data[data$test != 1, ]

df_cumulative <- data %>%
  group_by(subject, session) %>%
  arrange(subject, session, trial) %>%
  mutate(trial_index = trial,  # Continuous trial number per session
         cumulative_correct = cumsum(correct))

ggplot(df_cumulative, aes(x = trial_index, y = cumulative_correct, color = factor(session))) +
  geom_line() +
  facet_wrap(~ subject, scales = "free_y") +
  labs(
    title = "Correct (cumulative) responses per session",
    x = "Trial (within session)",
    y = "Correct responses (cumulative)",
    color = "Session"
  ) +
  theme_minimal()

df_summary <- data %>%
  group_by(subject, session) %>%
  summarise(
    correct_total = sum(correct),
    n_trials = n(),
    percent_correct = (correct_total / n_trials) * 100
  )

ggplot(df_summary, aes(x = factor(session), y = percent_correct, fill = factor(session))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ subject) +
  labs(
    title = "Percentage of correct responses per session (for each subject)",
    x = "Session",
    y = "Correct responses (%)",
    fill = "Session"
  ) +
  scale_fill_brewer(palette = "Set4") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")  # center and bold
  )