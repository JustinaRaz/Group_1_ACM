untangle_estimates <- function(df, nsubj, nturn){
  
  df_2 <- df 
  #filter by turn
  df_slice <- df_2 %>% 
    pivot_longer(cols = seq(1,nturn*n_subj,by = 1))
  df_slice <- df_slice %>% 
    #turn is derived from index so "..[1,12]" is the value for subject 1 at turn 12
    mutate(turn_ID = as.integer(str_extract(name,"(\\d+)(?!.*\\d)")),
           ID = as.integer(str_extract(str_extract(name,"^\\D*(\\d*)"),
                                       "\\d+"))
    )
  
  return(df_slice)
}

prior_predictions <- samples$draws(
  variables = "prior_preds",
  inc_warmup = FALSE,
  format = "df"
)

pp_vis %>% 
  group_by(name) %>% 
  reframe(sample(value,1))

pp_vis <- untangle_estimates(prior_predictions, nsubj = 5, nturn = 96)

pp_vis <- pp_vis %>% 
  group_by(name,ID, turn_ID,) %>% 
  reframe(draw = sample(value,1))
pp_vis$category <- as.vector(data_l$cat_one)
pp_vis <- pp_vis %>% 
  mutate(correct = ifelse(category == draw,1,0) )

break_l_1 = seq(0,1, by = 0.2)
break_l_2 = break_l_1 *  96

pp_vis %>% 
  ggplot(aes(x = correct, fill = as.factor(correct))) +
  geom_histogram(alpha = 0.5, stat = "count", bins = 2)+
  facet_wrap(~ID)+
  ggtitle("Prior Predictions\n% of correct categorisation  (blue: correct)") +
  geom_hline(yintercept = 96/2, linetype = 2) +
  scale_y_continuous(breaks = break_l_2,
                     labels =  break_l_1 *100) +
  scale_x_discrete(breaks = c(0,1)) +
  scale_fill_manual(values = c("darkorange","steelblue"),
                    guide="none") +
  ylab("") +
  xlab("") +
  theme_classic()

#######################################
posterior_predictions <- samples$draws(
  variables = "posterior_preds",
  inc_warmup = FALSE,
  format = "df"
)


pp_2_vis <- untangle_estimates(posterior_predictions, nsubj = 5, nturn = 96)

pp_2_vis <- pp_2_vis %>% 
  group_by(name,ID, turn_ID,) %>% 
  reframe(draw = sample(value,1))
pp_2_vis$category <- as.vector(data_l$cat_one)
pp_2_vis <- pp_2_vis %>% 
  mutate(correct = ifelse(category == draw,1,0) )

break_l_1 = seq(0,1, by = 0.2)
break_l_2 = break_l_1 *  96
pp_2_vis %>% 
  ggplot(aes(x = correct, fill = as.factor(correct))) +
  geom_histogram(alpha = 0.5, stat = "count", bins = 2)+
  facet_wrap(~ID)+
  ggtitle("Posterior Predictions\n% of correct categorisation  (blue: correct)") +
  geom_hline(yintercept = 96/2, linetype = 2) +
  scale_y_continuous(breaks = break_l_2,
                     labels =  break_l_1 *100) +
  scale_x_discrete(breaks = c(0,1)) +
  scale_fill_manual(values = c("darkorange","steelblue"),
                    guide="none") +
  ylab("") +
  xlab("") +
  theme_classic()
