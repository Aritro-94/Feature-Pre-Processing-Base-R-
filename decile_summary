library(dplyr)

get_decile_summary = function(y_prob,y_act,num_probability_quantile=10){
  
  ## Returns decile summary (decile by default) 
  ## Arguments : 1. Pedicted probabilty of an event (i.e. P(Y=1|X=x))
  ##             2. Actual Y class
  
  ## Returns   : 1. Decile
                 2. No_of_Responders in a decile
                 3. Min and Max probility in a decile
                 4. Gain
                 5. Lift
  
  df = data.frame(prob = y_prob,y_act = y_act)
  df$Decile <- ntile(df$prob, num_probability_quantile)
  
  df %>% 
  group_by(Decile) %>% 
  summarise(No_of_responders = sum(y_act),decile_volume = n(),Min_prob = min(prob),Max_prob = max(prob)) %>%
  mutate(cum_pct_volume=cumsum(decile_volume)*100/sum(decile_volume)) %>%
  mutate(Response_rate=No_of_responders/decile_volume, Responders_pct = No_of_responders*100/sum(No_of_responders)) %>%
  mutate(Gain=cumsum(Responders_pct)) %>%
  mutate(Lift=Gain/cum_pct_volume) %>%
  select(-c(decile_volume,cum_pct_volume)) 
  }
