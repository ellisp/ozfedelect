library(tidyverse)
library(rstan)

first_election <- as.Date("2007-11-24")
next_election <- as.Date("2019-10-15")

sample_sizes <- polls_2016 %>%
  mutate(firm = str_squish(gsub("\\(.+\\)", "", firm)),
         firm = ifelse(firm == "Morgan", "Roy Morgan", firm),
         firm = ifelse(firm == "ReachTel", "ReachTEL", firm)) %>%
  filter(firm != "Election result") %>%
  group_by(firm) %>%
  summarise(ss = mean(sample_size, tr = 0.2, na.rm = TRUE)) %>%
  ungroup() %>%
  rbind(tibble(firm = c("AMR", "YouGov"), ss = c(NA, 1034)))

sample_sizes <- sample_sizes  %>%
  mutate(ss = ifelse(is.na(ss), min(sample_sizes$ss, na.rm = TRUE), ss))

all_firms <- ozpolls %>%
  filter(firm != "Election result") %>%
  pull(firm) %>%
  unique() %>%
  as.character()

data_2pp <- ozpolls %>%
  filter(preference_type == "Two-party-preferred" & party == "ALP") %>%
  mutate(n_days = as.numeric(mid_date) - as.numeric(first_election) + 1,
         intended_vote = intended_vote / 100) %>%
  filter(n_days >= 1) %>%
  select(n_days, firm, intended_vote) %>%
  left_join(sample_sizes, by = "firm") %>%
  mutate(se = sqrt(intended_vote * (1 - intended_vote) / ss))

elections_2pp <- data_2pp %>%
  filter(firm == "Election result")
  
election_days <- c(pull(elections_2pp, n_days), next_election - first_election)
election_results <- pull(elections_2pp, intended_vote)

# Make a list of data frames, each one being the data for just one pollster:
one_pollster <- lapply(all_firms, function(x){
  filter(data_2pp, firm == x)
})

model_data <- list(
  election_days = election_days,
  election_results = election_results,
  
  inflator =sqrt(2),
  
  y1_values = one_pollster[[1]]$intended_vote,
  y1_days = one_pollster[[1]]$n_days,
  y1_n = nrow(one_pollster[[1]]),
  y1_se = one_pollster[[1]]$n_days,
  
  y2_values = one_pollster[[2]]$intended_vote,
  y2_days = one_pollster[[2]]$n_days,
  y2_n = nrow(one_pollster[[2]]),
  y2_se = one_pollster[[2]]$n_days,
  
  y3_values = one_pollster[[3]]$intended_vote,
  y3_days = one_pollster[[3]]$n_days,
  y3_n = nrow(one_pollster[[3]]),
  y3_se = one_pollster[[3]]$n_days,
  
  y4_values = one_pollster[[4]]$intended_vote,
  y4_days = one_pollster[[4]]$n_days,
  y4_n = nrow(one_pollster[[4]]),
  y4_se = one_pollster[[4]]$n_days,
  
  y5_values = one_pollster[[5]]$intended_vote,
  y5_days = one_pollster[[5]]$n_days,
  y5_n = nrow(one_pollster[[5]]),
  y5_se = one_pollster[[5]]$n_days,
  
  y6_values = one_pollster[[6]]$intended_vote,
  y6_days = one_pollster[[6]]$n_days,
  y6_n = nrow(one_pollster[[6]]),
  y6_se = one_pollster[[6]]$n_days,
  
  y7_values = one_pollster[[7]]$intended_vote,
  y7_days = one_pollster[[7]]$n_days,
  y7_n = nrow(one_pollster[[7]]),
  y7_se = one_pollster[[7]]$n_days,
  
  y8_values = one_pollster[[8]]$intended_vote,
  y8_days = one_pollster[[8]]$n_days,
  y8_n = nrow(one_pollster[[]]),
  y8_se = one_pollster[[8]]$n_days,
  
  y9_values = one_pollster[[9]]$intended_vote,
  y9_days = one_pollster[[9]]$n_days,
  y9_n = nrow(one_pollster[[9]]),
  y9_se = one_pollster[[9]]$n_days,
  
  y10_values = one_pollster[[10]]$intended_vote,
  y10_days = one_pollster[[10]]$n_days,
  y10_n = nrow(one_pollster[[10]]),
  y10_se = one_pollster[[10]]$n_days
  )

model_2pp <- stan(file = 'model-2pp/model-2pp.stan', data = model_data, chains = 4, 
                  control = list(max_treedepth = 15),
                  iter = 4000)


