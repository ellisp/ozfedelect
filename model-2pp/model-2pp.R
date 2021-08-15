#----------------set up----------------
source("setup.R")
rstan_options(auto_write = TRUE)
options(mc.cores = 7)

ozpolls <- ozpolls %>%
  mutate(firm = ifelse(firm == "YouGov/Galaxy", "Galaxy", firm))


#--------------Data preparation-------------------
first_election <- as.Date("2007-11-24")
next_election <- as.Date("2019-05-18")
firms_today <- ozpolls %>%
  filter(election_year == 2019) %>%
  distinct(firm) %>%
  pull(firm)

# We don't have sample sizes except in the build up to 2016, so we'll just get
# the average sample size for firms from them and assume that's what they stick to:
sample_sizes <- ozpolls_2016 %>%
  mutate(firm = str_squish(gsub("\\(.+\\)", "", firm)),
         firm = ifelse(firm == "Morgan", "Roy Morgan", firm),
         firm = ifelse(firm == "ReachTel", "ReachTEL", firm)) %>%
  filter(firm != "Election result") %>%
  group_by(firm) %>%
  summarise(ss = mean(sample_size, tr = 0.2, na.rm = TRUE)) %>%
  ungroup() %>%
  rbind(tibble(firm = c("AMR", "YouGov"), ss = c(NA, 1034)))

# If we still don't have anything, we'll assign the smallest sample size
sample_sizes <- sample_sizes  %>%
  mutate(ss = ifelse(is.na(ss), min(sample_sizes$ss, na.rm = TRUE), ss))

all_firms_index <- ozpolls %>%
  filter(!firm %in% c("Election result") &
           firm %in% firms_today) %>%
  select(firm) %>% 
  distinct() %>% 
  mutate(firm_idx = as.numeric(factor(firm)))

# 2 party preferred vote - both elections and polls:
data_2pp <- ozpolls %>%
  filter(preference_type == "Two-party-preferred" & party == "ALP") %>%
  mutate(n_days = round(as.numeric(mid_date) - as.numeric(first_election) + 1),
         intended_vote = intended_vote / 100) %>%
  filter(n_days >= 1) %>%
  select(mid_date, n_days, firm, intended_vote) %>%
  left_join(sample_sizes, by = "firm") %>%
  mutate(se = sqrt(intended_vote * (1 - intended_vote) / ss),
         rnd = runif(n())) %>%
  group_by(firm) %>%
  # small random sample of results, useful during dev
  arrange(rnd) %>%
  # slice(1:20) %>%
  ungroup()

# Just the elections:
elections_2pp <- data_2pp %>%
  filter(firm == "Election result") %>%
  arrange(n_days)
  
# A couple of vectors on which days the elections were (2007 election being day 1) and
# the result on the day:
election_days <- c(pull(elections_2pp, n_days), next_election - first_election)
election_results <- pull(elections_2pp, intended_vote)

# Polling data 
polls_2pp <- data_2pp %>% 
  left_join(all_firms_index, by = "firm") %>% 
  filter(!is.na(firm_idx)) 

# Put all the data into a single list we can pass to Stan:
model_data <- list(
  number_elections = 5,
  election_days = election_days,
  election_results = election_results,
  
  # This inflator doubles the variance of polls; ie allows for non-sampling error
  inflator =sqrt(2),
  
  n_polling_firms = nrow(all_firms_index),
  
  polls_n = nrow(polls_2pp),
  polls_firm_idx = polls_2pp$firm_idx,
  polls_intended_vote = polls_2pp$intended_vote, 
  polls_se = polls_2pp$se,
  polls_day = polls_2pp$n_days
  
  )

#------------------------Model fitting----------------------
# Takes about 20 minutes or so and diagnostics were ok
st <- system.time({
  model_2pp <- stan(file = 'model-2pp/model-2pp.stan', data = model_data, chains = 4, 
                    control = list(max_treedepth = 15),
                    iter = 1000)
})
print(st)

save.image(file = "output/model_2pp.rdata")

