rstan_options(auto_write = TRUE)
options(mc.cores = 7)


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
  filter(!firm %in% c("Election result") &
           firm %in% unique(polls_2019$firm)) %>%
  pull(firm) %>%
  unique() %>%
  as.character()

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

elections_2pp <- data_2pp %>%
  filter(firm == "Election result") %>%
  arrange(n_days)
  
election_days <- c(pull(elections_2pp, n_days), next_election - first_election)
election_results <- pull(elections_2pp, intended_vote)

# Make a list of data frames, each one being the data for just one pollster:
one_pollster <- lapply(all_firms, function(x){
  filter(data_2pp, firm == x)
})

model_data <- list(
  number_elections = 5,
  election_days = election_days,
  election_results = election_results,
  
  inflator =sqrt(2),
  
  y1_values = one_pollster[[1]]$intended_vote,
  y1_days = one_pollster[[1]]$n_days,
  y1_n = nrow(one_pollster[[1]]),
  y1_se = one_pollster[[1]]$se[1],
  
  y2_values = one_pollster[[2]]$intended_vote,
  y2_days = one_pollster[[2]]$n_days,
  y2_n = nrow(one_pollster[[2]]),
  y2_se = one_pollster[[2]]$se[1],
  
  y3_values = one_pollster[[3]]$intended_vote,
  y3_days = one_pollster[[3]]$n_days,
  y3_n = nrow(one_pollster[[3]]),
  y3_se = one_pollster[[3]]$se[1],
  
  y4_values = one_pollster[[4]]$intended_vote,
  y4_days = one_pollster[[4]]$n_days,
  y4_n = nrow(one_pollster[[4]]),
  y4_se = one_pollster[[4]]$se[1],
  
  y5_values = one_pollster[[5]]$intended_vote,
  y5_days = one_pollster[[5]]$n_days,
  y5_n = nrow(one_pollster[[5]]),
  y5_se = one_pollster[[5]]$se[1],
  
  y6_values = one_pollster[[6]]$intended_vote,
  y6_days = one_pollster[[6]]$n_days,
  y6_n = nrow(one_pollster[[6]]),
  y6_se = one_pollster[[6]]$se[1],
  
  y7_values = one_pollster[[7]]$intended_vote,
  y7_days = one_pollster[[7]]$n_days,
  y7_n = nrow(one_pollster[[7]]),
  y7_se = one_pollster[[7]]$se[1]
  
  )


st <- system.time({
  model_2pp <- stan(file = 'model-2pp/model-2pp.stan', data = model_data, chains = 4, 
                    control = list(max_treedepth = 15),
                    iter = 1000)
})
print(st)

# 10 iterations, no polls in model :4 seconds
# 20 iterations, no polls in model 5, 22, 39
# 40 iterations, no polls in model 1329 seconds
# 10 iterations, one firm in model, 5 observations per firm: 4, 4
# 20 iterations, one firm in model, 5 observations per firm:  9, 18, 31
# 10 iterations, two firm in model, 5 observations per firm: 4, 4
# 20 iterations, two firm in model, 5 observations per firm: 4, 4, 4,6 ,4, 8
# 40 iterations, two firm in model, 5 observations per firm: 1180 seconds

# trying a narrower starting prior for mu (4,4) instead of 2,2
# 20 iterations, two firm in model, 5 observations per firm:  3, 3, 3
# 20 iterations, seven firm in model, 5 observations per firm: 4,  43, 3, 21
### decided to keep that narrower prior

# I observed that the one that took 43 seconds had values up near 1 and 0; could be that problem
# that comes from restricting the value that can be taken when you get any probability mass at all
# there. So I've tried taking away the 0 and 1 constraints (after all, with polls near 0.5, it won't
# really go past those limits)
# 20 iterations, seven firm in model, 5 observations per firm: 4,  4,3,3 (but it goes below 0 and above 2)
# 40 iterations, seven firm in model, 5 observations per firm: 293 (much faster, but with many values outside 0,1)
### Decided to keep the 0,1 constraints off
# 20 iterations, seven firm in model, 10 observations per firm: 5, 3 (but it goes below 0 and above 2)
# 20 iterations, seven firm in model, 20 observations per firm: 3, 4 (but it goes below 0 and above 2)
# 20 iterations, seven firm in model, all data: 4, 4 (but it goes below 0 and above 2)
# 40 iterations, seven firm in model, all data: 4, 4 (but it goes below 0 and above 2)
# 60 iterations, seven firm in model, all data: 70 (but it goes below 0 and above 2)
# 120 iterations, seven firm in model, all data: 191 (but it goes below 0 and above 2 but loloking better. 
#                           and n_eff is single digits for many days)
# 1000 iterations 858

all_firms
apply(extract(model_2pp, "d")$d, 2, mean)
x <- summary(model_2pp)
x[[1]]

plot_results <- function(stan_m){
  if(class(stan_m) != "stanfit"){
    stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")
  }
  ex <- as.data.frame(rstan::extract(stan_m, "mu"))
  names(ex) <- 1:model_data$election_days[5]
  
  p <- ex %>%
    gather(day, value) %>%
    mutate(day = as.numeric(day),
           day = as.Date(day - 1, origin = first_election),
           value = value * 100) %>%
    group_by(day) %>%
    summarise(middle = mean(value),
              upper = quantile(value, 0.975),
              lower = quantile(value, 0.025)) %>%
    ggplot(aes(x = day)) +
    labs(x = "Shaded region shows a pointwise 95% credible interval for voting intention on each day.", 
         y = "Two party preferred voting intention for the ALP (%)",
         caption = "Source: seven polling firms collected on Wikipedia; analysis at freerangestats.info") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = middle)) +
    geom_point(data = elections_2pp, aes(x = mid_date, y = intended_vote * 100), 
               colour = "red", size = 2, shape = 13) +
    scale_y_continuous(sec.axis = dup_axis(name = "")) +
    theme(panel.grid.minor = element_blank())
  
  return(p)
}

plot_results(model_2pp)
