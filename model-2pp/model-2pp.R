#----------------set up----------------
source("setup.R")
rstan_options(auto_write = TRUE)
options(mc.cores = 7)


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

all_firms <- ozpolls %>%
  filter(!firm %in% c("Election result") &
           firm %in% firms_today) %>%
  pull(firm) %>%
  unique() %>%
  as.character()

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

# Data for one pollster as a time, elements in a list for ease of use:
one_pollster <- lapply(all_firms, function(x){
  filter(data_2pp, firm == x)
})

# Put all the data into a single list we can pass to Stan:
model_data <- list(
  number_elections = 5,
  election_days = election_days,
  election_results = election_results,
  
  # This inflator doubles the variance of polls; ie allows for non-sampling error
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

#------------------------Model fitting----------------------
# Takes about 20 minutes or so and diagnostics were ok
st <- system.time({
  model_2pp <- stan(file = 'model-2pp/model-2pp.stan', data = model_data, chains = 4, 
                    control = list(max_treedepth = 15),
                    iter = 1000)
})
print(st)

save(model_2pp, file = "output/model_2pp")

#-------------------------interpretation and presentation-------------------

# Polling firms

d <- as.data.frame(extract(model_2pp, "d")$d)
names(d) <- all_firms
pd <- d %>%
  gather(firm, overestimate) %>%
  mutate(firm = fct_reorder(firm, overestimate)) %>%
  ggplot(aes(x = overestimate, colour = firm, fill = firm)) +
#  facet_wrap(~firm) +
  geom_density(alpha = 0.3) +
  scale_fill_viridis_d("") +
  scale_colour_viridis_d("") +
  scale_x_continuous("Estimated overestimate of ALP two-party-preferred vote", label = percent_format()) +
  ggtitle("Over- and under-estimates of actual election results for Australian House of Representatives",
          "Drawn from a Bayesian state-space model of estimated actual daily voting tendency, 2007 to 2019") +
  labs(caption = "Source: seven polling firms' data collected on Wikipedia; analysis by freerangestats.info.")

CairoSVG("output/0145-density-d.svg", 8, 5)
print(pd)
dev.off()


# latent state space

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
         y = "Two-party-preferred voting intention for the ALP (%)",
         caption = "Source: seven polling firms' data collected on Wikipedia; analysis  by freerangestats.info.") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = middle)) +
    geom_point(data = data_2pp, aes(x = mid_date, y = intended_vote * 100), 
               colour = "steelblue", size = 0.7, shape = 19) +
    geom_point(data = elections_2pp, aes(x = mid_date, y = intended_vote * 100), 
               colour = "red", size = 4, shape = 13) +
    scale_y_continuous(sec.axis = dup_axis(name = "")) +
    theme(panel.grid.minor = element_blank())
  
  return(p)
}

p <- plot_results(model_2pp) +
  ggtitle(paste0("Australian federal voting intention to ", next_election),
          paste0("As at ", Sys.Date()))

CairoSVG("output/latest-model-results.svg", 8, 5)
print(p)
dev.off()


mu <- as.data.frame(rstan::extract(model_2pp, "mu"))
dim(mu)
election_sims <- mu[ , ncol(mu)]
mean(election_sims)
sd(election_sims)
