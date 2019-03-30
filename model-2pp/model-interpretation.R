load("output/model_2pp.rdata")

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
  ggtitle("House effects in polls for the Australian House of Representatives",
          "Drawn from a Bayesian state-space model of estimated actual daily voting tendency, 2007 to 2019") +
  labs(caption = "Source: seven polling firms' data collected on Wikipedia; analysis by freerangestats.info.")

CairoSVG("output/latest-polling-firm-density.svg", 8, 5)
print(pd)
dev.off()


# latent state space

ex <- as.data.frame(rstan::extract(model_2pp, "mu"))
names(ex) <- 1:model_data$election_days[5]

mod_results <- ex %>%
  gather(day, value) %>%
  mutate(day = as.numeric(day),
         day = as.Date(day - 1, origin = first_election),
         value = value * 100) %>%
  group_by(day) %>%
  summarise(middle = mean(value),
            upper = quantile(value, 0.975),
            lower = quantile(value, 0.025)) 

last_day <- tail(mod_results, 1)$day
last_lower <- round(tail(mod_results, 1)$lower,1)
last_upper <- round(tail(mod_results, 1)$upper, 1)
ci <- paste0("ALP two-party-preferred vote on the day estimated to be between ", last_lower, 
             "% and ", last_upper, "%.") 

p <- mod_results %>%
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
  theme(panel.grid.minor = element_blank()) +
  ggtitle(paste0("Australian federal voting intention to ", next_election),
          ci)
  
CairoSVG("output/latest-model-results.svg", 8, 5)
print(p)
dev.off()


mu <- as.data.frame(rstan::extract(model_2pp, "mu"))
dim(mu)
election_sims <- mu[ , ncol(mu)]
mean(election_sims)
sd(election_sims)

#-----------converting to seats---------------
# individual level swing statndard deviation is 3.2
# last election was 47.52 ALP 

oz_pendulum_2019


