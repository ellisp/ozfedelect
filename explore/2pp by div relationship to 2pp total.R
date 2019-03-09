# -7.5 for Aston in 2013 meant a swing against the Labor govt
# - 10.1 for Bass in 2016 meant a swing against the Lib/Nat govt

results_2pp_div %>%
  select(division_nm, swing_to_govt, election_year) %>%
  spread(election_year, swing_to_govt)

p1 <- results_2pp_div %>%
  mutate(year_inc = paste(election_year, incumbent, "\nincumbent")) %>%
  ggplot(aes(x = swing_to_govt / 100)) +
  geom_vline(xintercept = 0, colour = "steelblue") +
  scale_x_continuous("Swing to the incumbent government", label = percent) +
  geom_density() +
  geom_rug() +
  facet_wrap(~year_inc, nrow = 1) +
  coord_flip()

p1
p1 + xlim(c(-0.2, 0.2))

results_2pp_div  %>%
  group_by(election_year) %>%
  mutate(aust_alp_2pp = sum(alp_votes) / sum(total_votes)) %>%
  ggplot(aes(x = aust_alp_2pp, y = alp_percentage / 100,
             colour = as.ordered(election_year))) +
  geom_point() +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1))


d <- results_2pp_div  %>%
  group_by(election_year, incumbent) %>%
  mutate(avg_swing = sum(swing_to_govt * total_votes) / sum(total_votes)) %>%
  ungroup() %>%
  filter(abs(swing_to_govt) < 20) %>%
  mutate(year = fct_reorder(as.ordered(election_year), avg_swing))
  
avgs <- distinct(d, avg_swing, year, incumbent)

update_geom_defaults("label", list(family = main_font, fill = "white", colour = "grey10", alpha = 0.8))

p7 <- d %>%
  ggplot(aes(x = avg_swing / 100, y = swing_to_govt / 100)) +
  geom_smooth(se = FALSE, method = "lm") +
  geom_vline(xintercept = 0, colour = "grey20") +
  geom_hline(yintercept = 0, colour = "grey20") +
  geom_point(aes(colour = incumbent)) +
  geom_text(data = avgs, y = -0.18, aes(label = year, colour = incumbent)) +
  labs(caption = "Source: Australian Electoral Commission data, analysed by freerangestats.info.") +
  scale_x_continuous("Overall swing towards incumbent government", label = percent_format(accuracy = 1)) +
  scale_y_continuous("Division level swings to government", label = percent_format(accuracy = 1)) +
  annotate("label", x = -0.053, y = 0.06, label = "Strongly change\ngovernment") +
  annotate("label", x = -0.046, y = -0.065, label = "Narrow escape for\nHoward in 1998") +
  annotate("label", x = -0.028, y = -0.1, label = "Near ties for Gillard and\nTurnbull in 2010 and\n2016 respectively") +
  annotate("label", x = 0.0055, y = 0.04, label = "Strongly retain\ngovernment") +
  scale_colour_manual("Incumbent government:", values = c("ALP" = "red", "Lib/Nat" = "blue")) +
  ggtitle("Individual seats face more voting uncertainty than Australia as a whole",
          "Residual standard deviation of about 3.2 percentage points once the nation-wide swing has been observed")

CairoSVG("output/p7.svg", 13, 9)
print(p7)
dev.off()

model <- lm(swing_to_govt ~ avg_swing, data = d)
confint(model)
coef(model) # of course the slope is 1 and the intercept is 0, basically - by design
# the interesting thing is actually the residual standard error:
summary(model)

residual_sd <- summary(model)$sigma


#==============crude simulation===============
# so plausible to 
# a) model the overall swing to the govt
# b) simulate for individual seats a randomness of N(0, 3.2) on top of that overall swing
# c) add those swings to the 2016 results

# Important - this misses out on two things that are needed:
# a) 2 new electorates this time around
# b) seats held by other parties

last_result <- results_2pp_div %>%
  filter(election_year == 2016) %>%
  summarise(alp_2pp_2016 = sum(alp_votes) / sum(total_votes)) %>%
  pull(alp_2pp_2016)

# last election the ALP got 49.6% of the 2pp. Currently on track to about 52 with sd of about 3.
# So an average swing against the govt of N(2.5, 3), and division-level randomness on top of that.

baseline <- results_2pp_div %>%
  filter(election_year == 2016) %>%
  select(division_nm, alp_percentage) %>%
  mutate(link = 1)

nsims <- 1e5
set.seed(321)
sims <- tibble(sim = 1:nsims, link = 1) %>%
  mutate(avg_swing = rnorm(n(), -2.5, 3)) %>%
  full_join(baseline, by = "link") %>%
  select(-link) %>%
  mutate(extra_swing = rnorm(n(), 0, residual_sd)) %>%
  group_by(sim) %>%
  # we scale the extra_swing to be mean zero so we aren't accidentally changing the average swing:
  mutate(extra_swing = extra_swing - mean(extra_swing),
         total_swing = avg_swing + extra_swing,
         alp_percentage_2019 = alp_percentage - total_swing) %>%
  ungroup()


sims_by_div <- sims %>%
  group_by(sim) %>%
  summarise(avg_swing = unique(avg_swing),
            number_seats_alp = sum(alp_percentage_2019 > 50),
            prop_seats_alp = mean(alp_percentage_2019 > 50)) %>%
  ungroup()

m <- sims_by_div %>%
  summarise(m = round(mean(prop_seats_alp > 0.5) * 100, 1)) %>%
  pull(m)

sims_by_div %>%
  ggplot(aes(x = number_seats_alp)) +
  geom_histogram(alpha = 0.5, binwidth = 1, colour = "grey") +
  geom_vline(xintercept = 75.5, colour = "steelblue") +
  scale_x_continuous("Number of House of Representative seats won by ALP") +
  scale_y_continuous("Number of simulations\n(out of 100,000)", label = comma ) +
  ggtitle(paste0(m, "% probability of ALP win in the 2019 Federal Election"))

    