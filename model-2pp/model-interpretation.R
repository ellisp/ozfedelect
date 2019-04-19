load("output/model_2pp.rdata")

#-------------------------House effects-------------------

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

svglite("output/latest-polling-firm-density.svg", 8, 5)
print(pd)
dev.off()


#------------------two party preferred------------

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
ci <- paste0("ALP vote on election day is estimated to be between ", last_lower, 
             " and ", last_upper, " percent") 

p <- mod_results %>%
  ggplot(aes(x = day)) +
  labs(x = "Shaded region shows a pointwise 95% credible interval for voting intention on each day", 
       y = "Two-party-preferred voting intention for the ALP (%)",
       caption = "Source: seven polling firms' data collected on Wikipedia; analysis  by freerangestats.info") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(aes(y = middle)) +
  geom_point(data = data_2pp, aes(x = mid_date, y = intended_vote * 100), 
             colour = "steelblue", size = 0.5, shape = 19) +
  geom_point(data = elections_2pp, aes(x = mid_date, y = intended_vote * 100), 
             colour = "red", size = 4, shape = 13) +
  scale_y_continuous(sec.axis = dup_axis(name = "")) +
  theme(panel.grid.minor = element_blank()) +
  ggtitle(ci,
          paste0("Australian two-party-preferred federal voting intention to ", format(next_election, format = "%d %B %Y")))
  
svglite("output/latest-model-results.svg", 8, 5)
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

alp_v_coal <- oz_pendulum_2019[1:142, ] %>%
  mutate(margin = ifelse(party_against == "ALP", -margin, margin),
         pos_winner = "ALP",
         alt_winner = "Lib/Nat", 
         alp_v_coal = 1) %>%
  select(state, division, pos_winner, margin, alt_winner, alp_v_coal) %>%
  rbind(tibble(
    state = "VIC",
    division = "Indi",
    pos_winner = "ALP",
    margin = -8.8,
    alt_winner = "Lib/Nat",
    alp_v_coal = 1
  ))

alp_v_other <- oz_pendulum_2019 %>%
  filter(division %in% c("Wills", "Cooper", "Grayndler", "Clark")) %>%
  mutate(pos_winner = "ALP",
         margin = c(4.9, 1.3, 15.8, -17.8),
         alt_winner = c("Grn", "Grn", "Grn", "Ind"), 
         alp_v_coal = 0) %>%
  select(state, division, pos_winner, margin, alt_winner, alp_v_coal)

other_v_coal <- oz_pendulum_2019 %>%
  filter(division %in% c("Wentworth", "Mayo", "Kennedy", "Melbourne")) %>%
  mutate(coal_margin = margin,
         pos_winner = ifelse(incumbent %in% c("ALP", "KAP", "NXT"), 
                             incumbent, 
                             str_to_sentence(incumbent)),
         alt_winner = "Lib/Nat", 
         alp_v_coal = 0) %>%
  select(state, division, pos_winner, margin, alt_winner, alp_v_coal)

all_divs <- rbind(alp_v_coal, alp_v_other, other_v_coal)


n_sims <- 10000
n_divs <- 151

swing_centre <- tail(mod_results, 1)$middle - 49.64
swing_sd <-  sd(ex[ , ncol(ex)] * 100)

all_divs_sims <- all_divs[rep(1:n_divs, n_sims), ] %>%
  mutate(sim_number = rep(1:n_sims, each = n_divs),
         overall_swing = rep(rnorm(n_sims, swing_centre, swing_sd), each = n_divs),
         total_swing = rnorm(n_sims * n_divs, 0, 3.2) + alp_v_coal * overall_swing,
         winner = ifelse((margin + total_swing) > 0, pos_winner, alt_winner))

sim_summary <- all_divs_sims %>%
  group_by(sim_number, winner) %>%
  summarise(seats_won = n()) %>%
  ungroup() %>%
  complete(sim_number, winner, fill = list(seats_won = 0)) %>%
  mutate(winner = fct_reorder(winner, -seats_won, .fun = median))
  

alp_wins <- sim_summary %>%
  summarise(alp_wins = (mean(seats_won[winner == "ALP"] >= 76) %>%
                          prod(100) %>%
                          round(1) %>%
                          paste0(" percent"))) %>%
  pull(alp_wins)

svglite("output/seat-sims.svg", 8, 5)  
print(sim_summary %>%
  ggplot(aes(x = seats_won, fill = winner, y=..density..)) +
  facet_wrap(~winner, scales = "free_y") +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 75.5) +
  scale_fill_manual(values = oz_party_cols) +
  labs(x = "Seats won",
       y = "Proportion of simulations", 
       fill = "",
       caption = "Source: modelling by freerangestats.info") +
  ggtitle(paste("The estimated probability of ALP winning 76 or more seats is", alp_wins),
          paste("Forecast number of seats out of 151 in the House of Representatives on", format(next_election, format = "%d %B %Y")))
)
dev.off()

divs <- all_divs_sims %>%
  group_by(state, division, winner) %>%
  summarise(freq = n()) %>%
  group_by(division) %>%
  mutate(prop = freq / sum(freq)) %>%
  select(-freq) %>%
  mutate(marginality = sqrt((0.5 - prop) ^ 2)) %>%
  ungroup() %>%
  mutate(division = fct_reorder(division, -marginality, .fun = min)) %>%
  select(-marginality) %>%
  spread(winner, prop, fill = 0) %>%
  gather(winner, prop, -state, -division) %>%
  mutate(winner = fct_reorder(winner, -prop))
  
all_states <- unique(divs$state)
draw_state_heatmap <- function(the_state = "NSW",
                               height = 9){
  
  d <- divs %>%
    filter(state == the_state) %>%
    mutate(division = fct_drop(division))
  
  p <- d %>%
    ggplot(aes(x = winner, y = division, fill = prop)) +
    geom_tile(colour = "white") +
    geom_segment(aes(y = as.numeric(division) + 0.5, yend = as.numeric(division) + 0.5),
                 x = 0, xend = Inf, colour = "grey90", size = 0.8) +
    geom_segment(aes(y = as.numeric(division) - 0.5, yend = as.numeric(division) - 0.5),
                 x = 0, xend = Inf, colour = "grey90", size = 0.8) +
    scale_fill_gradientn(colours = brewer.pal(5, "RdPu"))+
    geom_text(aes(label = ifelse(prop > 0.05, round(prop, 2), "")), 
              colour = "white", size = 2.5) +
    theme(panel.grid = element_blank(),
          legend.position = "right") +
    labs(y = "",
         x = "Winning party",
         fill = "Frequency\nof outcome\nin simulations",
         caption = "Source: modelling by freerangestats.info.") +
    scale_x_discrete(expand = c(0, 0), position = "top") +
    ggtitle(paste0("Likely election outcomes in ", the_state, " divisions"),
            "Divisions ordered from most to least marginal") +
    expand_limits(fill = c(0, 1))
    
  svglite(paste0("output/", the_state, ".svg"), 8, height)
    print(p)
  dev.off()
}

draw_state_heatmap("NSW", 10.5)
draw_state_heatmap("ACT", 2)
draw_state_heatmap("NT", 2)
draw_state_heatmap("QLD", 7)
draw_state_heatmap("VIC", 9.5)
draw_state_heatmap("WA", 5)
draw_state_heatmap("SA", 3.5)
draw_state_heatmap("TAS", 2.5)

svgs_to_copy <- paste0(c(
  "seat-sims",
  "latest-model-results",
  "latest-polling-firm-density",
  all_states
), ".svg")

lapply(paste0("output/", svgs_to_copy), svg_googlefonts)

file.copy(paste0("output/", svgs_to_copy),
          paste0("~/blog/ellisp.github.io/img/ozpolls/", svgs_to_copy), overwrite = TRUE)



#---------------party prediction intervals-----------

sim_summary %>%
  rename(Party = winner) %>%
  group_by(Party) %>%
  summarise(`Lower estimate` = round(quantile(seats_won, 0.1)),
            `Upper estimate` = round(quantile(seats_won, 0.9))) %>%
  kable("html") %>%
  writeLines("output/ozpolls2019-table.html")

file.copy("output/ozpolls2019-table.html",
          "~/blog/ellisp.github.io/_includes/ozpolls2019-table.html", overwrite = TRUE)

# Convert SVGs to PNGs for use on Twitter
# todo - move this convert_pngs() function into frs R package:
source("~/blog/ellisp.github.io/_R/utilities.R")
convert_pngs("*", "~/blog/ellisp.github.io/img/ozpolls")


#----------------number of ALP seats-----
# See https://www.sportsbet.com.au/betting/politics/australian-federal-politics

sim_summary %>%
  filter(winner == "ALP") %>%
  mutate(seats_five = cut(seats_won, breaks = 1:30 * 5)) %>%
  group_by(seats_five) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  mutate(prop = freq / sum(freq),
         fair_return = 1/ prop)


