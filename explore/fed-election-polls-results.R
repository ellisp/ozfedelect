library(pscl)
library(scales)
library(tidyverse)

filter(ozpolls, intended_vote > 60) 

p <- ozpolls %>%
  filter(preference_type != "First preference") %>%
  ggplot(aes(x = mid_date, y = intended_vote)) +
  facet_wrap(~party) +
  geom_point(aes(colour = firm)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1, colour = "black")

print(p)



ggplot(AustralianElections, aes(x = ALP2PP / 100, y = ALPSeats / Seats)) +
  geom_abline(slope = 1, intercept = 0, colour = "white") +
  geom_hline(yintercept = 0.5, colour = "steelblue") +
  geom_vline(xintercept = 0.5, colour = "steelblue") +
  geom_path(colour = "grey75") +
  geom_text(aes(label = lubridate::year(date))) +
  #  geom_point() +
  scale_x_continuous("ALP Two party preferred overall vote", label = percent_format(accuracy = 1)) +
  scale_y_continuous("ALP seats in House of Representatives", label = percent_format(accuracy = 1))


ggplot(AustralianElections, aes(x = date, y = OtherSeats)) +
  geom_line(colour = "grey75") +
  geom_point() +
  labs(y = "Seats in House of Representatives one by parties other than ALP, LP and NP",
       x = "Election Date")
