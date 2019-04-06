# Exploratory graphics

p <- ozpolls %>%
  filter(preference_type == "First preference") %>%
  mutate(party = fct_reorder(party, intended_vote)) %>%
  ggplot(aes(x = mid_date, y = intended_vote)) +
  facet_wrap(~party) +
  geom_point(aes(colour = firm), size = 0.5) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1, colour = "black") +
  ggtitle("First preference vote intention",
          "Australian federal politics, 2007 to 2019") +
  labs(x = "", 
       y = "Estimated percentage of voters", 
       colour = "",
       caption = "Source: polling data on Wikipedia; analysis by Peter Ellis at freerangestats.info.") +
  theme(legend.position = c(0.83, 0.2)) +
  guides(colour = guide_legend(ncol=2))

svglite("output/0145-first-pref.svg", 9, 6)
print(p)
dev.off()



p2 <- ggplot(AustralianElections, aes(x = ALP2PP / 100, y = ALPSeats / Seats)) +
  geom_abline(slope = 1, intercept = 0, colour = "white") +
  geom_hline(yintercept = 0.5, colour = "steelblue") +
  geom_vline(xintercept = 0.5, colour = "steelblue") +
  geom_path(colour = "grey75") +
  geom_text(aes(label = lubridate::year(date))) +
  #  geom_point() +
  scale_x_continuous("ALP two party preferred overall vote", label = percent_format(accuracy = 1)) +
  scale_y_continuous("ALP seats in House of Representatives", label = percent_format(accuracy = 1)) +
  ggtitle("Relationship of two-party-preferred vote to seats in Parliament",
          "Actual historical election results for the Australian House of Representatives") +
  labs(caption = "Source: election data collected by Simon Jackman; analysis by Peter Ellis at freerangestats.info.")

svglite("output/0145-2pp-seats.svg", 8, 6)
print(p2)
dev.off()


p3 <- ggplot(AustralianElections, aes(x = date, y = OtherSeats)) +
  geom_line(colour = "grey75") +
  geom_point() +
  labs(y = "Seats won",
       x = "Election Date",
       caption = "Source: election data collected by Simon Jackman; analysis by Peter Ellis at freerangestats.info.") +
  ggtitle("Seats won by parties other than Australian Labor Party, Liberal Party and National Party",
          "Election results for the Australian House of Representatives")

svglite("output/0145-other-parties.svg", 8, 6)
print(p3)
dev.off()
