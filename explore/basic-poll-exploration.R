ozpolls %>%
  ggplot(aes(x = mid_date, y = intended_vote, colour = party)) +
  geom_point(size = 0.2) +
  geom_smooth(span = 0.1, se = FALSE) +
  facet_wrap(~preference_type) +
  scale_colour_manual(values = oz_party_cols)


