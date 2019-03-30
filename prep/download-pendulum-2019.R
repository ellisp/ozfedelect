
redist_page <- read_html("https://www.abc.net.au/news/elections/federal-redistribution-2018/")
redist_data <- html_table(redist_page)[[2]] 



coal_seats <- redist_data[2:74, 1:3]
alp_seats <- redist_data[2:73, 4:6]
oth_seats <- redist_data[75:80, 4:6]

names(coal_seats) <- names(alp_seats) <- names(oth_seats) <- c("state", "division", "var")

oz_pendulum_2019 <- rbind(coal_seats, alp_seats, oth_seats) %>%
  as_tibble() %>%
  separate(var, into = c("incumbent", "margin"), sep = " ") %>%
  mutate(remarks = str_extract(division, "\\(.+\\)"),
         remarks = gsub("[\\(\\)]", "", remarks),
         division = str_squish(gsub("\\(.+\\)", "", division)),
         party_against = case_when(
           incumbent == "ALP"              ~ "Lib/Nat",
           incumbent %in% c("LIB", "NAT", "LNP")  ~ "ALP",
           TRUE                            ~ str_squish(gsub("[v\\+]", "", remarks))
         )) %>%
  select(state, division, incumbent, margin, party_against, remarks)

save(oz_pendulum_2019, file = "pkg/data/oz_pendulum_2019.rda")
