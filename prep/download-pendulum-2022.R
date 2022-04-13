redist_page <- read_html("https://antonygreen.com.au/2022-federal-electoral-pendulum/")
redist_data <- html_table(redist_page)[[1]] 

coal_seats <- redist_data[2:77, 1:2]
alp_seats <- redist_data[2:70, 3:4]
oth_seats <- redist_data[74:79, 3:4]

names(coal_seats) <- names(alp_seats) <- names(oth_seats) <- c("var", "state")

oz_pendulum_2022 <- rbind(coal_seats, alp_seats, oth_seats) %>%
  as_tibble() %>%
  separate(var, into = c("margin", "incumbent"), sep = " ") %>%
  separate(state, into = c("division", "state"), sep = "\\(") %>%
  mutate(division = str_squish(division)) %>%
  mutate(state = gsub("[()]", "", state)) %>%
  mutate(remarks = str_extract(state, "\\(.+\\)"),
         remarks = gsub("[\\(\\)]", "", remarks),
         state = str_squish(gsub("\\(.+\\)", "", state)),
         party_against = case_when(
           incumbent == "ALP"              ~ "Lib/Nat",
           incumbent %in% c("LIB", "NAT", "LNP")  ~ "ALP",
           TRUE                            ~ str_squish(gsub(".*v", "", state))
         )) %>%
  mutate(state = substr(state, 1, 3)) %>%
  select(state, division, incumbent, margin, party_against, remarks) %>%
  mutate(margin = as.numeric(margin))

save(oz_pendulum_2022, file = "pkg/data/oz_pendulum_2022.rda")
