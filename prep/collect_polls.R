
#--------------2010 election-----------------
url = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2010_Australian_federal_election"
tab_names = c("dates", 
              paste0(c("ALP", "Lib", "Nat", "Grn", "Oth"), "!First preference"), 
              "ALP!Two-party-preferred", "Lib/Nat!Two-party-preferred", 
              "x1", "x2", "x3")

last_election_date = as.Date("2007-11-24")
election_year = 2010

webpage <- url %>%
  read_html(encoding = "UTF-8") 

tabs <- webpage %>%
  html_nodes("table") 

# number below depends on the webpage...
tab1 <- html_table(tabs[[1]], fill = TRUE) 

names(tab1) <- tab_names

tab1 <- tab1[-1, ] %>%
  as_tibble() %>%
  select(-starts_with("x")) %>%
  mutate(firm = "Newspoll")

tab2 <- html_table(tabs[[5]], fill = TRUE) 

names(tab2) <- tab_names

tab2 <- tab2[-1, ] %>%
  as_tibble() %>%
  select(-starts_with("x")) %>%
  mutate(firm = "Roy Morgan")


ozpolls_2010 <- tab1 %>%
  rbind(tab2) %>%
  mutate(wiki_row = paste0("r", 1:n())) %>%
  gather(variable, intended_vote, -dates, -firm, -wiki_row) %>%
  separate(variable, sep = "!", into = c("party", "preference_type")) %>%
  mutate(intended_vote = suppressWarnings(as.numeric(gsub("%", "", intended_vote, fixed = TRUE)))) %>%
  mutate(election_year = election_year) %>%
  filter(!is.na(intended_vote)) %>%
  parse_dates() %>%
  mutate(start_date = if_else(original_dates == "2007 Election", last_election_date, start_date),
         end_date = if_else(original_dates == "2007 Election", last_election_date, end_date),
         firm = if_else(original_dates == "2007 Election", "Election result", firm))

stopifnot(sum(is.na(ozpolls_2010$start_date)) == 0 )

#--------------2013 election-----------------
url = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2013_Australian_federal_election"
tab_names = c("dates", "firm",
              paste0(c("ALP", "Lib/Nat", "Grn", "Oth"), "!First preference"), 
              "ALP!Two-party-preferred", "Lib/Nat!Two-party-preferred", 
              "x1")

last_election_date = as.Date("2010-06-24")
election_year = 2013

webpage <- url %>%
  read_html(encoding = "UTF-8") 

tabs <- webpage %>%
  html_nodes("table") 

# number below depends on the webpage...
tab <- html_table(tabs[[2]], fill = TRUE) 

names(tab) <- tab_names

ozpolls_2013 <- tab %>%
  as_tibble() %>%
  mutate(wiki_row = paste0("r", 1:n())) %>%
  select(-starts_with("x")) %>%
  gather(variable, intended_vote, -dates, -firm, -wiki_row) %>%
  separate(variable, sep = "!", into = c("party", "preference_type")) %>%
  mutate(intended_vote = suppressWarnings(as.numeric(gsub("%", "", intended_vote, fixed = TRUE)))) %>%
  mutate(election_year = election_year) %>%
  filter(!is.na(intended_vote)) %>%
  parse_dates() %>%
  mutate(start_date = if_else(original_dates == "2010 election", last_election_date, start_date),
         end_date = if_else(original_dates == "2010 election", last_election_date, end_date),
         firm = if_else(original_dates == "2010 election", "Election result", firm)) %>%
  mutate(firm = gsub("\\[.+\\]", "", firm),
         firm = str_squish(firm))

stopifnot(sum(is.na(ozpolls_2013$start_date)) == 0 )


#-----------------2016-------------
url <- "https://en.wikipedia.org/wiki/National_opinion_polling_for_the_2016_Australian_federal_election"
# parties in different order from 2013 and 2010:
tab_names = c("dates", "firm",
              paste0(c("Lib/Nat", "ALP", "Grn", "Oth"), "!First preference"), 
              "Lib/Nat!Two-party-preferred", "ALP!Two-party-preferred",  
              "sample_size", "margin_of_error", "method",
              "x1")

last_election_date = as.Date("2013-09-07")
election_year = 2016

webpage <- url %>%
  read_html(encoding = "UTF-8") 

tabs <- webpage %>%
  html_nodes("table") 

# number below depends on the webpage...
tab <- html_table(tabs[[3]], fill = TRUE) 

names(tab) <- tab_names

ozpolls_2016 <- tab %>%
  as_tibble() %>%
  mutate(wiki_row = paste0("r", 1:n())) %>%
  select(-starts_with("x")) %>%
  gather(variable, intended_vote, -dates, -firm, -sample_size, -margin_of_error, -method, -wiki_row) %>%
  separate(variable, sep = "!", into = c("party", "preference_type")) %>%
  mutate(intended_vote = suppressWarnings(as.numeric(gsub("%", "", intended_vote, fixed = TRUE)))) %>%
  mutate(sample_size = as.numeric(gsub(",", "", sample_size))) %>%
  mutate(election_year = election_year) %>%
  filter(!is.na(intended_vote)) %>%
  parse_dates() %>%
  mutate(start_date = if_else(original_dates == "2013 election", last_election_date, start_date),
         end_date = if_else(original_dates == "2013 election", last_election_date, end_date),
         firm = if_else(original_dates == "2013 election" | firm == "2013 election", "Election result", firm)) %>%
  mutate(firm = gsub("\\[.+\\]", "", firm),
         firm = gsub("\\(.+\\)", "", firm),
         firm = str_squish(firm))

stopifnot(sum(is.na(ozpolls_2016$start_date)) == 0 )

#-----------------2019-------------
url <- "https://en.wikipedia.org/wiki/National_opinion_polling_for_the_2019_Australian_federal_election"
# parties in different order from 2013 and 2010:
tab_names = c("dates", "firm",
              paste0(c("Lib/Nat", "ALP", "Grn", "ONP", "Oth"), "!First preference"), 
              "Lib/Nat!Two-party-preferred", "ALP!Two-party-preferred",  
              "x1", "x2", "x3")

last_election_date = as.Date("2016-07-02")
election_year = 2019

webpage <- url %>%
  read_html(encoding = "UTF-8") 

tabs <- webpage %>%
  html_nodes("table") 

# number below depends on the webpage...
tab <- html_table(tabs[[2]], fill = TRUE) 

names(tab) <- tab_names

ozpolls_2019 <- tab %>%
  as_tibble() %>%
  mutate(wiki_row = paste0("r", 1:n())) %>%
  select(-starts_with("x")) %>%
  gather(variable, intended_vote, -dates, -firm, -wiki_row) %>%
  separate(variable, sep = "!", into = c("party", "preference_type")) %>%
  mutate(intended_vote = suppressWarnings(as.numeric(gsub("%", "", intended_vote, fixed = TRUE)))) %>%
  mutate(election_year = election_year) %>%
  filter(!is.na(intended_vote)) %>%
  parse_dates() %>%
  mutate(start_date = if_else(original_dates == "2 July 2016 election", last_election_date, start_date),
         end_date = if_else(original_dates == "2 July 2016 election", last_election_date, end_date),
         firm = if_else(original_dates == "2 July 2016 election", "Election result", firm)) %>%
  mutate(firm = gsub("\\[.+\\]", "", firm),
         firm = gsub("\\(.+\\)", "", firm),
         firm = str_squish(firm))

stopifnot(sum(is.na(ozpolls_2019$start_date)) == 0 )

#----------------Combine-----------------

ozpolls <- ozpolls_2010 %>%
  rbind(ozpolls_2013) %>%
  rbind(select(ozpolls_2016, -sample_size, -margin_of_error, -method)) %>%
  rbind(ozpolls_2019) %>%
  mutate(firm = str_squish(gsub("\\(.+\\)", "", firm)),
         firm = ifelse(firm == "Morgan", "Roy Morgan", firm),
         firm = ifelse(firm == "ReachTel", "ReachTEL", firm)) %>%
  mutate(mid_date = start_date + (as.numeric(end_date) - as.numeric(start_date)) / 2) %>%
  mutate(party = ifelse(party %in% c("Lib", "Nat"), "Lib/Nat", party)) %>%
  group_by_if(function(x){!is.numeric(x) | min(x) > 1000}) %>%
  summarise(intended_vote = sum(intended_vote)) %>%
  ungroup()

save(ozpolls, file = "pkg/data/ozpolls.rda")

# Two of the data frames have reasons for independent existence (see helpfile for why):
save(ozpolls_2016, file = "pkg/data/ozpolls_2016.rda")
save(ozpolls_2010, file = "pkg/data/ozpolls_2010.rda")

