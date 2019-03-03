

#=======================Two party preferred by polling place===============

#------------------------2016--------------------
url <- "https://results.aec.gov.au/20499/Website/Downloads/HouseTppByPollingPlaceDownload-20499.csv"

destfile = "raw-data/HouseTppByPollingPlace2016.csv"
download_if_fresh(url, destfile = destfile)

hr_2016_2pp <- read_csv(destfile, skip = 1) %>%
  rename_all(to_snake_case) %>%
  rename(swing_against_lib_nat = swing)

hr_2016_2pp %>%
  group_by(division_nm) %>%
  summarise(alp = sum(australian_labor_party_votes) / sum(total_votes)) %>%
  arrange(desc(alp))

# a negative swing means a swing against the government; positive is a swing to the government
hr_2016_2pp %>%
  group_by(division_nm) %>%
  summarise(alp = sum(australian_labor_party_votes) / sum(total_votes),
            # this isn't quite right because it doesn't take into account different number voting last election:
            swing_against_lib_nat = sum(swing_against_lib_nat * total_votes) / sum(total_votes)) %>%
  ggplot(aes(x = alp, y = swing_against_lib_nat)) +
  geom_point()

# These numnbers don't add up to the total vote. Postal votes must be excluded from this polling place vote:
sum(hr_2016_2pp$australian_labor_party_votes)
sum(hr_2016_2pp$liberal_national_coalition_votes)

#---------------------2013-----------------------------
url <- "https://results.aec.gov.au/17496/Website/Downloads/HouseTppByPollingPlaceDownload-17496.csv"

destfile = "raw-data/HouseTppByPollingPlace2013.csv"
download_if_fresh(url, destfile = destfile)

hr_2013_2pp <- read_csv(destfile, skip = 1) %>%
  rename_all(to_snake_case) %>%
  rename(swing_against_lib_nat = swing)

