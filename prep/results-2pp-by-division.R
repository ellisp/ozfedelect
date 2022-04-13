# This script reads in the House of Representative two party preferred results by division
#
# Peter Ellis 3 Marhc 2019


#=============From the tally room=============

read_div_res <- function(url, election_date, incumbent){
  election_year <- lubridate::year(as.Date(election_date))
  destfile <- paste0("raw-data/hr-", election_year, "-2pp-div.csv")
  
  download_if_fresh(url, destfile = destfile)
  
  tmp_2pp_div <- read_csv(destfile, skip = 1) %>%
    rename_all(to_snake_case) %>%
    rename(swing_to_govt = swing) %>%
    mutate(election_date = election_date,
           election_year = election_year,
           incumbent = incumbent,
           incumbent_votes = case_when(
             incumbent == "ALP" ~ australian_labor_party_votes,
             incumbent == "Lib/Nat" ~ liberal_national_coalition_votes
           ))
  
  names(tmp_2pp_div) <- gsub("australian_labor_party", "alp", names(tmp_2pp_div))
  names(tmp_2pp_div) <- gsub("liberal_national_coalition", "lib_nat", names(tmp_2pp_div))
  
  return(tmp_2pp_div)
}

config <- data.frame(
  url =c("https://results.aec.gov.au/24310/Website/Downloads/HouseTppByDivisionDownload-24310.csv",
         "https://results.aec.gov.au/20499/Website/Downloads/HouseTppByDivisionDownload-20499.csv",
         "https://results.aec.gov.au/17496/Website/Downloads/HouseTppByDivisionDownload-17496.csv",
         "https://results.aec.gov.au/15508/Website/Downloads/HouseTppByDivisionDownload-15508.csv",
         "https://results.aec.gov.au/13745/Website/Downloads/HouseTppByDivisionDownload-13745.csv",
         "https://results.aec.gov.au/12246/results/Downloads/HouseTppByDivisionDownload-12246.csv"),
  election_date = as.Date(c("2019-05-18", "2016-07-02", "2013-09-07", "2010-08-21", "2007-11-24", "2004-10-09")),
  incumbent =     c("Lib/Nat", "Lib/Nat",    "ALP",        "ALP",        "Lib/Nat",    "Lib/Nat"),
  stringsAsFactors = FALSE
  )

recent_divs <- lapply(1:nrow(config), function(x){
  read_div_res(config[x , "url"], config[x, "election_date"], config[x, "incumbent"])
}) %>%
  do.call("rbind", .)


# check the understanding of 'swing'
# Ballarat in 2016 was a swing of 2.43% to the ALP ie against the govt (https://results.aec.gov.au/20499/website/HouseDivisionPage-20499-198.htm)
# Banks was a swing of 1.4% to the ALP (https://www.abc.net.au/news/federal-election-2016/guide/bank/)
# Bennelong was a swing of 1.95% to the LP
# Ballarat in 2010 was a swing of 3.55% to the ALP ie to the Govt (ALP won in 2007) (http://www.abc.net.au/elections/federal/2010/guide/ball.htm)
# Banks was 8.9% against govt to Lib (https://www.abc.net.au/elections/federal/2010/guide/bank.htm)
recent_divs %>%
  filter(division_nm %in% c("Ballarat", "Banks", "Bennelong") & election_year %in% c(2010, 2016)) %>%
  select(division_nm, party_ab, swing_to_govt, election_year)
# that all checks out

#===============================Pre 2004=========================

#-------------------2001-----------------------------

# for some reason the "percentages" are out of 10000 eg 9317 means 93.17%
# but swing is an actual percentage

divs2001 <- read_csv2("raw-data/2001/data/import/divs.txt") %>%
  mutate(alp_percentage = as.numeric(`ALP TPP Pc`) / 100,
         lib_nat_percentage = 100 - alp_percentage,
         swing_to_govt = -as.numeric(`ALP Swing`)) %>%
  select(division_nm = Division,
         state_ab = State,
         lib_nat_votes = `Coalition TPP Vote`,
         lib_nat_percentage,
         alp_votes = `ALP TPP Vote`,
         alp_percentage,
         total_votes = `Formal Vote`,
         swing_to_govt
  ) %>%
  mutate(division_id = NA,
         party_ab = NA,
         incumbent = "Lib/Nat",
         incumbent_votes = lib_nat_votes,
         election_date = "2001-11-10",
         election_year = 2001)
  # I'm not sure the "ALP Swing" here is calculated the same as "swing" in the tally room

expect_equal(
  divs2001 %>%
    summarise(x = sum(alp_votes)) %>%
    pull(x),
  5627785)

expect_equal(
  divs2001 %>%
    summarise(x = sum(lib_nat_votes) / sum(total_votes) * 100)  %>%
    pull(x) %>%
    round(2),
  50.95)

# check some swings. 
# Banks should be 4.37 % to Liberal (ie to govt) https://en.wikipedia.org/wiki/Electoral_results_for_the_Division_of_Banks
# Bradfield should be -/13% to ALP ie against govt so -0.13
# Chifley should be +5.98 (https://en.wikipedia.org/wiki/Electoral_results_for_the_Division_of_Chifley)
# This all checks out.
divs2001 %>%
  select(swing_to_govt, division_nm)
  
  
# -----------------------1990s--------------------------
reader <- function(election_date, incumbent){
  election_date <- as.Date(election_date)
  election_year <- lubridate::year(election_date)
  yab <- substring(election_year, 3, 4)
  fn <- paste0("raw-data/1990s/data/import/tables", yab, "/DIVS.TXT")
  data <- read_csv2(fn) %>%
    mutate(election_date = election_date,
           election_year = election_year,
           incumbent = incumbent)
}

dates <- c( "1993-03-13", "1996-03-02","1998-10-03")
incumbents <- c("ALP", "ALP", "Lib/Nat")

old_divs <- lapply(1:3, function(x){
  reader(dates[x], incumbents[x])
}) %>%
  do.call("rbind", .) %>%
  mutate(alp_percentage = as.numeric(`ALP TPP Pc`) / 100,
                             lib_nat_percentage = 100 - alp_percentage,
                             swing_to_govt = as.numeric(`ALP Swing`) *
           ifelse(incumbent == "ALP", 1, -1)) %>%
  select(division_nm = Division,
         state_ab = State,
         lib_nat_votes = `Coalition TPP Vote`,
         lib_nat_percentage,
         alp_votes = `ALP TPP Vote`,
         alp_percentage,
         total_votes = `Formal Vote`,
         swing_to_govt,
         incumbent,
         election_year,
         election_date
  ) %>%
  mutate(division_id = NA,
         party_ab = NA,
         incumbent_votes = ifelse(incumbent == "ALP",
                                  alp_votes,
                                  lib_nat_votes))

 


#=========================combine==============================
results_2pp_div <-rbind(recent_divs,
                        divs2001,
                        old_divs)

# Check we have the correct number of divisions:
expect_equal(
  results_2pp_div %>%
    group_by(election_year) %>%
    summarise(number_divs = n()) %>%
    pull(number_divs),
  c(147, 148, 148, rep(150, 6), 151)
)

save(results_2pp_div, file = "pkg/data/results_2pp_div.rda", compress = "xz")

