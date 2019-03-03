# This script reads in the House of Representative two party preferred results by division
#
# Peter Ellis 3 Marhc 2019


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
  
  return(tmp_2pp_div)
}

config <- data.frame(
  url =c("https://results.aec.gov.au/20499/Website/Downloads/HouseTppByDivisionDownload-20499.csv",
         "https://results.aec.gov.au/17496/Website/Downloads/HouseTppByDivisionDownload-17496.csv",
         "https://results.aec.gov.au/15508/Website/Downloads/HouseTppByDivisionDownload-15508.csv",
         "https://results.aec.gov.au/13745/Website/Downloads/HouseTppByDivisionDownload-13745.csv",
         "https://results.aec.gov.au/12246/results/Downloads/HouseTppByDivisionDownload-12246.csv"),
  election_date = c("2016-07-02", "2013-09-07", "2010-08-21", "2007-11-24", "2004-10-09"),
  incumbent =     c("Lib/Nat",    "ALP",        "ALP",        "Lib/Nat",    "Lib/Nat"),
  stringsAsFactors = FALSE
  )

results_2pp_div <- lapply(1:nrow(config), function(x){
  read_div_res(config[x , "url"], config[x, "election_date"], config[x, "incumbent"])
}) %>%
  do.call("rbind", .)


save(results_2pp_div, file = "pkg/data/results_2pp_div.rda")

