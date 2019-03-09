# This script downloads the historical, pre the results on the web "Tally Room", which date from 2004
#
# These appear to be old CD Roms for researchers
#
# Peter Ellis 9 March 2019

zipfiles <- c("aec-2001-election-statistics.zip",
              "aec-1993-1996-1998-election-statistics.zip")

for(f in zipfiles){
  df <- paste0("raw-data/", f)
  download_if_fresh(paste0("https://www.aec.gov.au/About_AEC/Publications/statistics/files/", f),
                    destfile = df)
 
}


unzip("raw-data/aec-2001-election-statistics.zip", exdir = "raw-data/2001")
unzip("raw-data/aec-1993-1996-1998-election-statistics.zip", exdir = "raw-data/1990s")

