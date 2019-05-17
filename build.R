source("setup.R")


#=====================Building package====================

# Download and clean polling data from Wikipedia:
source("prep/collect_polls.R")

# The divisions and their margins for the 2019 election
source("prep/download-pendulum-2019.R")

source("prep/define-colours.R")

# More official sources
source("prep/download-2001-and-earlier.R")
source("prep/results-2pp-by-division.R")
source("prep/download-import-boundaries.R")
source("prep/census-2016-summary-data.R")


document("pkg")
check("pkg")



#==============Modelling==================
source("model-2pp/model-2pp.R")

the_caption <- "Source: analysis by freerangestats.info with polling data on Wikipedia; last updated 17 May 2019"
source("model-2pp/model-interpretation.R")
