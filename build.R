source("setup.R")


#=====================Building package====================

# Download and clean polling data from Wikipedia:
source("prep/collect_polls.R")
source("prep/download-2001-and-earlier.R")
source("prep/results-2pp-by-division.R")
source("prep/download-import-boundaries.R")
source("prep/census-2016-summary-data.R")

# Fit model (takes 20 minutes so beware)
# source("model-2pp/model-2pp.R")


document("pkg")
check("pkg")
