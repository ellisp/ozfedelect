library(frs)
library(tidyverse)
library(scales)
library(rstan)
library(rvest)
library(pscl)
library(scales)
library(extrafont)
library(Cairo)
library(devtools)
library(knitr)
library(snakecase)
library(testthat)

run_all_r_scripts("R", cleanup = FALSE)


myfont <- "Roboto"
main_font <- "Roboto"
heading_font <- "Sarala"


theme_set(theme_light(base_family = main_font) + 
            theme(legend.position = "bottom") +
            theme(plot.caption = element_text(colour = "grey50"),
                  strip.text = element_text(size = rel(1), face = "bold"),
                  plot.title = element_text(family = heading_font))
) 
update_geom_defaults("text", list(family = main_font))

# Download and clean polling data from Wikipedia:
source("prep/collect_polls.R")
source("prep/results-2pp-by-division.R")

# Fit model (takes 20 minutes so beware)
# source("model-2pp/model-2pp.R")

document("pkg")
check("pkg")
