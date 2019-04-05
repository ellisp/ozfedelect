library(frs)
library(tidyverse)
library(scales)
library(rstan)
library(rvest)
library(pscl)
library(scales)
library(extrafont)
#library(Cairo)
library(svglite)
library(devtools)
library(knitr)
library(snakecase)
library(testthat)
library(ozfedelect)
library(RColorBrewer)

run_all_r_scripts("R", cleanup = FALSE)


myfont <- "Roboto"
main_font <- "Roboto"
heading_font <- "Sarala"


theme_set(theme_light(base_family = main_font) + 
            theme(legend.position = "bottom") +
            theme(plot.caption = element_text(colour = "grey50"),
                  strip.text = element_text(size = rel(1), face = "bold"),
                  plot.title = element_text(family = heading_font),
                  plot.subtitle = element_text(family = heading_font, colour = "grey50"))
) 
update_geom_defaults("text", list(family = main_font))
