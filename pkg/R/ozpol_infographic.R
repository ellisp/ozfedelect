
#' Theme for a map of federal divisions
#' 
#' @import ggplot2
#' @param ... other parameters to pass to \code{theme_bw}
#' @export
theme_ozpol_map <- function(...){
  ggplot2::theme_bw(...) +
    ggplot2::theme(text            = ggplot2::element_text(colour = "white"),
                  panel.grid       = ggplot2::element_line(colour = 'transparent'),
                  panel.background = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
                  plot.background  = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
                  panel.border     = ggplot2::element_blank(),
                  axis.text        = ggplot2::element_blank(),
                  axis.ticks       = ggplot2::element_blank(),
                  legend.position  = "none")
}

#' Draw an infographic map of Australian election results
#' 
#' 
#' @export
#' @importFrom scales percent_format
#' @importFrom dplyr filter left_join %>% mutate pull
#' @importFrom grid gpar grid.text grid.newpage grid.rect grid.segments grid.draw
#' @importFrom cowplot get_legend
#' @param year Election year - should be one of 2016, 2013 or 2010
#' @param fontfamily Font family to use
#' @param variable which variable to use
#' @details This should be printed to an 8 x 6.5 inch output. Anything else might be a bit wrong... Basically,
#' this is a bit of a hack for a very specific single purpose. 
#' 
#' As a side effect, this function draws a chart that will look good on an 8 x6.5 inch output.
ozpol_infographic <- function(year, fontfamily = "sans", variable = c("lib_nat_percentage", "swing_to_govt")){
  
  variable = match.arg(variable)
  midpoint <- ifelse(variable == "lib_nat_percentage", 0.5, 0)
  
  d1 <- filter(ced_boundaries, election_year == year)
  d2 <- filter(results_2pp_div, election_year == year)
  d2$fill_variable <- pull(d2, variable)
  if(unique(d2$incumbent) == "ALP" & variable == "swing_to_govt"){
    d2$fill_variable <- -d2$fill_variable
  }
  d3 <- dplyr::left_join(d1, d2, by = c("ced_name" = "division_nm"))
    
  m <- list()
  
  m[[1]] <- d3 %>%
    ggplot2::ggplot(ggplot2::aes(fill = fill_variable / 100))  +
    ggplot2::scale_fill_gradient2(low = "#e53440", mid = "white", high = "#1c4f9c", midpoint = midpoint, 
                                  label = scales::percent_format(accuracy = 1)) +
    ggplot2::geom_sf(colour = NA) +
    theme_ozpol_map(base_family = fontfamily) 
      
  m[[2]] <- m[[1]] + ggplot2::coord_sf(xlim =c(144.2, 145.7), ylim = c(-37.55, -38.4)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey20"))
  m[[3]] <- m[[1]] + ggplot2::coord_sf(xlim =c(150.6, 151.4), ylim = c(-33.6, -34.2)) 
  m[[4]] <- m[[1]] + ggplot2::coord_sf(xlim =c(152, 154), ylim = c(-26.6, -28.4)) 
  m[[5]] <- m[[1]] + ggplot2::coord_sf(xlim =c(138.4, 138.8), ylim = c(-35.4, -34.7))
  m[[6]] <- m[[1]] + ggplot2::coord_sf(xlim =c(115.4, 116.3), ylim = c(-32.4, -31.7))
  
  # how to draw just the legend of a ggplot2 graphic
  leg_map <- m[[1]] + 
    labs(fill = "") +
    theme(legend.position = c(0.2, 0.25),
          legend.background = element_rect(fill = "grey20"))
  
  legend <- cowplot::get_legend(leg_map)
  
  
  dy <- 0.04
  dx <- 0.04
  centre_points <- data.frame(
    city =  c("Australia", "Melbourne", "Sydney", "Brisbane", "Adelaide", "Perth"),
    x =     c(0.39,          0.7,        0.795,       0.769,        0.45,       0.10),
    y =     c(0.65,         0.256,       0.557,       0.869,       0.257,      0.6),
    width = c(0.63,         0.31,        0.29,       0.24,        0.12,       0.26),
    xend =  c(NA,          0.543,        0.595,        0.61,        0.485,       0.29),
    yend =  c(NA,          0.517,        0.557,        0.635,        0.554,       0.58)
  ) %>%
    dplyr::mutate(y = .$y - dy,
           yend = .$yend - dy,
           x = .$x + dx,
           xend = .$xend + dx)
  
  
  grid::grid.newpage()
  grid::grid.rect(0.5, 0.5, width = 1, height = 1, gp = gpar(fill = "grey20"))
  grid::grid.segments(x0 = centre_points$x,
                      x1 = centre_points$xend,
                      y0 = centre_points$y,
                      y1 = centre_points$yend,
                      gp = grid::gpar(col = "grey80"))
  for(i in 1:nrow(centre_points)){
    print(m[[i]], vp = grid::viewport(x = centre_points[i, "x"], 
                                      y = centre_points[i, "y"],
                                      width = centre_points[i, "width"]))
  }
  # grid.circle(x = centre_points$xend, y = centre_points$yend, r = 0.02,
  #             gp = gpar(col = "white", fill = "transparent"))

  heading <- paste0("Two-party-preferred VOTE in the\n", year, " federal election")
  if(variable == "swing_to_govt"){
    heading <- gsub("VOTE", "SWING", heading)
  }
  
  grid::grid.text(0.03, 0.95, 
                  label = heading, 
                  hjust = 0, vjust = 1,
            gp = grid::gpar(fontfamily = fontfamily, cex = 1.5, col = "white", fontface = "bold"))
  
  grid.draw(legend) 
  

}
