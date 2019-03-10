
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
#' @importFrom dplyr filter left_join %>% mutate
#' @importFrom grid gpar grid.text grid.newpage grid.rect grid.segments
#' @param year Election year - should be one of 2016, 2013 or 2010
#' @param fontfamily Font family to use
#' @details This should be printed to an 8 x 6.5 inch output. Anything else might be a bit wrong... Basically,
#' this is a bit of a hack for a very specific single purpose. 
#' 
#' As a side effect, this function draws a chart that will look good on an 8 x6.5 inch output.
ozpol_infographic <- function(year, fontfamily = "Sans"){
  
  m <- list()
  
  m[[1]] <- ced_boundaries %>%
    dplyr::filter(election_year == year) %>%
    dplyr::left_join(dplyr::filter(results_2pp_div, election_year == year), by = c("ced_name" = "division_nm")) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = lib_nat_percentage / 100), colour = NA) +
    theme_ozpol_map(base_family = fontfamily) +
    ggplot2::scale_fill_gradient2(low = "#e53440", mid = "white", high = "#1c4f9c", midpoint = 0.5, 
                         label = scales::percent_format(accuracy = 1))
  
  m[[2]] <- m[[1]] + ggplot2::coord_sf(xlim =c(144.2, 145.7), ylim = c(-37.55, -38.4)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey20"))
  m[[3]] <- m[[1]] + ggplot2::coord_sf(xlim =c(150.6, 151.4), ylim = c(-33.6, -34.2)) 
  m[[4]] <- m[[1]] + ggplot2::coord_sf(xlim =c(152, 154), ylim = c(-26.6, -28.4)) 
  m[[5]] <- m[[1]] + ggplot2::coord_sf(xlim =c(138.4, 138.8), ylim = c(-35.4, -34.7))
  m[[6]] <- m[[1]] + ggplot2::coord_sf(xlim =c(115.4, 116.3), ylim = c(-32.4, -31.7))
  
  
  dy <- 0.04
  dx <- 0.04
  centre_points <- data.frame(
    city =  c("Australia", "Melbourne", "Sydney", "Brisbane", "Adelaide", "Perth"),
    x =     c(0.4,          0.65,        0.745,       0.719,        0.4,       0.11),
    y =     c(0.65,         0.275,       0.56,       0.85,       0.275,      0.6),
    width = c(0.63,         0.3,        0.28,       0.23,        0.12,       0.25),
    xend =  c(NA,          0.495,        0.545,        0.56,        0.45,       0.28),
    yend =  c(NA,          0.535,        0.58,        0.64,        0.575,       0.59)
  ) %>%
    dplyr::mutate(y = y - dy,
           yend = yend - dy,
           x = x + dx,
           xend = xend + dx)
  
  
  grid::grid.newpage()
  grid::grid.rect(0.5, 0.5, width = 1, height = 1, gp = gpar(fill = "grey20"))
  grid::grid.segments(x0 = centre_points$x,
                      x1 = centre_points$xend,
                      y0 = centre_points$y,
                      y1 = centre_points$yend,
                      gp = grid::gpar(col = "white"))
  for(i in 1:nrow(centre_points)){
    print(m[[i]], vp = grid::viewport(x = centre_points[i, "x"], 
                                      y = centre_points[i, "y"],
                                      width = centre_points[i, "width"]))
  }
  # grid.circle(x = centre_points$xend, y = centre_points$yend, r = 0.02, 
  #             gp = gpar(col = "white", fill = "transparent"))
  grid::grid.text(0.03, 0.95, 
                  label = "Two-party-preferred vote in the\n2016 federal election", 
                  hjust = 0, vjust = 1,
            gp = grid::gpar(fontfamily = fontfamily, cex = 1.5, col = "white", fontface = "bold"))

}