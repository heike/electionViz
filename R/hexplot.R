#' Hexagonal cartogram of the US by state
#' 
#' Add fill colour to a hexgonal cartogram of the US
#' @param states vector of state names
#' @param fill vector mapping to fill color, same order as states
#' @param label logical. Should labels be shown?
#' @param labelcolor colour in which labels are shown. Ignored if `label=FALSE`.
#' @importFrom utils data
#' @return ggplot2 object
#' @export
#' @examples 
#' data(elections)
#' el12 <- elections %>% dplyr::filter(year == 2012)
#' hexplot(el12$state, el12$perc_rep > el12$perc_dem) +
#'   scale_fill_party("Election 2012", labels=c("Democrat", "Republican"))
hexplot <- function(states, fill, label =TRUE, labelcolor = "grey90") {
  ## labels should be separated from polygons
  ushex <- NULL
  data("ushex", package = "electionViz", envir=environment())
  
  # make R CMD CHECK happy
  abbr <- centerX <- centerY <- group <- lat <- long <- NULL
  
  df <- data.frame(state = states, fill = fill)
  # Merge geospatial and numerical information
  ushex_plus <- ushex %>% left_join(df, by="state")
  
  # Geographic centers for labels
  centers <- ushex %>% select(centerX, centerY, abbr) %>% unique()
  
  gg <- ushex_plus %>%
    ggplot() +
    geom_polygon(aes(fill =  fill, 
                     x = long, y = lat, group = group), 
                 colour = "grey90", size = 0.1, alpha = 0.9) +
    theme_void() +
    coord_map() 
  
  if (label)
    gg <- gg + geom_text(aes(x = centerX, y = centerY, label = abbr), 
                       colour = labelcolor, size = 3, data = centers) 
  
  gg
}




# Make a first chloropleth map
