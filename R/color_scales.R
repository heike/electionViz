dem_blue <- "#0015BC" # Dem's blue
rep_red <- "#E9141D" # Rep's red

#' Color scale for two parties
#' 
#' @param ... passed on to scale_color_manual
#' @importFrom ggplot2 scale_color_manual
#' @export
scale_color_party <- function(...) {
  scale_color_manual(values = c(dem_blue, rep_red), ...)
}

#' Color scale for two parties
#' 
#' @param ... passed on to scale_colour_manual
#' @importFrom ggplot2 scale_colour_manual
#' @export
scale_colour_party <- function(...) {
  scale_colour_manual(values = c(dem_blue, rep_red), ...)
}
#' Fill scale for two parties
#' 
#' @param ... passed on to scale_fill_manual
#' @importFrom ggplot2 scale_fill_manual
#' @export
scale_fill_party <- function(...) {
  scale_fill_manual(values = c(dem_blue, rep_red), ...)
}


#' Color scale for gradient between two parties
#' 
#' @param ... passed on to scale_*_gradient2
#' @importFrom ggplot2 scale_color_gradient2
#' @export
scale_color_partygrad <- function(...) {
  scale_color_gradient2(low = dem_blue, mid = "white", high = rep_red, midpoint = 0)
}

#' Color scale for gradient between two parties
#' 
#' @param ... passed on to scale_*_gradient2
#' @importFrom ggplot2 scale_color_gradient2
#' @export
scale_colour_partygrad <- scale_color_partygrad

#' Fill scale for gradient between two parties
#' 
#' @inheritParams scale_color_partygrad
#' @importFrom ggplot2 scale_fill_gradient2
#' @export
scale_fill_partygrad <- function(...) {
  scale_fill_gradient2(low = dem_blue, mid = "white", high = rep_red, midpoint = 0)
}

#' Fill scale for binned gradient between two parties
#' 
#' @inheritParams scale_color_partygrad
#' @importFrom ggplot2 scale_fill_steps2
#' @export
#' @examples
#' 
#' library(dplyr)
#' electoral_votes_2016 %>%
#' mutate(diff = perc_rep - perc_dem, party = ifelse(perc_dem > perc_rep, "Dem", "Rep")) %>%
#' ggsnake(order = diff, fill = diff, label = state_district, color = party, size = electoral_votes) + 
#' scale_fill_party_binned() + scale_color_party()
scale_fill_party_binned <- function(...) {  
  scale_fill_steps2(low = dem_blue, high = rep_red, mid = "white", midpoint = 0,
                    breaks = c(-10, -5, -2, 2, 5, 10),
                    labels = c("Safe Dem", "Moderate Dem", "Lean Dem", "Lean Rep", "Moderate Rep", "Safe Rep"), trans = "pseudo_log")
}

#' Color scale for binned gradient between two parties
#' 
#' @inheritParams scale_color_partygrad
#' @importFrom ggplot2 scale_color_steps2
#' @export
scale_colour_party_binned <- function(...) {  
  scale_color_steps2(low = dem_blue, high = rep_red, mid = "white", midpoint = 0,
                     breaks = c(-10, -5, -2, 2, 5, 10),
                     labels = c("Safe Dem", "Moderate Dem", "Lean Dem", "Lean Rep", "Moderate Rep", "Safe Rep"), trans = "pseudo_log")
}

#' Color scale for binned gradient between two parties
#' 
#' @inheritParams scale_color_partygrad
#' @importFrom ggplot2 scale_color_continuous
#' @export
scale_color_party_binned <- scale_colour_party_binned
