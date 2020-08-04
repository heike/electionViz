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
#' @param ... passed on to scale_color_manual
#' @importFrom ggplot2 scale_color_manual
#' @export
scale_color_partygrad <- function(...) {
  scale_color_gradient2(low = dem_blue, mid = "white", high = rep_red, midpoint = 0)
}
#' Fill scale for gradient between two parties
#' 
#' @param ... passed on to scale_color_manual
#' @importFrom ggplot2 scale_color_manual
#' @export
scale_fill_partygrad <- function(...) {
  scale_fill_gradient2(low = dem_blue, mid = "white", high = rep_red, midpoint = 0)
}

#' Update to the newest polls provided by FiveThirtyEight
#' 
#' @param polls character string describing the race, one of 'president_polls', 'senate_polls', 'house_polls' or 'governor_polls'
#' @return tibble of the recent polls
#' @importFrom readr read_csv
#' @importFrom lubridate mdy
#' @export
#' @examples 
#' presidential_polls <- fivethirtyeight_update()
fivethirtyeight_update <- function(polls = "president_polls") {
  url <- sprintf("https://projects.fivethirtyeight.com/polls-page/%s.csv", polls)
  start_date <- end_date <- election_date <- ""
  
  res <- readr::read_csv(url)
  res %>% mutate(
    start_date = lubridate::mdy(start_date),
    end_date = lubridate::mdy(end_date),
    election_date = lubridate::mdy(election_date)
  )
}

