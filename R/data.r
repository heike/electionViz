#' Results of the US Presidential elections by state 1976-2016
#' 
#' A dataset containing results of all US Presidential elections by state from 1976 to 2016.
#' This data was provided by the MIT Election Data + Science Lab. 
#' @format An object of class data.frame with 3740 rows and 14 columns.
#' \describe{
#' \item{year}{Year of the election between 1976 and 2016}
#' \item{state}{Name of the US state}
#' \item{state_po}{U.S. postal code state abbreviation}
#' \item{state_fips}{State FIPS code}
#' \item{state_cen}{U.S. Census state code}
#' \item{state_ic}{ICPSR state code}
#' \item{office}{U.S. President}
#' \item{candidate}{Name of the candidate}
#' \item{party}{Party of the candidate}
#' \item{writein}{Vote totals associated with write-in candidates}
#' \item{candidatevotes}{Votes received by this candidate for this particular party}
#' \item{totalvotes}{Total number of votes cast for this election}
#' \item{version}{Date of final version}
#' \item{notes}{logical vector}
#' }
#' @source \url{https://doi.org/10.7910/DVN/42MVDX}
#' @references MIT Election Data and Science Lab, 2017, "U.S. President 1976–2016", \url{https://doi.org/10.7910/DVN/42MVDX}, Harvard Dataverse, V5.
"USelections"


#' Hexbin map of the United States
#' 
#' Hexbin map of the United states modified to include hexagon centers and state abbreviations.
#' The original hexbin map is provided by Andrew X Hill at \url{https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map}.
#' @format An object of class data.frame with 357 rows and 8 columns.
#' \describe{
#' \item{long}{geographic longitude}
#' \item{lat}{geographic latitude}
#' \item{order}{order in which nodes are connected}
#' \item{group}{group}
#' \item{state}{name of the state}
#' \item{abbr}{US postal code state abbreviations}
#' \item{centerX}{center of the hexagon in longitude}
#' \item{centerY}{center of the hexagon in latitude}
#' }
#' @source \url{https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map}
#' @references Andrew X Hill
"ushex"


#' Hexbin map of the United States by population
#' 
#' Hexbin map of the United states. Each hexagon represents a population of about 500k. This map is based on the  `sf_Pitch_US_Population_2016_v1` map in the `tilegramsR` package by Bhaskar Karambelkar.
#' @format An object of class data.frame with 4515 rows and 8 columns.
#' \describe{
#' \item{long}{geographic longitude}
#' \item{lat}{geographic latitude}
#' \item{order}{order in which nodes are connected}
#' \item{group}{group}
#' \item{state}{name of the state (abbreviated)}
#' \item{FID}{state fips codes}
#' \item{centerX}{center of the hexagon in longitude}
#' \item{centerY}{center of the hexagon in latitude}
#' }
"pop_hex"


#' Hexbin map of the United States by electoral votes
#' 
#' Hexbin map of the United states. Each hexagon represents a population of about 500k. This map is based on the  `sf_FiveThirtyEightElectoralCollege` map in the `tilegramsR` package by Bhaskar Karambelkar.
#' @format An object of class data.frame with 3766 rows and 8 columns.
#' \describe{
#' \item{long}{geographic longitude}
#' \item{lat}{geographic latitude}
#' \item{order}{order in which nodes are connected}
#' \item{group}{group}
#' \item{state}{name of the state (abbreviated)}
#' \item{FID}{state fips codes}
#' \item{centerX}{center of the hexagon in longitude}
#' \item{centerY}{center of the hexagon in latitude}
#' }
"electoral_hex"

#' Hexbin map of the United States by electoral votes
#' 
#' Hexbin map of the United states. Each hexagon represents a population of about 500k. This map is based on the  `sf_FiveThirtyEightElectoralCollege` map in the `tilegramsR` package by Bhaskar Karambelkar.
#' @format An object of class data.frame with 3766 rows and 8 columns.
#' \describe{
#' \item{long}{geographic longitude}
#' \item{lat}{geographic latitude}
#' \item{order}{order in which nodes are connected}
#' \item{group}{group}
#' \item{state}{name of the state (abbreviated)}
#' \item{FID}{state fips codes}
#' \item{centerX}{center of the hexagon in longitude}
#' \item{centerY}{center of the hexagon in latitude}
#' }
"electoral_state_outline_hex"


#' Electoral votes corresponding to U.S. states and Maine, Nebraska congressional districts
#' 
#' Electoral votes corresponding to U.S. states and congressional districts for Maine and Nebraska. Data from the UC-Santa Barbara American Presidency Project
#' @format An object of class data.frame with 56 rows and 4 columns.
#' \describe{
#' \item{state_district}{state name or congressional district}
#' \item{electoral_votes}{number of electoral votes associated with state or district}
#' \item{perc_dem}{percentage of votes earned by democratic candidate in 2016}
#' \item{perc_rep}{percentage of votes earned by republican candidate in 2016}
#' }
#' @source \url{https://www.presidency.ucsb.edu/statistics/elections/2016}
"electoral_votes_2016"

#' Polls for the presidential general election
#' 
#' Scraped from FiveThirtyEight's website of most recent polls on July 28 2020.
#' @format A tibble with 5312 rows and 37 columns
#' \describe{
#'   \item{question_id}{numeric}
#'   \item{poll_id}{numeric}
#'   \item{cycle}{numeric}
#'   \item{state}{character}
#'   \item{pollster_id}{numeric}
#'   \item{pollster}{character}
#'   \item{sponsor_ids}{numeric}
#'   \item{sponsors}{character}
#'   \item{display_name}{character}
#'   \item{pollster_rating_id}{numeric}
#'   \item{pollster_rating_name}{character}
#'   \item{fte_grade}{character}
#'   \item{sample_size}{numeric}
#'   \item{population}{character}
#'   \item{population_full}{character}
#'   \item{methodology}{character}
#'   \item{office_type}{character}
#'   \item{seat_number}{numeric}
#'   \item{seat_name}{logical}
#'   \item{start_date}{Date}
#'   \item{end_date}{Date}
#'   \item{election_date}{Date}
#'   \item{sponsor_candidate}{character}
#'   \item{internal}{logical}
#'   \item{partisan}{character}
#'   \item{tracking}{logical}
#'   \item{nationwide_batch}{logical}
#'   \item{ranked_choice_reallocated}{logical}
#'   \item{created_at}{character}
#'   \item{notes}{character}
#'   \item{url}{character}
#'   \item{stage}{character}
#'   \item{race_id}{numeric}
#'   \item{answer}{character}
#'   \item{candidate_name}{character}
#'   \item{candidate_party}{character}
#'   \item{pct}{numeric}
#' }
#' @source \url{https://projects.fivethirtyeight.com/polls-page/president_polls.csv}
#' @examples 
#' library(ggplot2)
#' library(dplyr)
#' 
#' data(presidential_polls)
#' presidential_polls %>% 
#'   filter(candidate_party %in% c("DEM", "REP")) %>% 
#'   filter(!is.na(state)) %>%
#'   ggplot(aes(x = end_date, y = pct)) + 
#'     geom_point(aes(colour = candidate_party)) +
#'     facet_wrap(~state) +
#'     scale_colour_party()
#'
#' # national polls
#' presidential_polls %>% 
#'   filter(candidate_party %in% c("DEM", "REP")) %>% 
#'   filter(answer %in% c("Biden", "Trump")) %>% 
#'   filter(is.na(state)) %>%
#'   ggplot(aes(x = end_date, y = pct, 
#'              colour = candidate_party)) + 
#'     geom_point(alpha = 0.3) +
#'     geom_smooth(se = FALSE, method="gam") +
#'     scale_colour_party() +
#'     theme(legend.position="bottom")
"presidential_polls"

#' State-level election results of US presidential elections
#' 
#' Wide format of the `USelection` data, watered down to democratic and republican votes.
#' @format A tibble with 564 rows and 10 columns
#' \describe{
#'   \item{year}{year of election}
#'   \item{state}{name of the state}
#'   \item{state_po}{two-letter abbreviation of state}
#'   \item{votes_dem}{number of votes for the Democratic candidate}
#'   \item{votes_rep}{number of votes for the Republican candidate}
#'   \item{perc_dem}{percent of votes for the Democratic candidate}
#'   \item{perc_rep}{percent of votes for the Republican candidate}
#'   \item{totalvotes}{number of total votes}
#'   \item{cand_dem}{name of the Democratic candidate}
#'   \item{cand_rep}{name of the Republican candidate}
#' }
"elections"


#' Electoral snake path data
#' 
#' A winding path of 538 approximately equal segments to be used for making
#' the electoral snake plot.
#' @format A tibble with 3278 rows and 4 columns
#' \describe{
#'   \item{ev}{electoral vote number}
#'   \item{order}{path plotting order}
#'   \item{x}{x coordinate}
#'   \item{y}{y coordinate}
#' }
"snake_path"