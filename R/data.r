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
#' @format An object of class data.frame with 357 rows and 5 columns.
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


#' Hexbin map of the United States
#' 
#' Hexbin map of the United states. Each hexagon represents a population of about 500k. This map is based on the  `sf_Pitch_US_Population_2016_v1` map in the `tilegramsR` package by Bhaskar Karambelkar.
#' @format An object of class data.frame with 357 rows and 5 columns.
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
