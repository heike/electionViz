% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polls.R
\name{fivethirtyeight_update}
\alias{fivethirtyeight_update}
\title{Update to the newest polls provided by FiveThirtyEight}
\usage{
fivethirtyeight_update(polls = "president_polls")
}
\arguments{
\item{polls}{character string describing the race, one of 'president_polls', 'senate_polls', 'house_polls' or 'governor_polls'}
}
\value{
tibble of the recent polls
}
\description{
Update to the newest polls provided by FiveThirtyEight
}
\examples{
presidential_polls <- fivethirtyeight_update()
}
