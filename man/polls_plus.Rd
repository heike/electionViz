% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_combine.R
\name{polls_plus}
\alias{polls_plus}
\title{Average polls and supplement with election data}
\usage{
polls_plus(polls, election)
}
\arguments{
\item{polls}{data frame of polls as returned by `polls_filter`}

\item{election}{tibble of election results (with number of electoral votes)}
}
\value{
tibble of average polls supplemented by last k polls
polls_plus(presidential_polls %>% filter(population %in% c("lv", "rv")),
           electoral_votes_2016) %>% head()
}
\description{
Find a state/district average of polls and supplement averages with election data in case no recent polls are available for a state or district.
}
