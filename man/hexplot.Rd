% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hexplot.R
\name{hexplot}
\alias{hexplot}
\title{Hexagonal cartogram of the US by state}
\usage{
hexplot(states, fill, label = TRUE, labelcolor = "grey90")
}
\arguments{
\item{states}{vector of state names}

\item{fill}{vector mapping to fill color, same order as states}

\item{label}{logical. Should labels be shown?}

\item{labelcolor}{colour in which labels are shown. Ignored if `label=FALSE`.}
}
\value{
ggplot2 object
}
\description{
Add fill colour to a hexgonal cartogram of the US
}
\examples{
data(elections)
el12 <- elections \%>\% dplyr::filter(year == 2012)
hexplot(el12$state, el12$perc_rep > el12$perc_dem) +
  scale_fill_party("Election 2012", labels=c("Democrat", "Republican"))
}
