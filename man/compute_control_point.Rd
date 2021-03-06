% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bead_snake.R
\name{compute_control_point}
\alias{compute_control_point}
\title{Helper function}
\usage{
compute_control_point(dfs, xwt = 0.5, ywt = 0.5, dir = 0, ...)
}
\arguments{
\item{dfs}{dataset}

\item{xwt}{some weight along x direction}

\item{ywt}{some weight along y direction}

\item{dir}{parameter controlling the direction}

\item{...}{passed on to inside functions}
}
\description{
This function computes the control point based on weights passed in - 
the control point is (xwt, ywt) in relative proportion between the two rows 
in the data frame
}
