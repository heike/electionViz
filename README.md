
<!-- README.md is generated from README.Rmd. Please edit that file -->

# electionViz

<!-- badges: start -->

<!-- badges: end -->

R package for visualizations of election (or poll) results as easy as
adding `geom_electoral_building`.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("heike/electionViz")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(electionViz)
#> Loading required package: ggplot2
#> Loading required package: tilegramsR
#> Loading required package: sf
#> Warning: package 'sf' was built under R version 3.6.2
#> Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0
# county level results from 2016 presidential elections
el2016 <- read.csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv") 

## basic example code
```
