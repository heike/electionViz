
<!-- README.md is generated from README.Rmd. Please edit that file -->

# electionViz

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/heike/electionViz.svg?branch=master)](https://travis-ci.org/heike/electionViz)
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

We will be inundated with information about the state of the polls in
the build-up to the upcoming Presidential election of 2020.

With the tools of this package you will be able to pick your favorite
visualization(s) and explore the results your own way.

``` r
# load the package
library(electionViz)
library(tidyverse)
```

### Hexbin Cartogram of Election Results by State

Each state is represented by one hexagon. This map has been made
available by Andrew X Hill at
[CARTO](https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map).

``` r
data(elections)
el12 <- elections %>% filter(year == 2012)
gg12 <- hexplot(el12$state, el12$perc_rep > el12$perc_dem) +
  scale_fill_party("", labels=c("Democrat", "Republican")) +
  theme(legend.position = "bottom") +
  ggtitle("Election 2012")

el16 <- elections %>% filter(year == 2016)
gg16 <- hexplot(el16$state, el16$perc_rep > el16$perc_dem) +
  scale_fill_party("", labels=c("Democrat", "Republican")) +
  theme(legend.position = "bottom") +
  ggtitle("Election 2016")

gridExtra::grid.arrange(gg12, gg16, ncol=2)
```

<img src="man/figures/README-example-1.png" width="100%" />

### Hexbin Cartogram of the US Presidential Election by Electoral Votes

Each state is represented by a set of hexagons corresponding in number
to the state’s electoral votes. This map was adapted from the object
`sf_FiveThirtyEightElectoralCollege` in Bhaskar Karambelkar’s R package
[tilegramsR](https://github.com/bhaskarvk/tilegramsR).

<img src="man/figures/README-electoral-1.png" width="100%" />

### Electoral Building of the U.S. Presidential Election by Electoral Votes and Margin of Victory

“Electoral Building” diagram, inspired by a 2000 graphic from the New
York Times. Vote margin is represented on the x axis, while number of
electoral votes is represented in the height.

<img src="man/figures/README-building-1.png" width="100%" />
