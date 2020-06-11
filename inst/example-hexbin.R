# from https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html

# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("DATA/us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Show it
plot(spdf)

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

# HH: choropleth map

# Load mariage data
data <- read.table("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/State_mariage_rate.csv", header=T, sep=",", na.strings="---")

# Distribution of the marriage rate?
data %>% 
  ggplot( aes(x=y_2015)) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') + 
  scale_x_continuous(breaks = seq(1,30))

# Merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , data, by=c("id"="state")) 

# Make a first chloropleth map
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =  y_2015, x = long, y = lat, group = group)) +
  scale_fill_gradient(trans = "log") +
  theme_void() +
  coord_map()

## viridis map:

spdf_fortified$bin <- cut( spdf_fortified$y_2015 , breaks=c(seq(5,10), Inf), labels=c("5-6", "6-7", "7-8", "8-9", "9-10", "10+" ), include.lowest = TRUE )

# Prepare a color scale coming from the viridis color palette
library(viridis)
my_palette <- rev(magma(8))[c(-1,-8)]

# plot
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette, 
    name="Wedding per 1000 people in 2015", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
  ggtitle( "A map of marriage rates, state by state" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
