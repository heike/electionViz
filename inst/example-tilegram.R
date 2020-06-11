# from https://rpubs.com/bhaskarvk/tilegramsR

suppressPackageStartupMessages(library(tilegramsR))
library(leaflet)
# devtools::install_github('bhaskarvk/leaflet.extras')
library(leaflet.extras)

getLeafletOptions <- function(minZoom, maxZoom, ...) {
  leafletOptions(
    crs = leafletCRS("L.CRS.Simple"),
    minZoom = minZoom, maxZoom = maxZoom,
    dragging = FALSE, zoomControl = FALSE,
    tap = FALSE,
    attributionControl = FALSE , ...)
}



getFactorPal <- function(f) {
  colorFactor(colormap::colormap(
    colormap = colormap::colormaps$hsv,
    nshades = length(f)), f)
}

leaflet(
  sf_NPR1to1,
  options= getLeafletOptions(-1.5, -1.5)) %>%
  addPolygons(
    weight=2,color='#000000', group = 'states',
    fillOpacity = 0.6, opacity = 1, fillColor= ~getFactorPal(state)(state),
    highlightOptions = highlightOptions(weight = 4)) %>%
  addLabelOnlyMarkers(
    data=sf_NPR1to1.centers,
    label = ~as.character(state),
    labelOptions = labelOptions(
      noHide = 'T', textOnly = T,
      offset=c(-4,-10), textsize = '12px')) %>%
  setMapWidgetStyle()


leaflet(
  options= getLeafletOptions(-1.5, -1.5)) %>%
  addPolygons(
    data=sf_Pitch_US_Population_2016_v1,
    weight=1,color='#000000', fillOpacity = 0.5, opacity=0.2,
    fillColor= ~getFactorPal(state)(state)) %>%
  addPolygons(
    data=sf_Pitch_US_Population_2016_v1.states, group = 'states',
    weight=2,color='#000000',
    fill = T, opacity = 1, fillOpacity = 0,
    highlightOptions = highlightOptions(weight = 4)) %>%
  addLabelOnlyMarkers(
    data=sf_Pitch_US_Population_2016_v1.centers,
    label = ~as.character(state),
    labelOptions = labelOptions(
      noHide = 'T', textOnly = T,
      offset=c(-4,-10), textsize = '15px')) %>%
  setMapWidgetStyle()
