#' Create an Electoral Snake Plot
#' 
#' @param .data data frame
#' @param pct_diff percent difference between republican and democrat votes/polls. Democrat advantage should be a negative number.
#' @param electoral_votes number of electoral votes in each district
#' @param district 
#' @export
ggsnake <- function(.data) {
  # browser()
  data("snake_poly", package = "electionViz")
  
  get_segments <- function(idx) {
    tmp <- filter(snake_poly, ev %in% idx) %>%
      summarize(data = sf::st_union(data) %>% sf::st_cast("MULTIPOLYGON"))
  }
  
  basic <- .data %>%
    dplyr::arrange(pct_diff) %>%
    dplyr::mutate(cumev = cumsum(electoral_votes),
                  allev = purrr::map2(cumev, lag(cumev, 1, 0), ~ (.y+1):.x)) %>%
    dplyr::mutate(data = purrr::map(allev, get_segments)) %>%
    tidyr::unnest(data) %>%
    dplyr::select(-allev) %>%
    dplyr::mutate(fill = ifelse(pct_diff > 0, "#E9141D", "#0015BC"))
  
  labels <- basic %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(center = purrr::map(data, sf::st_point_on_surface)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(center = sf::st_as_sfc(center))

  ggplot(basic, aes(geometry = data, group = district)) +
    geom_sf(aes(geometry = data, color = fill, fill = pct_diff)) +
    scale_color_identity() + scale_fill_partygrad() +
    geom_sf_text(data = labels, aes(geometry = center, label = state), inherit.aes = F) +
    theme_void()
}