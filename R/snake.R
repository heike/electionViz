#' Create an Electoral Snake Plot
#' 
#' @param data data frame
#' @param order variable to use to order the snake segments
#' @param label variable to use to label the segments (also used as a grouping variable)
#' @param fill variable to use to fill the segments in
#' @param color variable to use to outline the segments
#' @param size variable to use to determine how many segments are allocated to a group 
#' @import dplyr
#' @importFrom purrr map
#' @importFrom sf st_union st_cast
#' @importFrom tidyr nest unnest
#' @importFrom rlang ensym `!!` .data
#' @importFrom ggplot2 ggplot aes geom_sf geom_sf_text theme_void
#' @export
#' @examples
#' 
#' library(dplyr)
#' electoral_votes_2016 %>%
#' mutate(diff = perc_rep - perc_dem, party = ifelse(perc_dem > perc_rep, "Dem", "Rep"),
#'        label = clean_districts(state_district, electoral_votes)) %>%
#' ggsnake(order = diff, fill = diff, label = label, color = party, size = electoral_votes) + 
#' scale_fill_party_binned() + scale_color_party()
ggsnake <- function(data, order, label, fill, color, size) {
  # browser()
  
  # ensym variables
  f.order <- rlang::ensym(order)
  f.fill <- rlang::ensym(fill)
  f.color <- rlang::ensym(color)
  f.size <- rlang::ensym(size)
  f.label <- rlang::ensym(label)
  
  # data(snake_poly)
  get_segments <- function(idx) {
    tmp <- dplyr::filter(snake_poly, .data$ev %in% idx) %>%
      dplyr::summarize(geometry = sf::st_union(.data$geometry, by_feature = T) %>% sf::st_union() %>% sf::st_cast("MULTIPOLYGON"))
  }
  # browser()
  basic <- data %>%
    dplyr::arrange(!!f.order) %>%
    dplyr::mutate(cumev = cumsum(!!f.size),
                  allev = purrr::map2(.data$cumev, lag(.data$cumev, 1, 0), ~ (.y+1):.x)) %>%
    dplyr::mutate(geometry = purrr::map(.data$allev, get_segments)) %>%
    tidyr::unnest(geometry) %>%
    dplyr::select(-.data$allev)
  
  get_width_bbox <- function(x) {
    tmp <- x %>% sf::st_bbox() 
    tmp$xmax - tmp$xmin
  }
  
  labels <- basic %>%
    dplyr::group_by(!!f.label, !!f.size) %>%
    dplyr::summarize(center = purrr::map(.data$geometry, sf::st_point_on_surface)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(center = sf::st_as_sfc(.data$center)) %>%
    dplyr::mutate(ev_intersect = sf::st_intersects(.data$center, snake_poly$geometry) %>% as.numeric()) %>%
    dplyr::left_join(snake_poly_path, by = c("ev_intersect" = "ev")) %>%
    dplyr::mutate(angle = ifelse(!!f.size == 1, 0, angle))

  geometry <- center <- width <- angle <- NULL # CRAN check fixes
  
  ggplot2::ggplot(basic, ggplot2::aes(geometry = geometry, group = !!f.label)) +
    ggplot2::geom_sf(ggplot2::aes(geometry = geometry, color = !!f.color, fill = !!f.fill)) +
    ggplot2::geom_sf_text(data = labels, ggplot2::aes(geometry = center, label = !!f.label, angle = angle), inherit.aes = F) +
    ggplot2::theme_void() + 
    ggplot2::theme(legend.position = c(.525, .45))
}


#' Clean up district labels
#' 
#' Replace state and district names with abbreviations (for districts with fewer 
#' EVs than letters)
#' @param label text labels
#' @param ev electoral votes
#' @export
clean_districts <- function(label, ev) {
  . <- NULL
  
  tibble::tibble(
    orig = label, size = ev, nchar = nchar(label)
  ) %>%
    dplyr::mutate(abbreviate = .data$nchar > .data$size) %>%
    dplyr::mutate(state = gsub(" CD-\\d", "", .data$orig) %>% gsub("District of Columbia", "DC", .),
                  district = gsub("\\D{1,}", "", .data$orig)) %>%
    dplyr::left_join(tibble::tibble(state = datasets::state.name, abb = datasets::state.abb)) %>%
    dplyr::mutate(abb = ifelse(is.na(.data$abb), .data$state, .data$abb), 
                  label = ifelse(.data$abbreviate, paste(.data$abb, .data$district, sep = "-"), .data$state) %>%
                    gsub("-$", "", .)) %>%
    `[[`("label")
}