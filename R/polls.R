search_for_parent <- function(node, tag) {
  parents <- xml2::xml_parents(node)
  tags <- purrr::map_chr(parents, rvest::html_name)
  matches <- min(which(tags == tag))
  if (length(matches) == 1) return(parents[matches]) else return(NULL)
}


rcp_race_info <- function(nodelist) {
  txt <- rvest::html_text(x = nodelist, trim = T)
  tibble::tibble(
    idx = 1:length(txt),
    rcp_state_poll_link = rvest::html_attr(nodelist, "href"),
    state = stringr::str_extract(txt, "^([A-z ]{1,})"),
    race = stringr::str_extract(txt, "([A-z \\.]{1,})$") %>% stringr::str_trim()
  )
}

rcp_poll_info <- function(nodelist) {
  tibble::tibble(
    idx = 1:length(nodelist),
    poll_link = rvest::html_attr(nodelist, "href"),
    pollster = rvest::html_text(x = nodelist, trim = T)
  )
}

rcp_results_info <- function(nodelist) {
  c1 <- c1score <- c2 <- c2score <- candidate <- idx <- pct <- . <- NULL
  
  tmp <- rvest::html_text(x = nodelist, trim = T)
  tibble::tibble(tmp = tmp, idx = 1:length(tmp)) %>%
    tidyr::extract(tmp, into = c("c1", "c1score", "c2", "c2score"), "([A-z]{1,}) (\\d{1,3}), ([A-z]{1,}) (\\d{1,3})") %>%
    tidyr::pivot_wider(names_from = c(c1, c2), values_from = c(c1score, c2score)) %>%
    magrittr::set_names(stringr::str_replace_all(names(.), c("c1score_([A-z]{1,})_([A-z]{1,})"="c1_\\1",
                                                             "c2score_([A-z]{1,})_([A-z]{1,})"="c2_\\2"))) %>%
    tidyr::pivot_longer(-1, names_to = "candidate", values_to = "pct") %>%
    dplyr::filter(!is.na(pct)) %>%
    dplyr::mutate(candidate = stringr::str_remove(candidate, "c\\d_")) %>%
    # tidyr::pivot_wider(names_from = candidate, values_from = pct) %>%
    tidyr::nest(res = -idx)
}

get_rcp_poll_info <- function(node) {
  races <- rvest::xml_nodes(node, xpath = "tr/td[@class='lp-race']/a") %>% rcp_race_info()
  polls <- rvest::xml_nodes(node, xpath="tr/td[@class='lp-poll']/a") %>% rcp_poll_info()
  results <- rvest::xml_nodes(node, xpath="tr/td[@class='lp-results']/a") %>% rcp_results_info()
  # spread <- rvest::xml_nodes(node, xpath="tr/td[@class='lp-spread']") 
  dplyr::left_join(races, polls, by = "idx") %>%
    dplyr::left_join(results, by = "idx")
}

#' This function returns the latest polls from Real Clear Politics
#' 
#' @param type one of "national_president", "state_president", 
#'             "state_of_the_union". Deafults to "state_president"; 
#'             senate, governor tables are not optimally tidy. 
#' @return a table of poll links, with results for each candidate
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_node xml_nodes
#' @importFrom purrr map map_chr
#' @importFrom stringr str_replace
#' @importFrom lubridate dmy
#' @importFrom dplyr mutate select
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @examples 
#' rcp_update()
#' rcp_update("national_president")
rcp_update <- function(type = "state_president") {
  table_node <- data <- NULL
  
  stopifnot(type %in% c("national_president", "state_president", "senate", "governor", "state_of_the_union"))
  url <- paste0("https://www.realclearpolitics.com/epolls/latest_polls/", type, "/")
  page <- xml2::read_html(url) 

  table_date_nodes <- page %>%
    rvest::xml_nodes(".table-races .date")
  table_date_nodes <- purrr::map(table_date_nodes, ~.)
  table_dates <- purrr::map_chr(table_date_nodes, rvest::html_text) %>%
    stringr::str_replace("^([A-z]{1,}), ([A-z]{1,}) (\\d{1,})", "\\3 \\2 2020") %>%
    lubridate::dmy()
  
  table_dates_parents <- purrr::map(table_date_nodes, search_for_parent, tag = "table")
  table_dates_nextsib <- table_dates_parents %>%
    purrr::map(rvest::html_node, xpath = "following-sibling::*[1]")
  
  info <- tibble::tibble(date = table_dates, table_node = table_dates_nextsib) %>%
    dplyr::mutate(data = purrr::map(table_node, get_rcp_poll_info)) %>%
    dplyr::select(-table_node) %>%
    tidyr::unnest(data) 
  info
}

