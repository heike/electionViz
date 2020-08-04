
#' Update to the newest polls provided by FiveThirtyEight
#' 
#' @param polls character string describing the race, one of 'president_polls', 'senate_polls', 'house_polls' or 'governor_polls'
#' @return tibble of the recent polls
#' @importFrom readr read_csv
#' @importFrom lubridate mdy
#' @export
#' @examples 
#' presidential_polls <- fivethirtyeight_update()
fivethirtyeight_update <- function(polls = "president_polls") {
  url <- sprintf("https://projects.fivethirtyeight.com/polls-page/%s.csv", polls)
  start_date <- end_date <- election_date <- ""
  
  res <- readr::read_csv(url)
  res %>% mutate(
    start_date = lubridate::mdy(start_date),
    end_date = lubridate::mdy(end_date),
    election_date = lubridate::mdy(election_date)
  )
}



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
    rcp_poll_page_link = rvest::html_attr(nodelist, "href"),
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

rcp_detail_poll_info <- function(url_frag) {
  Date <- Spread <- Sample <- MoE <- Sample_n <- candidate <- pct <- NULL
  
  url <- paste0("https://www.realclearpolitics.com", url_frag)
  page <- xml2::read_html(url)
  
  # --- Clear out mobile links ---
  mobilelink <- rvest::xml_nodes(page, ".normal_pollster_name") %>%
    purrr::walk(xml2::xml_remove)
  table_node <- page %>%
    rvest::xml_nodes(xpath = "(//table[contains(@class, 'data')])")
  table_node <- table_node[[length(table_node)]]
  
  # --- Extra fun to get poll link for merge
  get_poll_link <- function(x) {
    tmp <- xml2::xml_find_all(x, "*/td[1]") %>% 
      purrr::map(~try(xml2::xml_child(.), silent = T)) %>% 
      purrr::map_chr(~try(rvest::html_attr(., "href"), silent = T))
    
    tmp[grepl("Error ", tmp)] <- NA
    tmp
  }
  
  # --- define cols not to pivot (easier than handling candidate names, etc.)
  nopivot <- c("id", "Poll", "Date_Start", "Date_End", "Sample_n", "Sample_type", "MoE")
  as.numeric.nowarning <- function(x) suppressWarnings(as.numeric(x))
  tmp <- rvest::html_table(table_node) 
  if (length(tmp) == 1) tmp <- tmp[[1]]
  
  tmp <- tmp %>%
    tidyr::extract(Date, c("Date_Start", "Date_End"), "(\\d{1,2}.\\d{1,2}) - (\\d{1,2}.\\d{1,2})") %>%
    dplyr::select(-Spread) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("Date"), ~paste0(., "/2020") %>% lubridate::mdy())) %>%
    tidyr::extract(Sample, c("Sample_n", "Sample_type"), regex = "(\\d{1,}) (.*)") %>%
    dplyr::mutate(dplyr::across(c(MoE, Sample_n), as.numeric.nowarning)) %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    tidyr::pivot_longer(-dplyr::one_of(nopivot), names_to = "candidate", values_to = "pct") %>%
    tidyr::nest(res = c(candidate, pct)) %>%
    # dplyr::rename(pollster = Poll) %>%
    dplyr::mutate(poll_link = get_poll_link(table_node),
                  rcp_poll_page_link = url_frag)
  
  tmp
}

get_rcp_poll_info <- function(node) {
  idx <- NULL
  races <- rvest::xml_nodes(node, xpath = "tr/td[@class='lp-race']/a") %>% rcp_race_info()
  polls <- rvest::xml_nodes(node, xpath="tr/td[@class='lp-poll']/a") %>% rcp_poll_info()
  # get detailed poll info only once for each state, not with each poll
  dplyr::left_join(races, polls, by = "idx")
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
  table_node <- data <- . <- NULL
  idx <- state <- Poll <- pollster <- NULL
  
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
    dplyr::mutate(data = purrr::map(table_node, ~try(get_rcp_poll_info(.)))) %>%
    dplyr::select(-table_node) %>%
    tidyr::unnest(data)
  
  detailed_info <- unique(info$rcp_poll_page_link)  %>%
      purrr::map_df(rcp_detail_poll_info)
  info <- dplyr::left_join(info, detailed_info, by = c("rcp_poll_page_link", "poll_link")) %>%
    dplyr::select(-idx) #%>%
#    dplyr::rename(Date_Posted = date, Pop = state, Poll_Abbrev = Poll, Poll = pollster)

  info
}