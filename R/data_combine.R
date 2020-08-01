#' Select polls
#' 
#' Filter polls based on k most recent by state or since a set date.
#' @param polls data frame of polling results as returned by `fivethirtyeight_update``
#' @param method which method do we use to include polls: "last_k" includes the last `k` polls, "since" includes polls as specified in `since`.
#' @param k include k most recent polls
#' @param since date after which polls to include (based on `date` or `end_date`).
#' @importFrom lubridate weeks today
#' @export
polls_filter <- function(polls, method = "last_k", k = 5, since = today() - weeks(4)) {
  state <- poll_id <- end_date <- pct <- candidate_party <- NULL
#  pct_diff <- perc_rep <- perc_dem <- state_district <- NULL
  
  state_polls <- polls %>%
    filter(!is.na(state)) %>%
    group_by(poll_id) %>%
    summarize(
      state = state[1],
      end_date = end_date[1],
      pct_diff = pct[candidate_party=="REP"] - pct[candidate_party=="DEM"],
      perc_dem = pct[candidate_party=="DEM"],
      perc_rep = pct[candidate_party=="REP"]
    ) %>%
    ungroup()
  
  if (method == "last_k") {
    state_1 <- state_polls %>%
      group_by(state) %>%
      mutate(rank = length(end_date) - rank(end_date, ties.method = "random") + 1) 
    state_1 <- state_1 %>% filter(rank <= k) %>% 
      ungroup() 
  }
  if (method == "since") {
    # look for date first:
    idx <- grep("^date$", names(state_polls))
    if (length(idx) == 1) {
      state_1 <- state_polls %>% filter(date >= since) %>% 
        ungroup() 
    }
    idx <- grep("^end_date$", names(state_polls))
    if (length(idx) == 1) {
      state_1 <- state_polls %>% filter(end_date >= since) %>% 
        ungroup() 
    }
  }
  
  state_1
}

#' Average polls and supplement with election data
#'
#' Find a state/district average of polls and supplement averages with election data in case no recent polls are available for a state or district.
#' @param polls data frame of polls as returned by `polls_filter`
#' @param election tibble of election results (with number of electoral votes)
#' @importFrom stats reorder
#' @importFrom lubridate today
#' @export
#' @return tibble of average polls supplemented by last k polls
#' polls_plus(presidential_polls %>% filter(population %in% c("lv", "rv")),
#'            electoral_votes_2016) %>% head()
polls_plus <- function(polls, election) {
  state <- poll_id <- end_date <- pct <- candidate_party <- NULL
  pct_diff <- perc_rep <- perc_dem <- state_district <- NULL
  electoral_votes <- NULL
  
  state_2 <- polls %>%
    group_by(state) %>%
    summarize(
      pct_diff = mean(pct_diff),
      perc_dem = mean(perc_dem),
      perc_rep = mean(perc_rep)
      ) %>%
    mutate(source="Polls")
  
  
  add_in <- anti_join(election, state_2, by=c("state_district"="state")) %>%
    mutate(
      pct_diff = perc_rep-perc_dem,
      state = state_district
    ) %>%
    select(state, pct_diff, perc_dem, perc_rep) %>%
    mutate(
      source = "Election"
    )
  
  state_3 <- rbind(state_2, add_in) %>%
    mutate(
      state = reorder(factor(state), pct_diff, mean)
    )
  
#  state_3 %>% anti_join(election, by = c("state"="state_district"))
  state_3 %>% left_join(election %>% select(state_district, electoral_votes), by = c("state"="state_district"))
}
