#' Average polls and supplement with election data
#'
#' 
#' @param polls tibble result from `fivethirteight_update`
#' @param election tibble of election results (with number of electoral votes)
#' @param k include k most recent polls
#' @importFrom stats reorder
#' @export
#' @return tibble of average polls supplemented by last k polls
#' polls_plus(presidential_polls %>% filter(population %in% c("lv", "rv")),
#'            electoral_votes_2016) %>% head()
polls_plus <- function(polls, election, k = 5) {
  state <- poll_id <- end_date <- pct <- candidate_party <- NULL
  pct_diff <- perc_rep <- perc_dem <- state_district <- NULL
  electoral_votes <- NULL
  
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
  
  
  state_1 <- state_polls %>%
    group_by(state) %>%
    mutate(rank = length(end_date) - rank(end_date, ties.method = "random") + 1) %>%
    filter(rank <= k) %>% 
    ungroup() 
  
  state_2 <- state_1 %>%
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
