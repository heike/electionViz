electoral_votes_2016 <- electoral_votes_2016 %>% mutate(
  state_district = ifelse(state_district=="NE-1", "Nebraska CD-1", state_district),
  state_district = ifelse(state_district=="NE-2", "Nebraska CD-2", state_district),
  state_district = ifelse(state_district=="NE-3", "Nebraska CD-3", state_district),
  state_district = ifelse(state_district=="ME-1", "Maine CD-1", state_district),
  state_district = ifelse(state_district=="ME-2", "Maine CD-2", state_district)
  
)
use_data(electoral_votes_2016, overwrite = TRUE)


presidential <- fivethirtyeight_update()
presidential <- presidential %>%
  mutate(
    start_date = lubridate::mdy(start_date),
    end_date = lubridate::mdy(end_date),
    election_date = lubridate::mdy(election_date)
  )
use_data(presidential, overwrite = TRUE)

roxify <- function(data) {
  items <- names(data)
  classes <- data %>% purrr::map_chr(.f = class)
  itemlist <- paste0("#\'   \\item{",items,"}{", classes,"}", sep="", collapse="\n")
  cat(sprintf("#\' @format A tibble with %s rows and %s columns
#\' \\describe{
%s
#\' }
", nrow(data), ncol(data), itemlist))
}
