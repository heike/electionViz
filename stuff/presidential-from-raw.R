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
