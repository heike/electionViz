source("inst/example-tilegram.R")

electoral <- sf_FiveThirtyEightElectoralCollege %>%
  group_by(state) %>%
  mutate(
    group = paste(state, 1:n(), sep="-"),
    data = geometry %>% purrr::map(.f = function(x) {
      res <- data.frame(x[[1]])
      names(res) <- c("long", "lat")
      res$order <- 1:nrow(res)
      res
    })
  ) %>% select(-geometry)

electoral_hex <- electoral %>% st_drop_geometry() %>% 
  unnest(cols = data)

use_data(electoral_hex, overwrite = TRUE)

centers <- sf_FiveThirtyEightElectoralCollege.centers %>%
  mutate(
    centers = geometry %>% purrr::map(.f = function(x) {
      data.frame(centerX = x[1], centerY = x[2])
    })
  )
res <- centers %>% unnest(cols=centers)
res <- st_drop_geometry(res)
electoral_hex <- electoral_hex %>% left_join(res, by = "state")


use_data(electoral_hex, overwrite = TRUE)


elections <- USelections %>% 
  select(-office) %>%
  group_by(year, state_po) %>%
  mutate(percent = candidatevotes/totalvotes*100) %>%
  summarise(
    votes_dem = candidatevotes[party %in% c("democrat", "democratic-farmer-labor")],
    votes_rep = candidatevotes[party=="republican"],
    perc_dem = percent[party %in% c("democrat", "democratic-farmer-labor")],
    perc_rep = percent[party=="republican"],
    totalvotes = max(totalvotes),
    cand_dem = candidate[party %in% c("democrat", "democratic-farmer-labor")],
    cand_rep = candidate[party=="republican"],
    .groups = 'drop'
  )

# Merge geospatial and numerical information
electoral_hexplus <- electoral_hex %>%
  left_join(elections, by=c("state" = "state_po"))

centers <- electoral_hex %>% select(centerX, centerY, state) %>% unique()
# Make a first chloropleth map
electoral_hexplus %>%
  filter(year >= 2012) %>%
  mutate(
    Election = year,
    Outcome = c("Republican", "Democrat")[as.numeric(perc_rep < perc_dem) + 1]) %>%
  ggplot() +
  geom_polygon(aes(fill =  Outcome, 
                   x = long, y = lat, group = group), 
               colour = "grey90", size = 0.1) +
  scale_fill_manual(values = c("darkblue", "darkred")) +
  geom_text(aes(x = centerX, y = centerY, label = state), 
            colour = "grey80", size = 3, data = centers) +
  theme_void() +
#  coord_map() +
  facet_wrap(~Election, labeller = "label_both") +
  theme(legend.position = "bottom") 

# combine hexagons of each state into a single shape:

outlines <- sf_FiveThirtyEightElectoralCollege %>% 
  group_by(state, FID) %>% nest() %>%
  mutate(
    outline = data %>% purrr::map(.f = function(d) {
#      browser()
      st_union(d$geometry)
    }),
    outline = outline %>% purrr::map(.f = st_simplify)
  )

# florida and california have additional edges XXX fix later

outlines <- outlines %>%
  mutate(
    group = state,
    data = outline %>% purrr::map(.f = function(x) {
      res <- data.frame(x[[1]][[1]])
      names(res) <- c("long", "lat")
      res$order <- 1:nrow(res)
      res
    })
  ) 

state_electoral_hex <- outlines %>% unnest(cols=data)
state_electoral_hex <- select(state_electoral_hex, -outline)
state_electoral_hex <- state_electoral_hex %>% ungroup()
state_electoral_hex %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_path()
electoral_state_outline_hex <- state_electoral_hex
use_data(electoral_state_outline_hex, overwrite = TRUE)

centers <- electoral_hex %>% select(centerX, centerY, state) %>% unique()
electoral_state_outline_hex <- electoral_state_outline_hex %>% left_join(centers, by = "state")
