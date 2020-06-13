source("inst/example-tilegram.R")

popus <- sf_Pitch_US_Population_2016_v1 %>%
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

pop_hex <- popus %>% dplyr::select(-geometry) %>% 
  unnest(cols = data)

res <- pop_hex %>% select(long, lat, order, group, state, FID) 
res <- st_drop_geometry(res)
pop_hex <- res
use_data(pop_hex, overwrite = TRUE)

centers <- sf_Pitch_US_Population_2016_v1.centers %>%
  mutate(
    centers = geometry %>% purrr::map(.f = function(x) {
      data.frame(centerX = x[1], centerY = x[2])
    })
  )
res <- centers %>% unnest(cols=centers)
res <- st_drop_geometry(res)
pop_hex <- pop_hex %>% left_join(res, by = "state")


use_data(pop_hex, overwrite = TRUE)


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
pophex_plus <- pop_hex %>%
  left_join(elections, by=c("state" = "state_po"))

centers <- pop_hex %>% select(centerX, centerY, state) %>% unique()
# Make a first chloropleth map
pophex_plus %>%
  filter(year >= 2016) %>%
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


