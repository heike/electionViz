# get elections data
elections <- USelections %>% 
  select(-office) %>%
  group_by(year, state, state_po) %>%
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

usethis::use_data(elections, overwrite = TRUE)