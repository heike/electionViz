state_polls <- presidential_polls %>%
  filter(!is.na(state)) %>%
  group_by(poll_id) %>%
  summarize(
    state = state[1],
    pollster = pollster[1],
    fte_grade = fte_grade[1],
    sample_size = sample_size[1],
    population = population[1],
    start_date = start_date[1],
    end_date = end_date[1],
    pct_diff = pct[candidate_party=="REP"] - pct[candidate_party=="DEM"]
  ) %>%
  ungroup()


state_1 <- state_polls %>%
  group_by(state) %>%
  mutate(rank = length(end_date) - rank(end_date, ties.method = "random") + 1) %>%
  filter(rank <= 5) %>% 
  ungroup() 

state_2 <- state_1 %>%
  group_by(state) %>%
  summarize(pct_diff = mean(pct_diff)) %>%
  mutate(source="Polls")


add_in <- anti_join(electoral_votes_2016, state_2, by=c("state_district"="state")) %>%
  mutate(
    pct_diff = perc_rep-perc_dem,
    state = state_district
    ) %>%
  filter(!(state_district %in% c("NE-1", "NE-2", "NE-3", "ME-1", "ME-2"))) %>%
  select(state, pct_diff) %>%
  mutate(
    source = "Election 2016"
  )

state_3 <- rbind(state_2, add_in) %>%
  mutate(
    state = reorder(factor(state), pct_diff, mean)
  )
state_1 <- state_1 %>%
  mutate(state = factor(state, levels = levels(state_3$state)))

gg_states <- state_1 %>%
  ggplot(aes(x = pct_diff, y = state)) + 
  geom_rect(xmin=-5, xmax=+5, ymin=-Inf, ymax = +Inf, 
            fill="#fcfcc5", size=0) + 
  geom_vline(xintercept = 0, colour = "grey10") + 
  geom_point(colour = "grey50", alpha = 0.5, size = 1) +
  theme_bw() +
  ylab("") +
  scale_y_discrete(drop=FALSE)


  
gg_states +
  geom_point(aes(x = pct_diff, colour = pct_diff>0, shape=source),
             size = 2, data = state_3) +
  scale_colour_party("Party", labels=c("Democrat", "Republican")) +
  scale_x_continuous("Percent Difference", breaks=c(-60,-40,-20,0,20,40,60), 
                     limits=c(-60,60),
                     labels = c("←\nMore Democrat", "40","20",0,20, 40, "→\nMore Republican")) +
  theme(legend.position="bottom") +
  scale_shape_manual("Source", values = c(1, 19))

