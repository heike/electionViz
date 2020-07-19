#' Create electoral bead chart
#' 
#' @param 



library(tidyverse)
data("electoral_votes_2016")

# Column height
height <- max(sqrt(electoral_votes_2016$electoral_votes)*8)
# space between beads
buffer <- min(sqrt(electoral_votes_2016$electoral_votes)*3)

# Prep data
seg_arrange <- electoral_votes_2016 %>%
  mutate(split = perc_dem - perc_rep) %>%
  arrange(desc(split)) %>%
  mutate(won = ifelse(split > 0, "Dem", "Rep"),
         evtot = cumsum(electoral_votes), 
         ord = row_number(),
         seq = row_number() - min(which(evtot >= 270)),
         seqsign = sign(seq),
         absseq = abs(seq)) 

cum_half_sum <- function(x) {
  cumsum(lag(x, 1, 0)) + .5*x
}


fix_df <- function(df) {
  df %>% 
    mutate(
      # Compute point radius
      radius = sqrt(electoral_votes/pi),
      # Compute full distance between beads
      bead_dist = 2*radius + buffer,
      # Total dist along string
      total_dist = cum_half_sum(bead_dist),
      # what x column is it in?
      xcol2 = floor(total_dist/height),
      # Which direction is the string going?
      ydir = ifelse(floor(total_dist/height) %% 2 == 1, 1, -1)
    ) %>%
    group_by(xcol2) %>%
    # Get column width
    mutate(colwidth = max(bead_dist) - buffer) %>%
    ungroup() %>%
    mutate(
      # Compute y coord
      y = cum_half_sum(ydir*bead_dist)
    )
}

# Split into chunks
left <- filter(seg_arrange, seq <= 0) %>%
  arrange(absseq) %>%
  fix_df()
right <- filter(seg_arrange, seq >= 0) %>%
  arrange(absseq) %>% 
  fix_df()
# mid <- filter(seg_arrange, seq==0) %>%
#   fix_df() %>%
#   mutate(xcol2 = 0)

mysumfun <- function(x) {
  if (is.character(x)) {
    if (length(unique(x)) == 1) unique(x) else NA
  } else mean(x)
}

  
plot_df <- bind_rows(left, right) %>%
  group_by(ord) %>%
  summarize(across(everything(), mysumfun)) %>%
  arrange(ord) %>%
  nest(data = -c(xcol2, colwidth, seqsign)) %>%
  # Compute x coord
  mutate(x = cumsum(lag(colwidth, 1, 0)) + .5 * colwidth) %>%
  unnest(c(data)) %>%
  nest(extra = c(colwidth, seq, absseq, bead_dist, total_dist, ydir))


ggplot() + 
  ggforce::geom_bezier(data = bind_rows(plot_df, ctrl_pts), aes(x = x, y = y, group = xcol2)) + 
  ggforce::geom_circle(aes(x0 = x, y0 = y, r = radius, fill = ydir), alpha = .5) + 
  geom_text(aes(x = x, y = y, label = ord)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) + 
  coord_fixed()
