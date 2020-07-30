#' Helper function
#' 
#' This function is useful for calculating the location of the center of the 
#' beads in various situations
#' @param x vector
cum_half_sum <- function(x) {
  cumsum(lag(x, 1, 0)) + .5*x
}

#' Helper function
#' 
#' Sets up beads based on initial information
#' This should eventually become stat_beads or something like that, I think.
#' @param df data frame
#' @param height overall height
#' @param buffer space between beads
set_up_beads <- function(df, height = NULL, buffer = NULL) {
  # R CMD check
  electoral_votes <- radius <- bead_dist <- total_dist <- xcol2 <- NULL
  ydir <- NULL
  
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

#' Helper function
#' 
#' Summarize by taking the mean (if numeric), the unique value 
#' (if there's only one), and NA otherwise
#' @param x vector
mysumfun <- function(x) {
  if (is.character(x)) {
    if (length(unique(x)) == 1) unique(x) else NA
  } else if (is.list(x)) {
    NA
  } else {
    mean(x)
  } 
}

#' Helper function
#' 
#' This function computes the control point based on weights passed in - 
#' the control point is (xwt, ywt) in relative proportion between the two rows 
#' in the data frame
#' @param dfs dataset
#' @param xwt some weight along x direction
#' @param ywt some weight along y direction
#' @param dir parameter controlling the direction
#' @param ... passed on to inside functions
compute_control_point <- function(dfs, xwt = .5, ywt = .5, dir = 0, ...) { 
  # check df has columns x, y
  stopifnot("x" %in% names(dfs), "y" %in% names(dfs), "radius" %in% names(dfs))
  stopifnot(nrow(df) == 2)
  
  # R CMD check
  x <- y <- NULL
  
  # create a row for the control point
  dfnew <- dfs %>% summarize(across(everything(), mysumfun))
  dfnew$x <- as.numeric(t(dfs$x) %*% c(xwt, 1 - xwt))
  # if dir != 0, move y by dir*radius
  dfnew$y <- as.numeric(t(dfs$y) %*% c(ywt, 1 - ywt) + t(dfs$radius) %*% c(ywt, 1 - ywt) * dir)
  
  dfnew$.type <- "control"
  dfs$.type <- "endpoint"
  
  bind_rows(dfs, dfnew) %>% arrange(x, y)
}

#' Helper function
#' 
#' Compute the direction/y offset of the bezier control point
#' @param xx data frame
#' @param adjust integer
compute_dir <- function(xx, adjust = 3) {
  # This function should be applied to a whole uni-directional-ish segment 
  # e.g. y is almost always increasing
  #
  # the value returned is 
  end_seg_dir <- sign(mean(diff(xx$y))) * adjust
  beg_seg_dir <- -end_seg_dir
  c(beg_seg_dir, rep(0, nrow(xx) - 3), end_seg_dir)
}

#' Add control points to the middle of a two-point data frame
#' 
#' @param df tibble subset of bead dataframe
#' @param dir direction
#' @param adjust adjustment
#' @import tibble
#' @import dplyr
#' @import purrr
add_control_points <- function(df, dir = "right", adjust = 3) {
  # this assumes df is properly sorted ahead of time
  idx <- NULL
    
  # dir is the direction in x - moving right or moving left
  if (dir == "left") {
    df <- rev(df)
  }
  
  cptbl <- tibble(
    xwt = c(.75, rep(.5, length(df) - 2), .25),
    ywt = c(.25, rep(.5, length(df) - 2), .75)
  )
  
  # define the weights for the  coordinates
  cptbl_2row <- tibble(
    idx = 1:(nrow(df) - 1),
    idxs = purrr::map(idx, ~c(., .+1)),
    dfs = purrr::map(idx, ~df[idxs[[.]],]),
    xwt = c(.75, rep(.5, nrow(df) - 3), .25),
    ywt = c(.25, rep(.5, nrow(df) - 3), .75),
    dir = compute_dir(df, adjust)
  )
  
  purrr::pmap_df(cptbl_2row, compute_control_point, .id = "id") %>%
    mutate(id = df$ord[as.numeric(id)])
}

#' Split the bead dataframe into directional segments 
#' 
#' @param df tibble with bead information
#' @param height plot height (in sqrt(electoral votes/pi) units)
#' @param buffer space between circles
#' @import purrr
#' @import dplyr
#' @import tibble
split_df_bezier <- function(df, height = NULL, buffer = NULL) {
  stopifnot(c("xcol2", "seqsign", "x", "y", "ord") %in% names(df))
  # making R CMD check happpy
  ord <- nr <- x <- y <- seqsign <- xcol2 <- NULL

  # break up segments
  df <- df %>% mutate(xcol2 = seqsign * (abs(seqsign) + xcol2))
  # get main parts of segments
  tmp <- split(df, df$xcol2)
  
  cpdf <- tibble(
    df = map(tmp, function(xx) {
      if (nrow(xx) > 1) {
        range <- range(xx$ord) + c(-1, 1)
        filter(df, ord >= range[1], ord <= range[2])
      } else {
        tibble()
      }
    }),
    nr = map_int(df, nrow)
  ) %>%
    filter(nr > 0) %>%
    mutate(
      bezier = purrr::map(df, add_control_points, adjust = buffer)
    )
  
  bind_rows(cpdf$bezier) %>%
    arrange(id, x, y) %>%
    unique()
}


#' Plot state electoral votes as beads on a string
#' 
#' TODO: Make this work with NSE 
#' 
#' @param state name of state/district
#' @param votes number of electoral votes
#' @param perc_dem percentage democrats
#' @param perc_rep percentage republicans
#' @param height column height, default is carefully chosen (as max(sqrt(electoral_votes_2016$electoral_votes)*8)) based on number of electoral votes 
#' @param buffer space between beads, defaults to 3 based on min(sqrt(electoral_votes_2016$electoral_votes)*3)
#' @importFrom ggforce geom_bezier geom_circle
#' @importFrom tidyr nest
#' @import ggplot2 
#' @importFrom stats df
#' @export
#' @examples 
#' 
#' data("electoral_votes_2016")
#' bead_snake_plot(
#'   electoral_votes_2016$state_district,
#'   electoral_votes_2016$electoral_votes,
#'   electoral_votes_2016$perc_dem,
#'   electoral_votes_2016$perc_rep
#'   ) 
#' bead_snake_plot(
#'   electoral_votes_2016$state_district,
#'   electoral_votes_2016$electoral_votes,
#'   electoral_votes_2016$perc_dem,
#'   electoral_votes_2016$perc_rep, 
#'   height = 30, buffer = 3)
bead_snake_plot <- function(state, votes, perc_dem, perc_rep, height = 60, buffer = 3) {
  # Make R CMD check happy
  evtot <- absseq <- NULL
  ord <- xcol2 <- colwidth <- seqsign <- bead_dist <- total_dist <- NULL
  ydir <- state.name <- state.abb <- x <- y <- NULL
  radius <- label <- abb <- NULL
  electoral_votes <- state_district <- NULL
 
  data <- data.frame(state_district = as.character(state), electoral_votes = votes,
                     perc_dem = perc_dem, perc_rep = perc_rep, stringsAsFactors = FALSE) 
#  data("electoral_votes_2016", package="electionViz", envir = environment())
  # Prep data
  seg_arrange <- data %>%
    mutate(split = perc_dem - perc_rep) %>%
    arrange(desc(split)) %>%
    mutate(won = ifelse(split > 0, "Dem", "Rep"),
           evtot = cumsum(electoral_votes), 
           ord = row_number(),
           seq = row_number() - min(which(evtot >= 270)),
           seqsign = sign(seq),
           absseq = abs(seq)) 
  
  # Split into chunks
  left <- filter(seg_arrange, seq <= 0) %>%
    arrange(absseq) %>%
    set_up_beads(height = height, buffer = buffer)
#    set_up_beads(., height = height, buffer = buffer)
  right <- filter(seg_arrange, seq >= 0) %>%
    arrange(absseq) %>% 
    set_up_beads(height = height, buffer = buffer)
#   set_up_beads(., height = height, buffer = buffer)
  
  plot_df <- bind_rows(left, right) %>%
    group_by(ord) %>%
    summarize(across(everything(), mysumfun)) %>%
    arrange(ord) %>%
    # Hide unnecessary stuff
    tidyr::nest(data = -c(xcol2, colwidth, seqsign)) %>%
    # Compute x coord
    mutate(x = cumsum(lag(colwidth, 1, 0)) + .5 * colwidth) %>%
    unnest(c(data)) %>%
    # Hide unnecessary stuff
    nest(extra = c(colwidth, seq, absseq, bead_dist, total_dist, ydir)) 
  
  data("state", package="datasets", envir = environment()) # load data
  # Make nice labels... TODO: make this something that is passed in
  plot_df <- plot_df %>%
    left_join(tibble(state_district = c(state.name, "District of Columbia"), 
                     abb = c(state.abb, "DC"))) %>%
    mutate(abb = ifelse(is.na(abb), state_district, abb)) %>%
    # Create label
    mutate(label = paste0(abb, "(", electoral_votes, ")") %>%
             str_replace("\\(1\\)$", ""))
  
  bezier <- split_df_bezier(plot_df, height = height, buffer = buffer)
  
  # Really probably need to recode this whole thing as a stat + geom combination... sigh.
  ggplot() + 
    # Should probably set this up so that it's not the center of the line, but offset based on % over 270
    # and maybe normalize things? Not sure... space rows based on # electoral votes?...
    geom_vline(aes(xintercept = x), data = filter(plot_df, seqsign == 0), colour = "grey70") + 
    geom_label(aes(x = x, y = Inf, label = "270 Electoral Votes"), data = filter(plot_df, seqsign == 0),
               hjust = 0.5, vjust = 1) + 
    # Plot bezier curve underneath
    ggforce::geom_bezier(data = bezier, aes(x = x, y = y, group = id), colour = "grey70") + 
    # Plot circles scaled so that area is precisely the # electoral votes
    ggforce::geom_circle(aes(x0 = x, y0 = y, r = radius, color = split, fill = split), 
                         alpha = 1, data = plot_df) + 
    geom_text(aes(x = x, y = y+radius, label = label), 
              nudge_x = 1, nudge_y = 1, data = plot_df, size = 2, alpha = 1) +
    scale_color_gradientn(guide = "none", colors = c("darkred", "red", "grey70", "blue", "darkblue"), 
                          values = c(0, .48, .5, .52, 1), limits = c(-100, 100)) + 
    scale_fill_gradientn(guide = "none", colors = c("darkred", "red", "white", "blue", "darkblue"), 
                         values = c(0, .48, .5, .52, 1), limits = c(-100, 100)) + 
    coord_fixed() + 
    theme(panel.grid = element_blank(), panel.background = element_blank(),
          axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), plot.background = element_blank())
  
}
