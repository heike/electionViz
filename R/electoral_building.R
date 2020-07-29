#' Electoral Building
#' 
#' @param state_district state/district name
#' @param electoral_votes number of electoral votes
#' @param perc_dem percent democrats
#' @param perc_rep percent republicans
#' @export
#' @examples 
#' library(ggplot2)
#' electoral_building(state_district = electoral_votes_2016$state_district, 
#'                    electoral_votes = electoral_votes_2016$electoral_votes, 
#'                    perc_dem = electoral_votes_2016$perc_dem, 
#'                    perc_rep = electoral_votes_2016$perc_rep) +
#'   scale_color_party("Party") +
#'   scale_fill_party("Party") +
#'   theme(legend.position = "bottom")
electoral_building <- function(state_district, electoral_votes, perc_dem, perc_rep){
  rep_margin <- victor <- location_max <- location_min <- height_min <- height_max <- text_loc <- hjust_param <- NULL
  
  df <- data.frame(state_district, electoral_votes, perc_dem, perc_rep, stringsAsFactors = F)
  df <- df %>% mutate(rep_margin = perc_rep - perc_dem, 
                      margin = abs(rep_margin), 
                      victor = c("Republican", "Democrat")[as.numeric(perc_rep < perc_dem) + 1]) %>%
    group_by(victor) %>%
    nest(data = c(state_district, electoral_votes, perc_dem, perc_rep, rep_margin, margin)) %>%
    mutate(data = purrr::map(data, .f = function(data){
      data <- data %>% arrange(desc(margin), electoral_votes) %>%
        mutate(height_min = cumsum(electoral_votes) - electoral_votes, height_max = cumsum(electoral_votes))
      return(data)
    })) %>%
    unnest(cols = c(data))%>%
    mutate(location_min = ifelse(victor == "Republican", 0, rep_margin), 
           location_max = ifelse(victor == "Republican", rep_margin, 0), 
           text_loc = ifelse(victor == "Republican", location_max + 2, location_min - 2), 
           hjust_param = ifelse(victor == "Republican", 0, 1))
  
  p <- df %>% 
    ggplot() + 
    geom_hline(aes(yintercept = 270), lty = 2, colour = "grey20") + 
    geom_rect(aes(xmin = location_min, xmax = location_max, 
                  ymin = height_min, ymax = height_max, 
                  fill = factor(victor), color = factor(victor)), alpha = 0.7) + 
    geom_text(aes(x = text_loc, y = (height_min + height_max)/2, 
                  label = state_district, hjust = hjust_param), size = 2.5, color = "black",
              data = df %>% filter(margin < 15 | electoral_votes < 7)) + 
    geom_text(aes(x = (location_min + location_max)/2, y = (height_min + height_max)/2, 
                  label = state_district, hjust = 0.5), size = 2.5, color = "black",
              data = df %>% filter(margin >= 15, electoral_votes >= 7)) + 
    labs(x = "Margin of Victory", y = "Number of Electoral Votes") + 
    theme_void() + 
    theme(axis.text = element_text(color = "grey20"), 
          legend.text = element_text(color = "grey20"), 
          axis.title = element_text(color = "grey20"), 
          axis.title.y = element_text(angle = 90), 
          panel.grid.major = element_line(colour = "grey80")) + 
    annotate(geom = "text", x = -75, y = 275, 
             label = "270 Electoral Votes to Win", 
             color = "grey20",size = 3) + 
    xlim(c(-100, 100))
  
  return(p)
}