svg_split <- function(x) {
  # Start by separating types of point sequences
  tmp <- x %>%
    gsub(" ([A-z]) ", "; \\1 ", .) %>%
    strsplit(., ";") %>% 
    unlist(recursive = F) %>%
    gsub(" $", "", .) %>%
    gsub("^ ", "", .)
  
  # Then, separate coordinates in each point sequence
  df <- tibble::tibble(type = gsub("^([A-z]) (.*)", "\\1", tmp),
                       coords = gsub("^([A-z]) (.*)", "\\2", tmp) %>% 
                         gsub(" $", "", .)) %>%
    dplyr::mutate(coords = strsplit(coords, " ")) %>%
    tidyr::unnest(coords) %>%
    tidyr::separate(coords, into = c("x", "y"), sep = ",", fill = "right") %>%
    dplyr::mutate(order = 1:length(y),
                  x = as.numeric(x), y = as.numeric(y))
  
  # Handle H w/ undef NA -- horizontal move, doesn't require y to change
  df <- df %>%
    dplyr::mutate(y = ifelse(is.na(y), ifelse(type == "h", 0, dplyr::lag(y, 1)), y))
  
  # # lowercase letters are relative points
  # # This section may not be necessary now - I've saved the SVG using only 
  # # absolute coordinates
  # lowercase <- which(df$type %in% letters)
  # uppercase <- which(df$type %in% LETTERS)
  # 
  # if (length(lowercase) > 0) {
  #   # If first coord in group is lowercase, it's still absolute coords
  #   if (length(uppercase) == 0) {
  #     df$type[1] <- toupper(df$type[1])
  #     uppercase <- 1
  #     lowercase <- setdiff(lowercase, 1)
  #   }
  #   
  #   # Handle lowercase (relative) coordinates by taking the last absolute coord
  #   # and summing the changes
  #   df <- df %>%
  #     dplyr::mutate(capchunk = cumsum(type == toupper(type))) %>%
  #     dplyr::group_by(capchunk) %>%
  #     dplyr::mutate(x = cumsum(x), y = cumsum(y), type = toupper(type)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(-capchunk)
  # }
  # 
  # # Handle other lowercase
  # df <- df %>%
  #   dplyr::mutate(x = ifelse(type %in% letters, x + dplyr::lag(x, 1, 0), x),
  #                 y = ifelse(type %in% letters, ifelse(is.na(y), 0, y) + dplyr::lag(y, 1, 0), y),
  #                 type = ifelse(type %in% letters, toupper(type), type))

  
  # Handle bezier start points
  df <- df %>%
    dplyr::mutate(type = ifelse(dplyr::lead(type, 1, default = "M") == "C", 
                                # Change type to "C" if it's the first point in a bezier sequence
                                # (those can be marked as something other than C)
                                ifelse(type == "C", type, "C"), type)) %>%
    tidyr::unnest(type) %>%
    # Ensure things are ordered correctly, and then get rid of distinctions between
    # M, H, L -- since everything but bezier curves have been expanded properly
    dplyr::mutate(type = factor(type, levels = c("M", "H", "L", "C"), 
                                labels = c("P", "P", "P", "C"))) %>%
    # Don't keep duplicates
    unique() %>%
    # Ensure C is the last point (so that it is just above the other bezier points)
    dplyr::arrange(order, type)
  
  df
}

#' Take control points and turn them into a full sequence of points along the 
#' curve
#' @param type Point type (SVG specification)
#' @param data data frame with columns x, y, order
#' @importFrom bezier bezier
#' @importFrom purrr set_names
#' @importFrom dplyr mutate arrange desc
bezier_control_to_df <- function(type, data) {
  if (type != "C") {
    return(data)
  }
  
  if (!nrow(data) %in% c(4, 7)) {
    warn("Must have 4 or 7 control points: start, control1, control2, end")
    return(data)
  }
  
  if (nrow(data) == 7) {
    return(
      rbind(bezier_control_to_df(type, data[1:4,]),
            bezier_control_to_df(type, data[4:7,]))
    )
  }
  
  pts <- data[,c("x", "y")] %>% as.matrix()
  
  ctrl <- data.frame(idx = 1:4, dist = abs(data$x - 100), x = data$x, y = data$y) %>%
    dplyr::arrange(dplyr::desc(dist))
  
  xy <- ctrl[3:4,] %>%
    dplyr::arrange(dplyr::desc(y))
  
  bezier::bezier(t = seq(0,1, length.out = 20), p = pts[c(2,3),], 
                 start = pts[1,], end = pts[4,]) %>%
    as.data.frame() %>%
    purrr::set_names(c("x", "y")) %>%
    dplyr::mutate(order = seq(min(data$order), max(data$order), length.out = nrow(.))#,
           #row = seq(min(data$row), max(data$row), length.out = nrow(.))
           )
}


#' Read in the SVG snake with 538 segments as an R object
snake_path <- function() {
  # html <- xml2::read_html(system.file("DATA/snake_segments.svg", package = "electionViz"))
  html <- xml2::read_html("data-raw/snake_segments.svg")
  paths <- xml2::xml_find_all(html, "//path")
  dir_orig <- tibble::tibble(group = 1:length(paths)) %>%
    dplyr::mutate(directions = purrr::map_chr(paths, xml2::xml_attr, "d"))
  
  # Handle SVG data and transition everything into absolute coords
  segments_orig <- dir_orig %>%
    dplyr::mutate(points = purrr::map(directions, svg_split)) %>%
    tidyr::unnest(points) %>%
    dplyr::select(-directions) %>%
    unique()
  
  segments <- segments_orig %>%
    tidyr::nest(data = -c(group, type)) %>%
    dplyr::mutate(data = purrr::map2(type, data, bezier_control_to_df)) %>%
    tidyr::unnest(data) %>%
    dplyr::select(ev = group, order = order, x = x, y = y)
  
  segments
}

snake_path <- snake_path()
usethis::use_data(snake_path)


