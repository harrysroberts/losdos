#' Generate links connecting origins to the modal networks
#'
#' This function generates links connecting the origin in the `origins` dataset 
#' to each modal network corresponding to the time period of the trip
#'
#' @param origins A dataset containing the list of origin points
#' @param modal_networks A named list containing:
#'   - walk: a single modal network
#'   - bike: a list of bike networks for each time period
#'   - car:  a list of car networks for each time period
#' @param walk_speed Numeric. Assumed walk speed in kilometres per hour.
#'
#' @return A named list containing:
#'   - walk: a table of links connecting origins to the walk network
#'   - bike: a list of tables of links connecting origins to bike networks for 
#'   each time period
#'   - car: a list of tables of links connecting origins to car networks for 
#'   each time period
#'
#' @keywords internal
#'
#' @import sf
#' @import dplyr
#' @import purrr
#' @import stringr
generate_origin_links <- function(origins,modal_networks,walk_speed) {
  
  # ------------------------------------------------------------
  # Define time periods present in the modal networks
  # ------------------------------------------------------------
  
  time_periods <- names(modal_networks$bike)
  
  # ------------------------------------------------------------
  # Get links connecting origins to the nearest network node
  # ------------------------------------------------------------
  
  get_origin_links <- function(network, p = NA) {
    
    origins %>%
      filter(is.na(p)|period == p) %>%
      rowwise() %>%
      mutate(
        nearest_startnode_row = {
          distsquared <- (network$from_easting-easting)^2 + 
            (network$from_northing-northing)^2
          which.min(distsquared)
        } 
      ) %>%
      ungroup() %>%
      mutate(
        trip_id = id,
        from_node = str_c("origin",id),
        to_node = network$from_node[nearest_startnode_row],
        from_easting = easting,
        from_northing = northing,
        to_easting = network$from_easting[nearest_startnode_row],
        to_northing = network$from_northing[nearest_startnode_row],
        distance = sqrt(
          (network$from_easting[nearest_startnode_row]-from_easting)^2 +
            (network$from_northing[nearest_startnode_row]-from_northing)^2
        ),
        time = distance / (walk_speed*1000/60),
      ) %>%
      select(trip_id,from_node,to_node,distance,time,from_easting,from_northing,to_easting,to_northing)
  }
  
  
  # ------------------------------------------------------------
  # Return a list of origin links
  # ------------------------------------------------------------
  
  list(
    walk = get_origin_links(modal_networks$walk),
    bike = map(time_periods, ~ get_origin_links(modal_networks$bike[[.x]], .x)) %>% 
      set_names(time_periods),
    car = map(time_periods, ~ get_origin_links(modal_networks$car[[.x]], .x)) %>% 
      set_names(time_periods)
  )
  
}


