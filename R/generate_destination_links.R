#' Generate links connecting the modal networks to destinations
#'
#' This function generates links connecting into the destination of each trip 
#' in the`trips` dataset from each modal network corresponding to the time 
#' period of the trip
#'
#' @param trips A dataset containing the list of trips being evaluated
#' @param modal_networks A named list containing:
#'   - walk: a single modal network
#'   - bike: a list of bike networks for each time period
#'   - car:  a list of car networks for each time period
#' @param walk_speed Numeric. Assumed walk speed in kilometres per hour.
#'
#' @return A named list containing:
#'   - walk: a table of links connecting the walk network to destinations
#'   - bike: a list of tables of links connecting bike networks to destinations 
#'   for each time period
#'   - car: a list of tables of links connecting car networks to destinations 
#'   for each time period
#'
#' @keywords internal
#'
#' @import sf
#' @import dplyr
#' @import purrr
#' @import stringr
generate_destination_links <- function(trips,modal_networks,walk_speed) {
  
  # ------------------------------------------------------------
  # Define time periods
  # ------------------------------------------------------------
  
  time_periods <- c(
    "MoFr04000700", "MoFr07000900", "MoFr09001200", "MoFr12001400",
    "MoFr14001600", "MoFr16001900", "MoFr19002200", "MoFr22000400",
    "SaSu04000700", "SaSu07001000", "SaSu10001400", "SaSu14001900",
    "SaSu19002200", "SaSu22000400"
  )
  
  # ------------------------------------------------------------
  # Get links connecting origins to the nearest network node
  # ------------------------------------------------------------
  
  get_destination_links <- function(network, p = NA) {
    
    trips %>%
      filter(is.na(p)|period == p) %>%
      rowwise() %>%
      mutate(
        nearest_endnode_row = {
          distsquared <- (network$to_easting-to_easting)^2 + 
            (network$to_northing-to_northing)^2
          which.min(distsquared)
        }
      ) %>%
      ungroup() %>%
      mutate(
        from_node = network$to_node[nearest_endnode_row],
        to_node = str_c("destination",trip_id),
        from_easting = network$to_easting[nearest_endnode_row],
        from_northing = network$to_northing[nearest_endnode_row],
        distance = sqrt(
          (network$to_easting[nearest_endnode_row]-to_easting)^2 +
            (network$to_northing[nearest_endnode_row]-to_northing)^2
        ),
        time = distance / (walk_speed*1000/60),
      ) %>%
      select(trip_id,from_node,to_node,distance,time,from_easting,from_northing,to_easting,to_northing)
  }
  
  
  # ------------------------------------------------------------
  # Return a list of destination links
  # ------------------------------------------------------------
  
  list(
    walk = get_destination_links(modal_networks$walk),
    bike = map(time_periods, ~ get_destination_links(modal_networks$bike[[.x]],.x)) %>% 
      set_names(time_periods),
    car = map(time_periods, ~ get_destination_links(modal_networks$car[[.x]],.x)) %>% 
      set_names(time_periods)
  )
  
}