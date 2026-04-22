#' Augments the modal networks by appending the origin and destination links 
#'
#' This appends to each modal network links connecting it to the origins and
#' destinations of the trips being evaluated.
#'
#' @param modal_networks A named list containing:
#'   - walk: a single modal network
#'   - bike: a list of bike networks for each time period
#'   - car:  a list of car networks for each time period
#' @param origins A named list containing:
#'   - walk: a table of links connecting origins to the walk network
#'   - bike: a list of tables of links connecting origins to bike networks for 
#'   each time period
#'   - car: a list of tables of links connecting origins to car networks for 
#'   each time period
#' @param destinations A named list containing:
#'   - walk: a table of links connecting the walk network to destinations
#'   - bike: a list of tables of links connecting bike networks to destinations 
#'   for each time period
#'   - car: a list of tables of links connecting car networks to destinations 
#'   for each time period
#'   
#' @return A named list containing:
#'   - walk: a single augmented walk network
#'   - bike: a list of augmented bike networks for each time period
#'   - car:  a list of augmented car networks for each time period
#'
#' @keywords internal
#'
#' @import sf
#' @import dplyr
#' @import purrr
#' @import stringr
generate_augmented_networks <- function(modal_networks,origins,destinations) {
  
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
  # Return a list of augmented networks
  # ------------------------------------------------------------
  
  list(
    
    walk = modal_networks$walk %>% 
      bind_rows(select(origins$walk,!trip_id)) %>% 
      bind_rows(select(destinations$walk,!trip_id)),
    
    bike = map(
      time_periods, 
      ~ modal_networks$bike[[.x]] %>%
        bind_rows(select(origins$bike[[.x]],!trip_id)) %>%
        bind_rows(select(destinations$bike[[.x]],!trip_id))
      ) %>% 
      set_names(time_periods),
    
    car = map(
      time_periods, 
      ~ modal_networks$car[[.x]] %>%
        bind_rows(select(origins$car[[.x]],!trip_id)) %>%
        bind_rows(select(destinations$car[[.x]],!trip_id))
      ) %>% 
      set_names(time_periods)
    
  )
}
