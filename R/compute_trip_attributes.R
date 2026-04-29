#' Computes the distance and time of each trip by each mode
#'
#' This acts as a wrapped for the dodgr::dodgr_distances() function that
#' takes the dual network together with pairs of origins and destinations and
#' returns a data frame containing the computed distance and time of each mode
#' for each trip.
#' 
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
#' @param dual_networks A named list containing:
#'   - walk: a single dual walk network
#'   - bike: a list of dual bike networks for each time period
#'   - car:  a list of dual car networks for each time period
#'
#' @return A data frame containing comprising the `trip_id` together with
#' the computed distance and time of each mode as a separate column
#'
#' @keywords internal
#'
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import dodgr
compute_trip_attributes <- function(origins,destinations,dual_networks) {
  
  # ------------------------------------------------------------
  # Define time periods present in the dual networks
  # ------------------------------------------------------------
  
  time_periods <- names(dual_networks$bike)
  
  # ------------------------------------------------------------
  # Get distance and time attributes for trips in each mode/time period
  # ------------------------------------------------------------
  
  get_trip_attributes <- function(o,d,network){
    
    #Build O-D pairs with link keys
    ods <- o %>%
      mutate(from_link = str_c(from_node, ">", to_node)) %>%
      select(trip_id, from_link) %>%
      left_join(
        d %>%
          mutate(to_link = str_c(from_node, ">", to_node)) %>%
          select(trip_id, to_link),
        by = join_by(trip_id)
        )
    
    distances <- network %>%
      
      #place into dodgr edge list format
      mutate(w = time, d = distance) %>%
      select(from_link, to_link, w, d) %>%
      
      #pass each O-D pair to the dodgr_distances function
      dodgr_distances(
        from = ods$from_link,
        to   = ods$to_link,
        pairwise = TRUE
        ) %>%
      
      #vector results, need to bind to the trip_id and name the value
      bind_cols(select(ods,trip_id)) %>%
      rename(distance = ...1) %>%
      select(trip_id,distance)
    
    times <- network %>%
      
      #place into dodgr edge list format, setting metric to time
      mutate(w = time, d = time) %>%
      select(from_link, to_link, w, d) %>%
      
      #pass each O-D pair to the dodgr_distances function
      dodgr_distances(
        from = ods$from_link,
        to   = ods$to_link,
        pairwise = TRUE
      ) %>%
      
      #vector results, need to bind to the trip_id and name the value
      bind_cols(select(ods,trip_id)) %>%
      rename(time = ...1) %>%
      select(trip_id,time)
    
    #return data frame with distance and time of each trip
    left_join(distances,times, by = join_by("trip_id"))

  }
  
  # ------------------------------------------------------------
  # Return a data frame of walk, bike and car distance and time for each trip
  # ------------------------------------------------------------
  
  message("Computing walk attributes...")
  
  walk_results <- get_trip_attributes(
      origins$walk,
      destinations$walk,
      dual_networks$walk
      ) %>%
    rename(
      walk_distance = distance,
      walk_time = time
      )
  
  message("Computing bike attributes...") 
  
  bike_results <- map(
      time_periods,
      ~ get_trip_attributes(
        origins$bike[[.x]],
        destinations$bike[[.x]],
        dual_networks$bike[[.x]]
        )
      ) %>%
      set_names(time_periods) %>%
      bind_rows(.id = "period") %>%
      rename(
        bike_distance = distance,
        bike_time = time
      ) %>%
      select(!period)
  
  message("Computing car attributes...") 
    
  car_results <- map(
    time_periods,
    ~ get_trip_attributes(
      origins$car[[.x]],
      destinations$car[[.x]],
      dual_networks$car[[.x]]
      )
    )%>%
    set_names(time_periods) %>%
    bind_rows(.id = "period") %>%
    rename(
      car_distance = distance,
      car_time = time
    ) %>%
    select(!period)
  
  walk_results %>%
    left_join(bike_results, by = join_by(trip_id)) %>%
    left_join(car_results, by = join_by(trip_id))
  
}
