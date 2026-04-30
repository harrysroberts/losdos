#' Computes the distance and time matrix for each mode and time period
#'
#' This acts as a wrapped for the dodgr::dodgr_distances() function that
#' takes the dual network together with pairs of origins and destinations and
#' returns a data frame containing the computed distance and time matrix of each 
#' mode and time period.
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
#' @return A data frame containing comprising the origin and destination ids 
#' together with the computed distance and time of each mode as a separate column
#'
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import dodgr
compute_matrix_attributes <- function(origins,destinations,dual_networks) {
  
  # ------------------------------------------------------------
  # Define time periods present in the dual networks
  # ------------------------------------------------------------
  
  time_periods <- names(dual_networks$bike)
  
  # ------------------------------------------------------------
  # Get distance and time attributes for trips in each mode/time period
  # ------------------------------------------------------------
  
  get_matrix_attributes <- function(o,d,network){
    
    #Build O-D pairs with link keys
    o <- o %>%
      mutate(from_link = str_c(from_node, ">", to_node)) %>%
      select(id, from_link)

    d <- d %>%
      mutate(to_link = str_c(from_node, ">", to_node)) %>%
      select(id, to_link)

    distances <- network %>%
      
      #place into dodgr edge list format
      mutate(w = time, d = distance) %>%
      select(from_link, to_link, w, d) %>%
      
      #pass each O-D pair to the dodgr_distances function
      dodgr_distances(
        from = o$from_link,
        to   = d$to_link,
        pairwise = FALSE
        ) %>%
      
      #matrix results, need to convert back to edge list format
        as_tibble(rownames = "origin") %>%
        pivot_longer(-origin, names_to = "destination", values_to = "distance") %>%
      
      #extract the original origin and destination IDs from the link keys
      mutate(
        origin = str_extract(origin, "(?<=origin)[^>]+"),
        destination = str_extract(destination, "(?<=destination)[^>]+")
      )
    
    times <- network %>%
      
      #place into dodgr edge list format, setting metric to time
      mutate(w = time, d = time) %>%
      select(from_link, to_link, w, d) %>%
      
      #pass each O-D pair to the dodgr_distances function
      dodgr_distances(
        from = o$from_link,
        to   = d$to_link,
        pairwise = FALSE
      ) %>%
      
      #matrix results, need to convert back to edge list format
      as_tibble(rownames = "origin") %>%
      pivot_longer(-origin, names_to = "destination", values_to = "time") %>%
      
      #extract the original origin and destination IDs from the link keys
      mutate(
        origin = str_extract(origin, "(?<=origin)[^>]+"),
        destination = str_extract(destination, "(?<=destination)[^>]+")
      )

    #return data frame with distance and time of each trip
    left_join(distances,times, by = join_by("origin","destination"))

  }
  
  # ------------------------------------------------------------
  # Return a data frame of walk, bike and car distance and time for each trip
  # ------------------------------------------------------------
  
  message("Computing walk attributes...")
  
  walk_results <- get_matrix_attributes(
      origins$walk,
      destinations$walk,
      dual_networks$walk
      ) %>%
    rename(
      walk_distance = distance,
      walk_time = time
      ) %>%
    cross_join(
      data.frame(period = time_periods)
    )
  
  message("Computing bike attributes...") 
  
  bike_results <- map(
      time_periods,
      ~ get_matrix_attributes(
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
      )
  
  message("Computing car attributes...") 
    
  car_results <- map(
    time_periods,
    ~ get_matrix_attributes(
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
    )
    
  walk_results %>%
    left_join(bike_results, by = join_by("origin","destination","period")) %>%
    left_join(car_results, by = join_by("origin","destination","period"))
  
}
