#' Generate modal networks for walk, bike, and car
#'
#' This function extracts modal networks from `base_network` for:
#' - walk (single network)
#' - bike (one per time period)
#' - car  (one per time period)
#'
#' It simply wraps `get_modal_network()` and returns a named list
#' containing the modal networks for each mode and time-period.
#'
#' @param base_network The full network edge table containing travel times.
#' @param periods Optional character vector of periods to generate modal networks for.
#'   Defaults to all supported time periods. Only periods present in the trips
#'   dataset should be requested.
#'
#' @return A named list containing:
#'   - walk: a single modal network
#'   - bike: a list of bike networks for each time period
#'   - car:  a list of car networks for each time period
#'
#' @keywords internal
#'   
#' @import dplyr
#' @import purrr
#' @import stringr
generate_modal_networks <- function(base_network, periods = NULL) {
  
  # ------------------------------------------------------------
  # Define supported time periods
  # ------------------------------------------------------------
  
  default_periods <- c(
    "MoFr04000700", "MoFr07000900", "MoFr09001200", "MoFr12001400",
    "MoFr14001600", "MoFr16001900", "MoFr19002200", "MoFr22000400",
    "SaSu04000700", "SaSu07001000", "SaSu10001400", "SaSu14001900",
    "SaSu19002200", "SaSu22000400"
  )
  
  if (is.null(periods)) {
    time_periods <- default_periods
  } else {
    if (!is.character(periods)) {
      stop("`periods` must be a character vector of period identifiers.", call. = FALSE)
    }
    time_periods <- sort(unique(periods))
    time_periods <- time_periods[!is.na(time_periods)]
    if (length(time_periods) == 0L) {
      stop("`periods` must contain at least one valid time period.", call. = FALSE)
    }
    unsupported <- setdiff(time_periods, default_periods)
    if (length(unsupported) > 0L) {
      stop(
        sprintf(
          "Unsupported time periods: %s",
          paste(unsupported, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  
  required_columns <- c(paste0("bike_", time_periods), paste0("car_", time_periods))
  missing_cols <- setdiff(required_columns, names(base_network))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "Base network missing required time columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # Create a helper function to extract the modal networks
  # ------------------------------------------------------------
  
  get_modal_network <- function(mode, period = NULL) {
    
    mode_period <- if (is.null(period)) mode else str_c(mode, "_", period)
    
    message("Generating ", mode_period)
    
    base_network %>%
      
      #extract only the relevant columns for that time period
      transmute(
        from_node,
        to_node,
        distance,
        time = !!sym(mode_period),
        from_easting,
        from_northing,
        to_easting,
        to_northing
      ) %>%
      
      #get rid of links with no access by that mode - 
      filter(!is.na(time)) %>%
      
      #remove parallel edges (replace with minimal time)
      group_by(from_node, to_node) %>%
      slice_min(time, with_ties = FALSE) %>%
      ungroup() %>%
      
      #convert to igraph
      igraph::graph_from_data_frame() %>%
      
      #filter for only the largest component
      igraph::largest_component(mode = "strong") %>%
      
      #convert back to data frame
      igraph::as_data_frame(what = "edges") %>%
      rename(from_node = from, to_node = to)
    
  }
  
  # ------------------------------------------------------------
  # Return a list of modal networks for each mode and time period
  # ------------------------------------------------------------
  
  list(
    walk = get_modal_network("walk"),
    bike = map(time_periods, ~ get_modal_network("bike", .x)) %>% 
      set_names(time_periods),
    car = map(time_periods, ~ get_modal_network("car", .x)) %>% 
      set_names(time_periods)
  )
  
}
