#' Build base directed network with walk/bike/car times (OS-MRN)
#'
#' Constructs a **directional base network** from OS MRN links with:
#' - `from_node`, `to_node`
#' - `distance` (metres)
#' - `walk` (minutes; Weidmann model via `get_walk_time()`)
#' - `bike_<period>` (minutes; Parkin–Rotheram via `get_bike_time()`)
#' - `car_<period>` (minutes; from `avgspeed.<dir>.<period>`, m/s)
#'
#' Walk is set to `NA` where access prohibits walking (`access == "no"` or `foot == "no"`).
#' Bike time is set to `NA` where cycling is not allowed.
#'
#' The function expects the links to contain, at minimum:
#' - Geometry column `geom` (LINESTRING), CRS **EPSG:27700**
#' - Node IDs: `os_startnode`, `os_endnode`
#' - Slopes: `ascent.forward`, `ascent.backward`
#' - Access flags: `access`, `foot`, `bicycle`, `bicycle.forward`, `bicycle.backward`
#' - Car speeds (m/s): `avgspeed.forward.<PERIOD>`, `avgspeed.backward.<PERIOD>`
#'
#' @param walk_speed Numeric. Assumed walk speed in kilometres per hour. 
#'    Defaults to 4.824 km/h as used by MatSim.
#' @param bike_speed Numeric. Assumed bike speed in kilometres per hour. 
#'    Defaults to 21.636 km/h as used by MatSim.
#'       
#' @return A data frame (geometry dropped) with columns:
#'   `from_node`, `to_node`, `distance`, `walk`, and per‑period `bike_*`, `car_*`.
#'
#' @details
#' - Distance is computed with `st_length(geom)` (metres).
#' - Times are returned in **minutes**.
#' - Both **forward** and **backward** directions are included and row‑bound.
#' - This function relies on `get_walk_time()` and `get_bike_time()` being available
#'   in the package namespace.
#'
#' @keywords internal
#'
#' @import sf
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @import stringr
create_base_network <- function(walk_speed, bike_speed) {
  
  # ------------------------------------------------------------
  # Process inputs
  # ------------------------------------------------------------
  
  walk_speed <- as.numeric(walk_speed)*1000/60 #convert to m/min
  bike_speed <- as.numeric(bike_speed)*1000/60 #convert to m/min
  
  # ------------------------------------------------------------
  # Load network links and join with node coordinates extracted from
  # ------------------------------------------------------------
  
  links <- read_sf("input/processed/links.gpkg")
  nodes <- read_sf("input/processed/nodes.gpkg")
  
  # ------------------------------------------------------------
  # Attach start and end node geometries
  # ------------------------------------------------------------
  
  links_with_node_geometries <- links %>%
    mutate(link_length = as.numeric(st_length(geom))) %>%
    st_drop_geometry() %>%
    left_join(
      select(nodes,os_parentid),
      by=join_by("os_startnode"=="os_parentid")
      ) %>%
    mutate(
      start_easting = st_coordinates(geom)[,1],
      start_northing = st_coordinates(geom)[,2]
      ) %>%
    select(!geom) %>%
    left_join(
      select(nodes,os_parentid),
      by=join_by("os_endnode"=="os_parentid")
    ) %>%
    mutate(
      end_easting = st_coordinates(geom)[,1],
      end_northing = st_coordinates(geom)[,2]
    ) %>%
    select(!geom) 
  
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
  # Define slope factors
  # ------------------------------------------------------------
  
  #slope factors taken from MatSim GitHub repository
  #values for slopes above 0.7 removed due to lack of realism
  slope_factors <- tibble(
    slope = seq(0.7, -0.4, by=-0.01),
    factor = c(
      0.0776, 0.0870, 0.0966, 0.1064, 0.1163, 0.1264, 0.1365, 0.1468, 0.1572,
      0.1677, 0.1782, 0.1888, 0.1996, 0.2104, 0.2212, 0.2322, 0.2432, 0.2544, 
      0.2656, 0.2770, 0.2884, 0.3000, 0.3117, 0.3236, 0.3356, 0.3477, 0.3600, 
      0.3725, 0.3852, 0.3981, 0.4112, 0.4245, 0.4380, 0.4518, 0.4658, 0.4800, 
      0.4944, 0.5091, 0.5241, 0.5392, 0.5546, 0.5703, 0.5861, 0.6022, 0.6185,
      0.6349, 0.6516, 0.6683, 0.6852, 0.7023, 0.7194, 0.7365, 0.7537, 0.7708, 
      0.7879, 0.8048, 0.8216, 0.8382, 0.8546, 0.8706, 0.8862, 0.9013, 0.9160, 
      0.9300, 0.9433, 0.9558, 0.9675, 0.9782, 0.9878, 0.9963, 1.0000, 1.0055, 
      1.0108, 1.0163, 1.0219, 1.0273, 1.0325, 1.0372, 1.0413, 1.0448, 1.0474,
      1.0491, 1.0497, 1.0494, 1.0478, 1.0451, 1.0412, 1.0361, 1.0297, 1.0221,
      1.0133, 1.0033, 0.9922, 0.9801, 0.9670, 0.9530, 0.9382, 0.9227, 0.9067,
      0.8903, 0.8737, 0.8570, 0.8405, 0.8242, 0.8085, 0.7935, 0.7795, 0.7667, 
      0.7555, 0.7460, 0.7386
      )
    )
  
  # ------------------------------------------------------------
  # Vectorised function to compute walk times
  # ------------------------------------------------------------
  
  get_walk_time <- function(distance,ascent,descent){
    
    #Compute slope and max-location along link
    s <- (ascent + descent) / distance
    m <- if_else(s != 0, ascent / s, 0)
    
    #Find nearest uphill slope value
    uphill_factor <- map_dbl(s, ~ {
      slope_factors$factor[which.min(abs(slope_factors$slope - .x))]
    })
    
    #Find nearest downhill slope value
    downhill_factor <- map_dbl(s, ~ {
      slope_factors$factor[which.min(abs(slope_factors$slope + .x))]
    })
    
    #Compute uphill and downhill speeds in m/min
    #Based on Horni et al. (2016, p.137) model used by MatSim
    uphill_speed <- walk_speed * uphill_factor
    downhill_speed <- walk_speed * downhill_factor
    
    # Return total time in minutes
    m/uphill_speed + (distance - m)/downhill_speed
    
  }
  
  # ------------------------------------------------------------
  # Vectorised function to compute bike times
  # ------------------------------------------------------------
  
  get_bike_time <- function(distance,ascent,descent,car_speed){
    
    #Convert car speed from m/s to m/min
    car_speed <- as.numeric(car_speed)*60
    
    #Compute slope and max-location along link
    s <- (ascent + descent) / distance
    m <- if_else(s != 0, ascent / s, 0)
    
    
    #Find nearest uphill slope value
    uphill_factor <- map_dbl(s, ~ {
      slope_factors$factor[which.min(abs(slope_factors$slope - .x))]
    })
    
    #Compute uphill speeds in m/min
    #Capped below at the walk speed, capped above at the car_speed
    #Based on Horni et al. (2016, p.140) model used by MatSim
    uphill_speed <- pmin(
      pmax(
        bike_speed - 40.02 * 60 * s,
        walk_speed * uphill_factor),
      car_speed,
      na.rm=TRUE
    )
    
    #Compute downhill speeds in m/min
    #Capped above by the lower of car speed and 35km/h
    #Based on Horni et al. (2016, p.140) model used by MatSim
    downhill_speed <- pmin(
      bike_speed + 23.79 * 60 * s,
      pmin(
        35*1000/60,
        car_speed,
        na.rm=TRUE
      )
    )
    
    #Return total time in minutes
    m/uphill_speed + (distance - m)/downhill_speed
    
  }
  
  # ------------------------------------------------------------
  # Extract attribute values for links in forward direction
  # ------------------------------------------------------------

  forward_network <- links_with_node_geometries %>%
    mutate(
      from_node = os_startnode,
      from_easting = start_easting,
      from_northing = start_northing,
      to_node   = os_endnode,
      to_easting = end_easting,
      to_northing = end_northing,
      distance  = link_length,
      ascent    = coalesce(`ascent.forward`, 0),
      descent   = coalesce(`ascent.backward`, 0), #descent is backwards ascent
      
      #compute walk times where permitted, otherwise NA
      walk = if_else(
        coalesce(access == "no", FALSE) |
          coalesce(foot == "no", FALSE),
        NA,
        get_walk_time(distance, ascent, descent)
        ),
      
      #compute bike times in each period where permitted, otherwise use walk time
      across(
        all_of(paste0("avgspeed.forward.", time_periods)),
        ~ if_else(
          coalesce(bicycle == "yes", FALSE) |
            coalesce(`bicycle.forward` == "yes", FALSE),
          get_bike_time(distance, ascent, descent, .x),
          walk
        ),
        .names = "bike_{sub('avgspeed.forward.', '', .col)}"
        ),
      
      #fetch car times in each period
      across(
        all_of(paste0("avgspeed.forward.", time_periods)),
        ~ (distance / .x) / 60,
        .names = "car_{sub('avgspeed.forward.', '', .col)}"
        )
      ) %>%
    select(from_node:last_col())

  # ------------------------------------------------------------
  # Extract attribute values for links in backward direction
  # ------------------------------------------------------------
  
  backward_network <- links_with_node_geometries %>%
    mutate(
      from_node = os_endnode, #start of reverse edge is end node
      from_easting = end_easting,
      from_northing = end_northing,
      to_node   = os_startnode, #vice-versa
      to_easting = start_easting,
      to_northing = start_northing,
      distance  = link_length, #same distance
      ascent    = coalesce(`ascent.backward`, 0),
      descent   = coalesce(`ascent.forward`, 0), #descent is backwards ascent
      
      #compute walk times where permitted, otherwise NA
      walk = if_else(
        coalesce(access == "no", FALSE) |
          coalesce(foot == "no", FALSE),
        NA,
        get_walk_time(distance, ascent, descent)
      ),
      
      #compute bike times in each period where permitted, otherwise use walk
      #as cyclists can get off bike and push where walking is permitted
      across(
        all_of(paste0("avgspeed.backward.", time_periods)),
        ~ if_else(
          coalesce(bicycle == "yes", FALSE) |
            coalesce(`bicycle.backward` == "yes", FALSE),
          get_bike_time(distance, ascent, descent, .x),
          walk
        ),
        .names = "bike_{sub('avgspeed.backward.', '', .col)}"
      ),
      
      #fetch car times in each period
      across(
        all_of(paste0("avgspeed.backward.", time_periods)),
        ~ (distance / .x) / 60,
        .names = "car_{sub('avgspeed.backward.', '', .col)}"
      )
    ) %>%
    select(from_node:last_col())

  # ------------------------------------------------------------
  # Return both directional networks bound together
  # ------------------------------------------------------------
  
  bind_rows(
      forward_network,
      backward_network
      )
  
}
