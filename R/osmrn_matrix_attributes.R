#' Compute OSMRN matrix of attributes (walk/bike/car distance & time) via dodgr
#'
#' @description
#' **OS-specific wrapper** that builds mode × period networks (including 
#' slope-aware walk and bike models, and time-period car speeds), constructs
#' dual graphs with OS turn restrictions, matches origins and destinations to 
#' the nearest nodes, and computes **minimum-time** routes with `dodgr`, 
#' returning per-mode **distance (metres)** and **time (minutes)** for each 
#' origin-destination pair in each specified time period.
#'
#' Distances and times are both measured **along minimum-time paths** (i.e., the
#' graph is weighted by time for routing; distance is summed along the same path).
#'
#' @param origins A `data.frame`/`tibble` with required columns:
#'   `id`, `easting`, `northing`.
#'   CRS for coordinates must be **EPSG:27700** (metres).
#'   
#' @param destinations A `data.frame`/`tibble` with required columns:
#'   `id`, `easting`, `northing`.
#'   CRS for coordinates must be **EPSG:27700** (metres).

#' @param periods Optional character vector of time period identifiers. Defaults 
#' to all supported OSMRN periods.
#'   
#' @param walk_speed Numeric. Assumed walk speed in kilometres per hour. 
#'    Defaults to 4.824 km/h as used by MatSim.
#'    
#' @param bike_speed Numeric. Assumed bike speed in kilometres per hour. 
#'    Defaults to 21.636 km/h as used by MatSim.
#'
#' @return A list of origin-destination pairs with additional columns for 
#' computed distance and time by each of walk, bike and car modes
#'
#' @examples
#' \dontrun{
#'  # Requires OS MRN data files in input/raw/ directory
#'
#'  origins <- data.frame(
#'    id = c(1, 2, 3),
#'    easting = c(429180, 427750, 435741),
#'    northing = c(434731, 435747, 432124)
#'  )
#'
#'  destinations <- data.frame(
#'    id = c(1, 2, 3),
#'    easting = c(429906, 430454, 430731),
#'    northing = c(433271, 433532, 441858)
#'  )
#'
#'  results <- osmrn_matrix_attributes(
#'    origins,
#'    destinations,
#'    periods = c("MoFr09001200", "SaSu14001900")
#'  )
#'
#' }
#'
#' @import dplyr
#'
#' @export
osmrn_matrix_attributes <- function(
    origins,
    destinations,
    periods = c(
      "MoFr04000700", "MoFr07000900", "MoFr09001200", "MoFr12001400",
      "MoFr14001600", "MoFr16001900", "MoFr19002200", "MoFr22000400",
      "SaSu04000700", "SaSu07001000", "SaSu10001400", "SaSu14001900",
      "SaSu19002200", "SaSu22000400"),
    walk_speed = 4.824,
    bike_speed = 21.636
) {
  # ----------------------------
  # 0) Basic validation
  # ----------------------------
  if (!is.data.frame(origins)|!is.data.frame(destinations)) {
    stop("`origins` and `destinations` must be data.frames or tibbles.", call. = FALSE)
  }
  
  if (!is.numeric(walk_speed)|!is.numeric(bike_speed)) {
    stop("`walk_speed` and `bike_speed` must be numeric", call. = FALSE)
  }
  
  required_cols <- c(
    "id", "easting", "northing"
  )
  
  missing_cols <- setdiff(required_cols, names(origins))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in `origins`: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  missing_cols <- setdiff(required_cols, names(destinations))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in `destinations`: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # ----------------------------
  # 1) Load OS MRN datasets and process if necessary
  # ----------------------------
  if (all(file.exists(
    file.path(
      "input/processed/",
      c("links.gpkg","nodes.gpkg","turn_restrictions.gpkg")
    )
  ))) {
    message("Using cached OSMRN network. \nTo update the network, please remove the 'links', 'nodes' and 'turn_restrictions' GeoPackage files from the input/processed/ directory.")
  } else if (all(file.exists(
    file.path(
      "input/raw/",
      c("boundary.gpkg","osmrn.gpkg")
    )
  ))) {
    message("Processing OS MRN network...")
    dir.create("input/processed", recursive = TRUE)
    process_osmrn()
  } else {
    message("Error: Please add the osmrn.gpkg and boundary.gpkg files to the input directory.")
  }
  
  # ----------------------------
  # 2) Create base network
  # ----------------------------
  
  message("Creating base network...")
  base_network <- create_base_network(
    walk_speed = walk_speed,
    bike_speed = bike_speed
  )
  
  # ----------------------------
  # 3) Generate modal networks for the periods present in the trips dataset
  # ----------------------------
  
  message("Generating modal networks for periods present in trips ...")
  
  modal_networks <- generate_modal_networks(
    base_network,
    periods = periods
  )
  
  # ----------------------------
  # 4) Extract links connecting origins and destinations to/from each network
  # ----------------------------
  
  message("Extracting links connecting origins and destinations to each network")
  
  origin_links <- generate_origin_links(
    cross_join(
      origins,
      data.frame(period = periods)
    ),
    modal_networks,
    walk_speed
  )
  
  destination_links <- generate_destination_links(
    cross_join(
      destinations,
      data.frame(period = periods)
    ),
    modal_networks,
    walk_speed
  )
  
  # ----------------------------
  # 5) Augmenting networks with origin and destination links
  # ----------------------------
  
  message("Appending origin and destination links to each network")
  
  augmented_networks <- generate_augmented_networks(
    modal_networks,
    origin_links,
    destination_links
  )
  
  # ----------------------------
  # 6) Dual the network such that links become nodes and remove restricted turns
  # ----------------------------
  
  message("Creating dual representations of each network")
  dual_networks <- generate_dual_networks(augmented_networks)
  
  # ----------------------------
  # 7) Compute distance and time matrices for each mode and time period
  # ----------------------------
  
  message("Computing distance and time for each mode")
  
  results <- compute_matrix_attributes(
    origin_links,
    destination_links,
    dual_networks
  )
  
  # ----------------------------
  # 8) Return the results joined to the trip database
  # ----------------------------
  
  message("Preparing output...")
  
  origins %>%
    cross_join(destinations) %>%
    mutate(
      origin = as.character(id.x),
      destination = as.character(id.y),
      origin_easting = easting.x,
      origin_northing = northing.x,
      destination_easting = easting.y,
      destination_northing = northing.y
    ) %>%
    cross_join(
      data.frame(period = periods)
    ) %>%
    select(
      origin,
      destination,
      period,
      origin_easting,
      origin_northing,
      destination_easting,
      destination_northing
    ) %>%
    left_join(
      results,
      by = join_by(origin, destination, period)
    )
  
  
}