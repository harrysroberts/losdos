#' Compute OSMRN trip attributes (walk/bike/car distance & time) via dodgr
#'
#' @description
#' **OS-specific wrapper** that builds mode × period networks (including 
#' slope-aware walk and bike models, and time-period car speeds), constructs
#' dual graphs with OS turn restrictions, matches trips to the nearest nodes,
#' and computes **minimum-time** routes with `dodgr`, returning per-mode
#' **distance (metres)** and **time (minutes)** appended to the input `trips`.
#'
#' Distances and times are both measured **along minimum-time paths** (i.e., the
#' graph is weighted by time for routing; distance is summed along the same path).
#'
#' @param trips A `data.frame`/`tibble` with required columns:
#'   `id`, `period`,
#'   `from_easting`, `from_northing`,
#'   `to_easting`,  `to_northing`.
#'   CRS for coordinates must be **EPSG:27700** (metres).
#'   
#' @param walk_speed Numeric. Assumed walk speed in kilometres per hour. 
#'    Defaults to 4.824 km/h as used by MatSim.
#'    
#' @param bike_speed Numeric. Assumed bike speed in kilometres per hour. 
#'    Defaults to 21.636 km/h as used by MatSim.
#'
#' @return The `trips` database with additional columns for computed distance
#' and time by each of walk, bike and car modes
#'
#' @examples
#' \dontrun{
#'  # Requires OSMRN data files in input/raw/ directory
#'  trips <- data.frame(
#'    id = c(1, 2),
#'    period = c("MoFr09001200", "MoFr09001200"),
#'    from_easting = c(432500, 432600),
#'    from_northing = c(434200, 434300),
#'    to_easting = c(432700, 432800),
#'    to_northing = c(434400, 434500)
#'  )
#'  
#'  results <- osmrn_trip_attributes(
#'    trips,
#'    walk_speed = 4.824,
#'    bike_speed = 21.636
#'  )
#' }
#'
#' @import dplyr
#'
#' @export
osmrn_trip_attributes <- function(
    trips,
    walk_speed = 4.824,
    bike_speed = 21.636
) {
  # ----------------------------
  # 0) Basic validation
  # ----------------------------
  if (!is.data.frame(trips)) {
    stop("`trips` must be a data.frame or tibble.", call. = FALSE)
  }
  
  if (!is.numeric(walk_speed)|!is.numeric(bike_speed)) {
    stop("`walk_speed` and `bike_speed` must be numeric", call. = FALSE)
  }
  
  required_cols <- c(
    "id", "period",
    "from_easting", "from_northing",
    "to_easting",  "to_northing"
  )
  
  missing_cols <- setdiff(required_cols, names(trips))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in `trips`: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  trip_periods <- sort(unique(as.character(trips$period)))
  trip_periods <- trip_periods[!is.na(trip_periods) & trip_periods != ""]
  
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
    periods = trip_periods
  )
  
  # ----------------------------
  # 4) Extract links connecting origins and destinations to/from each network
  # ----------------------------
  
  message("Extracting links connecting origins and destinations to each network")
  
  origins <- trips %>%
    select(id, period, from_easting, from_northing) %>%
    rename(
      easting = from_easting,
      northing = from_northing
    )

  origin_links <- generate_origin_links(
    origins,
    modal_networks,
    walk_speed
  )

  destinations <- trips %>%
    select(id, period, to_easting, to_northing) %>%
    rename(
      easting = to_easting,
      northing = to_northing
    )
  
  destination_links <- generate_destination_links(
    destinations,
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
  # 7) Compute distance and time by each mode for each trip
  # ----------------------------
  
  message("Computing distance and time for each mode")
  
  results <- compute_trip_attributes(
    origin_links,
    destination_links,
    dual_networks
  )
  
  # ----------------------------
  # 8) Return the results joined to the trip database
  # ----------------------------
  
  message("Preparing output...")
  
  left_join(
    trips,
    results,
    by = join_by(id)
  )
  
}
