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
#' @param make_cache Logical. If `TRUE`, the generated modal networks 
#' will be saved to disk as an RDS file for future reuse. Defaults to `FALSE`.
#'
#' @param use_cache Logical. If `TRUE`, the function will attempt to 
#' load pre-generated modal networks from disk (if they exist) instead of 
#' regenerating them. Defaults to `TRUE`.
#'
#' @return The `trips` database with additional columns for computed distance
#' and time by each of walk, bike and car modes




#'
#' @examples
#' \dontrun{
#'  # Requires OSMRN data files in input/raw/ directory
#'  trips <- data.frame(
#'    id = c(1, 2, 3),
#'    period = c("MoFr09001200", "MoFr19002200", "SaSu14001900"),
#'    from_easting = c(429180, 427750, 435741),
#'    from_northing = c(434731, 435747, 432124),
#'    to_easting = c(429906, 430454, 430731),
#'    to_northing = c(433271, 433532, 441858)
#'  )
#'  
#'  results <- osmrn_trip_attributes(trips)
#' }
#'
#' @import dplyr
#' @import sf
#'
#' @export
osmrn_trip_attributes <- function(
    trips,
    walk_speed = 4.824,
    bike_speed = 21.636,
    make_cache = FALSE,
    use_cache = TRUE,
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
    message("Using pre-processed OSMRN network. \nTo update the network, please remove the 'links', 'nodes' and 'turn_restrictions' GeoPackage files from the input/processed/ directory.")
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
  
  links <- st_read("input/processed/links.gpkg", quiet = TRUE)
  nodes <- st_read("input/processed/nodes.gpkg", quiet = TRUE)
  turn_restrictions <- st_read("input/processed/turn_restrictions.gpkg", quiet = TRUE)
  
  # ----------------------------
  # 2) Create or retrieve modal networks
  # ----------------------------

  if (use_cache && all(file.exists(
    file.path(
      "input/processed/",
      c("modal_networks.rds")
      )
    ))) {

    message("Using cached modal networks. \nTo update the networks, please remove the 'modal_networks.rds' file from the input/processed/ directory.")
    
    modal_networks <- readRDS("input/processed/modal_networks.rds")
  
  } else {

    message("Creating base network...")
    base_network <- create_base_network(
      walk_speed = walk_speed,
      bike_speed = bike_speed,
      links = links,
      nodes = nodes
    )
    
    message("Generating modal networks for periods present in trips ...")
    
    modal_networks <- generate_modal_networks(
      base_network,
      periods = trip_periods
    )
    
    if (make_cache) {
      saveRDS(modal_networks, "input/processed/modal_networks.rds")
    }
  }
  
  # ----------------------------
  # 3) Extract links connecting origins and destinations to/from each network
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
  # 4) Augmenting networks with origin and destination links
  # ----------------------------
  
  message("Appending origin and destination links to each network")
  
  augmented_networks <- generate_augmented_networks(
    modal_networks,
    origin_links,
    destination_links
  )
  
  # ----------------------------
  # 5) Dual the network such that links become nodes and remove restricted turns
  # ----------------------------
  
  message("Creating dual representations of each network")
  dual_networks <- generate_dual_networks(
    augmented_networks,
    links,
    nodes,
    turn_restrictions
    )
  
  # ----------------------------
  # 6) Compute distance and time by each mode for each trip
  # ----------------------------
  
  message("Computing distance and time for each mode")
  
  results <- compute_trip_attributes(
    origin_links,
    destination_links,
    dual_networks
  )
  
  # ----------------------------
  # 7) Return the results joined to the trip database
  # ----------------------------
  
  message("Preparing output...")
  
  left_join(
    trips,
    results,
    by = join_by(id)
  )
  
}
