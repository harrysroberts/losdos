#' Internal: process raw Ordnance Survey MRN into cleaned objects
#'
#' This function:
#' - loads `input/boundary.gpkg`
#' - buffers, transforms, unions -> WKT filter
#' - loads raw OS MRN layers from "input/osmrn.gpkg"
#' - expands conditional car speeds into named avgspeed.forward. and avgspeed.backward.
#' - filters nodes to only those referenced by the links
#' - saves links, nodes and turn restrictions to "input/processed" directory
#'
#' @keywords internal
#'
#' @import sf
#' @import dplyr
#' @import tidyr
#' @import stringr
process_osmrn <- function() {
  
  # ------------------------------------------------------------
  # 1) Boundary → buffered → WGS84 → union → WKT
  # ------------------------------------------------------------
  boundary <- read_sf("input/raw/boundary.gpkg") %>%
    st_transform(4326) %>%
    st_union() %>%
    st_as_text()
  
  # ------------------------------------------------------------
  # 2) Process links
  # ------------------------------------------------------------
  
  #Load link data from MRN and filter by boundary
  links <- st_read(
    dsn   = "input/raw/osmrn.gpkg",
    layer = "mrn_ntwk_transportlink",
    wkt_filter = boundary
    ) %>%
    st_transform(27700) %>%
    
    #Expand conditional forward speeds to columns
    separate_rows(`avgspeed.forward.conditional`, sep = ";\\s*") %>%
    mutate(
      forward_speed = as.numeric(
        str_extract(
          `avgspeed.forward.conditional`, "^[0-9.]+"
          )
        )*1000/3600,
      colname = str_c(
        "avgspeed.forward.",
        str_replace_all(
          str_extract(
            `avgspeed.forward.conditional`,
            "(?<=@ \\().+?(?=\\))"
            ),
          "[:\\-() ]", ""
          )
        )
    ) %>%
    select(-`avgspeed.forward.conditional`) %>%
    pivot_wider(names_from = colname, values_from = forward_speed) %>%
    select(-`NA`) %>%
    
    #Expand conditional backward speeds to columns
    separate_rows(`avgspeed.backward.conditional`, sep = ";\\s*") %>%
    mutate(
      backward_speed = as.numeric(
        str_extract(
          `avgspeed.backward.conditional`,
          "^[0-9.]+"
          )
        )*1000/3600,
      colname = str_c(
        "avgspeed.backward.",
        str_replace_all(
          str_extract(
            `avgspeed.backward.conditional`,
            "(?<=@ \\().+?(?=\\))"
            ),
          "[:\\-() ]", ""
          )
        )
    ) %>%
    select(-`avgspeed.backward.conditional`) %>%
    pivot_wider(names_from = colname, values_from=backward_speed) %>%
    select(-`NA`) %>%
    #write to processed input file
    st_write("input/processed/links.gpkg")
  
  # ------------------------------------------------------------
  # 3) Process nodes
  # ------------------------------------------------------------
  
  #Load node data from MRN and filter by boundary
  nodes <- st_read(
    "input/raw/osmrn.gpkg",
    layer="mrn_ntwk_transportnode",
    wkt_filter = boundary
    ) %>%
    st_transform(27700) %>%
    
    #Only retain nodes that form the start or end of links in the link set
    filter(
      os_parentid %in% unique(
        c(links$os_startnode,links$os_endnode)
        )
      ) %>%
    
    #write to processed input file
    st_write("input/processed/nodes.gpkg")
  
  # ------------------------------------------------------------
  # 4) Process turn restrictions (no filtering by links)
  # ------------------------------------------------------------
  
  #Load turn restriction data from MRN and filter by boundary
  turn_restrictions <- st_read(
    "input/raw/osmrn.gpkg",
    layer="mrn_ntwk_turnrestriction",
    wkt_filter = boundary) %>%
    st_transform(27700) %>%
    st_write("input/processed/turn_restrictions.gpkg")
  
}
