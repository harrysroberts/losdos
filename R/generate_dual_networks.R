#' Generate dual graph representations of each augmented modal network
#'
#' This converts the augmented modal networks to a dual representation, where
#' each link becomes an vertex identified by the joint ID "link1>link2", and
#' edges represent arcs corresponding to turnings between these links. The arc
#' attributes are arbitrarily taken to be those of the outgoing link. 
#'
#' @param augmented_networks A named list containing:
#'   - walk: a single augmented walk network
#'   - bike: a list of augmented bike networks for each time period
#'   - car:  a list of augmented car networks for each time period
#'   "Augmented" in this case meaning with origin and destination links attached
#'
#' @return A named list containing:
#'   - walk: a single dual walk network
#'   - bike: a list of dual bike networks for each time period
#'   - car:  a list of dual car networks for each time period
#'
#' @keywords internal
#'
#' @import sf
#' @import dplyr
#' @import purrr
#' @import stringr
generate_dual_networks <- function(augmented_networks) {
  
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
  # Import the datasets from OS MRN
  # ------------------------------------------------------------
  
  links <- st_read("input/processed/links.gpkg")
  nodes <- st_read("input/processed/nodes.gpkg")
  turn_restrictions <- st_read("input/processed/turn_restrictions.gpkg")
  
  
  # ------------------------------------------------------------
  # Convert turn restrictions into dual network format
  # ------------------------------------------------------------
  
  turn_restrictions_dual <- turn_restrictions %>%
    
    #Extract banned turns ABC expressed as from = A, to = C, via = B
    st_drop_geometry() %>%
    select(from,to,via) %>%
    mutate(via=as.integer(via)) %>%
    
    #Convert the node ids to unique OS feature IDs using the nodes and links
    #datasets. The filter removes turn restrictions where via is a link rather 
    #than a node - this may be a source of error but there is no alternative 
    #other than manual cleaning.
    left_join(
      st_drop_geometry(
        select(nodes,nodeid,os_parentid)
        ),
      by=c("via"="nodeid")
      ) %>%
    filter(!is.na(os_parentid)) %>%
    rename(via_node = os_parentid) %>%
    left_join(
      st_drop_geometry(
        select(links,wayid,os_startnode,os_endnode)
        ),
      by=c("from"="wayid")) %>%
    mutate(from_node = if_else(via_node==os_endnode,os_startnode,os_endnode))%>%
    select(!c(os_startnode,os_endnode)) %>%
    left_join(
      st_drop_geometry(
        select(links,wayid,os_startnode,os_endnode)
        ),
      by=c("to"="wayid")
      ) %>%
    mutate(to_node = if_else(via_node==os_startnode,os_endnode,os_startnode))%>%
    
    #Define ids for the incoming and outgoing link as 'from_node>via_node' and
    #via_node>to_node respectively, concatenating the node IDs with '>'.
    mutate(from_link = str_c(from_node,">",via_node),
           to_link = str_c(via_node,">",to_node))%>%
    select(from_link,to_link)
  
  # ------------------------------------------------------------
  # Helper function for dualing each augmented network
  # ------------------------------------------------------------
  
  get_dual_network <- function(network) {
    
    network %>%
      
      #create a joined table connecting each edge to all its successor edges
      #each row now represents an arc, or pair of successive edges
      inner_join(
        select(network,from_node,to_node),
        by=c("to_node"="from_node"),
        relationship = "many-to-many"
      ) %>%
      
      #label each link in the arc using the two node IDs concatenated with a ">"
      #symbol between them
      mutate(from_link = str_c(from_node,">",to_node),
             to_link = str_c(to_node,">",to_node.y)) %>%
      
      #removed any of the restricted turns using an anti-join
      anti_join(turn_restrictions_dual, by = c("from_link","to_link"))%>%
      
      #output the two link ids (now the vertices of the dual network) along with
      #the distance and time of the **first** link - the choice of whether to
      #take the first or second link attributes is arbitrary, but taking both
      #would lead to double counting
      select(from_link,to_link,distance,time)
    
    }
  
  # ------------------------------------------------------------
  # Return a list of dual networks
  # ------------------------------------------------------------
  
  list(
    
    walk = get_dual_network(augmented_networks$walk),
    
    bike = map(
      time_periods,
      ~ get_dual_network(augmented_networks$bike[[.x]])
      ) %>%
      set_names(time_periods),
    
    car = map(
      time_periods,
      ~ get_dual_network(augmented_networks$car[[.x]])
      ) %>%
      set_names(time_periods)
    
    )
  
}