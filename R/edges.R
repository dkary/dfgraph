# functions for defining diagram edges

# Get a relation table between node IDs and dependency node IDs
# I think this is inherently unvectorizable because it's position-based (due
#  to the possiblity of reassignment)
get_dependency_crosswalk <- function(dependencies, nodes) {
    n <- nodes
    d <- dependencies
    d[["node_id_dependency"]] <- NA # initialize
    get_dependency_id <- function(d, n, i) {
        x <- d[i, ]
        dependency <- d[i, "dependency"]
        if (length(unique(n[["node_id"]])) == nrow(n)) {
            last_ref_node <- x[["node_id"]] - 1
        } else {
            last_ref_node <- x[["node_id"]]
        }
        ref_ids <- n[!is.na(n[["assign"]]) 
                     & n[["assign"]] == dependency 
                     & n[["node_id"]] <= last_ref_node, ]
        max(ref_ids[["node_id"]])
    }
    for (i in 1:nrow(d)) {
        d[["node_id_dependency"]][i] <- get_dependency_id(d, n, i)
    }
    dplyr::select(d, node_id, node_id_dependency) |> 
        dplyr::distinct() |> 
        dplyr::filter(node_id != node_id_dependency)
}
