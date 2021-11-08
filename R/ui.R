# user-facing functions

# Extract nodes from R code into a dataframe
get_nodes <- function(path_to_file) {
    exprs <- parse_script(path_to_file)
    nodes <- parse_nodes(exprs)
    rownames(nodes) <- NULL
    nodes
}

# Get edges from nodes
get_edges <- function(nodes) {
    edges <- parse_edges(nodes)
    edges <- enrich_edges(edges)
    rownames(edges) <- NULL
    edges
}

# Convert nodes/edges into a dotfile format for dataflow graph
get_dot <- function(nodes, edges) {
    edges[["from"]] <- ifelse(
        is.na(edges[["dependency_effect"]]), 
        edges[["dependency"]], 
        edges[["dependency_effect"]]
    )
    edges[["effect"]] <- ifelse(
        # For membership assignments (there's probably a better way to do this)
        is.na(edges[["effect"]]), edges[["assign"]], edges[["effect"]]
    )
    edges[["dot"]] <- paste(edges[["from"]], "->", edges[["effect"]])
    attributes <- get_node_dot_attributes(nodes, edges)
    paste(
        "digraph {", 
        paste(attributes, collapse = " "), 
        paste(edges[["dot"]], collapse = " "), 
        "}"
    )
}

# Plot dataflow graph from R code
plot_flow <- function(path_to_file) {
    nodes <- get_nodes(path_to_file)
    edges <- get_edges(nodes)
    dot <- get_dot(nodes, edges)
    DiagrammeR::grViz(dot)
}
