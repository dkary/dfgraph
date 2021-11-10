# user-facing functions

# Extract nodes from R code into a dataframe
get_nodes <- function(path_to_file, collapse_nodes = TRUE) {
    exprs <- parse_script(path_to_file)
    nodes <- parse_nodes(exprs)
    dependencies <- get_dependencies(nodes)
    nodes <- add_dependencies(nodes, dependencies)
    nodes <- add_node_type(nodes, dependencies)
    if (collapse_nodes) {
        nodes <- recode_node_ids(nodes, dependencies)
    }
    rownames(nodes) <- NULL
    nodes
}

# Get edges from nodes
get_edges <- function(nodes) {
    d <- get_dependencies(nodes) |> dplyr::distinct()
    get_dependency_crosswalk(d, nodes) 
}

# Convert nodes/edges into a dotfile format for dataflow graph
get_dot <- function(nodes, edges) {
    a <- get_dot_attributes(nodes, edges)
    e <- get_dot_edges(edges)
    paste(
        "digraph {", 
        paste(a, collapse = " "), 
        paste(e, collapse = " "), 
        "}"
    )
}

# Plot dataflow graph from R code
plot_flow <- function(path_to_file, collapse_nodes = TRUE) {
    nodes <- get_nodes(path_to_file, collapse_nodes)
    edges <- get_edges(nodes)
    dot <- get_dot(nodes, edges)
    DiagrammeR::grViz(dot)
}
