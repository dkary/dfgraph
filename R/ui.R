# user-facing functions

# Extract nodes from R code into a dataframe
get_nodes <- function(path_to_file, collapse_nodes = TRUE) {
    exprs <- parse_script(path_to_file)
    nodes <- parse_nodes(exprs)
    # TODO: silly that this gets called twice, clean it up
    dependencies <- get_dependencies(nodes)
    nodes <- add_node_type(nodes, dependencies)
    if (collapse_nodes) {
        nodes <- recode_node_ids(nodes, dependencies)
    }
    rownames(nodes) <- NULL
    nodes
}

# Get edges from nodes
get_edges <- function(nodes) {
    # TODO: maybe enrich edges here
    get_dependencies(nodes)
}

# Convert nodes/edges into a dotfile format for dataflow graph
make_dot <- function(nodes, edges) {
    nodes <- add_dot_attributes(nodes, edges)
    n <- make_dot_nodes(nodes)
    e <- make_dot_edges(edges)
    paste("digraph {", n, e, "}", sep = "\n\n")
}

# Plot dataflow graph from R code
plot_flow <- function(path_to_file, collapse_nodes = TRUE) {
    nodes <- get_nodes(path_to_file, collapse_nodes)
    edges <- get_edges(nodes)
    dot <- make_dot(nodes, edges)
    DiagrammeR::grViz(dot)
}
