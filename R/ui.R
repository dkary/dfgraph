# user-facing functions

# Extract nodes and edges from R code into a list of 2 dataframes
get_flow_data <- function(path_to_file, collapse_nodes = TRUE) {
    exprs <- parse_script(path_to_file)
    n <- parse_nodes(exprs)
    e <- get_dependencies(n)
    n <- add_node_type(n, e)
    f <- list("nodes" = n, "edges" = e)
    if (collapse_nodes) {
        f <- recode_nodes(f[["nodes"]], f[["edges"]], type = "mutate")
    }
    # propagate assigned function dependencies
    # (i.e., remove assigned functions from dependency chain)
    f <- recode_nodes(f[["nodes"]], f[["edges"]], type = "function")
    f[["edges"]] <- drop_function_edges(f[["edges"]], f[["nodes"]])
    f
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
    f <- get_flow_data(path_to_file, collapse_nodes)
    dot <- make_dot(f[["nodes"]], f[["edges"]])
    DiagrammeR::grViz(dot)
}
