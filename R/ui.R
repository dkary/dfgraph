# user-facing functions

# Extract nodes from R code into a dataframe
get_nodes <- function(path_to_file) {
    exprs <- parse_script("stirfry.R")
    parse_nodes(exprs) 
}

# At least for now, wrap the existing edges functions
# although I may want do some refactoring
get_edges <- function(nodes) {
    edges <- parse_edges(nodes)
    enrich_edges(edges)
}

# TODO: pull in nodes$text for hover
# Convert nodes/edges into a dotfile format for dataflow graph
get_dot <- function(edges) {
    edges$from <- ifelse(
        is.na(edges$dependency_effect), edges$dependency, edges$dependency_effect
    )
    edges$dot <- paste(edges$from, "->", edges$effect)
    styles <- get_styles(edges)
    paste(
        "digraph {", 
        paste(styles, collapse = " "),
        paste(edges$dot, collapse = " "), "}"
    )
}

plot_flow <- function(path_to_file) {
    nodes <- get_nodes(path_to_file)
    edges <- get_edges(nodes)
    dot <- get_dot(edges)
    DiagrammeR::grViz(dot)
}
