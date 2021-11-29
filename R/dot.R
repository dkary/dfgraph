# functions for defining dot specification

# Make dot code for nodes
make_dot_nodes <- function(nodes) {
    x <- nodes
    by_node <- paste0(
        x[["id"]], 
        " [label='", x[["label"]], 
        "', tooltip='", x[["hover"]], 
        "', fillcolor='", x[["color"]], 
        "']", 
        collapse = "\n"
    )
    paste0(
        "node [shape=record, style='rounded, filled']",  
        "\n", by_node, "\n"
    )
}

# Make dot code for edges
make_dot_edges <- function(edges) {
    e <- paste(edges[["from"]], "->", edges[["to"]])
    paste(e, collapse = " ")
}
