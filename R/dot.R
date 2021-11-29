# functions for defining dot specification

# Make dot code for nodes
make_dot_nodes <- function(nodes) {
    x <- split(nodes, nodes[["node_type"]]) # splitting by groups for dot subgraphs
    assemble_attributes <- function(x) {
        paste0(
            x$name, " [label='", x$label, "', tooltip='", x$hover, "']", 
            collapse = "\n"
        )
    }
    assemble_subgraph <- function(x, shape, fillcolor) {
        paste0(
            "subgraph ", shape, " {", "\n",
            "node [shape=record, style='rounded, filled', fillcolor='", 
            fillcolor, "']", "\n", 
            assemble_attributes(x[[shape]]), "\n", 
            "}"
        )
    }
    colors <- get_color_palette()
    paste(
        assemble_subgraph(x, "function", colors[1]),
        assemble_subgraph(x, "input", colors[2]),
        assemble_subgraph(x, "mutate", colors[3]),
        assemble_subgraph(x, "assemble", colors[3]),
        assemble_subgraph(x, "terminal", colors[4]),
        sep = "\n\n"
    )
}

# Define a 4-color palette
# colors for (1) function (2) input (3) interim (4) terminal
get_color_palette <- function(use_colorbrewer = FALSE) {
    if (use_colorbrewer) {
        # A color-blind safe colorbrewer qualitative palette, but with a higher 
        # white-level: regular = 71% 41% 77% 40%, lighter = 90% 80% 95% 75%
        c("#e4f5d6", "#a7d3f1", "#ecf4f9", "#8dde87")
    } else {
        # A palette that is more pleasing to my eye, but still has alternating 
        # shading, so probably not too bad for colorblindness:
        # 90% 85% 95% 70%
        c("#ffffcc", "#b3d1ff", "#f9ffe6", "#ffc266")
    }
}

# Make dot code for edges
make_dot_edges <- function(edges) {
    from <- paste0("n", edges[["from"]])
    to <- paste0("n", edges[["to"]])
    e <- paste(from, "->", to)
    paste(e, collapse = " ")
}
