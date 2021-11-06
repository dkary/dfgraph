# convert nodes/edges into plots

# Prepare edges dataframe for plotting
# - x: dataframe returned by parse_edges()
enrich_edges <- function(x) {
    x$assign <- paste(x$assign, x$node_id, sep = "_")
    x$effect <- paste(x$effect, x$node_id, sep = "_")
    x$dependency <- paste(x$dependency, x$node_id_dependency, sep = "_")
    dependency2 <- dplyr::distinct(x, .data[["assign"]], .data[["effect"]])
    names(dependency2) <- c("dependency", "dependency_effect")
    x <- dplyr::left_join(x, dependency2, by = "dependency")
    x$from <- ifelse(is.na(x$dependency_effect), x$dependency, x$dependency_effect)
    x$relate <- paste(x$from, "->", x$effect)
    x
}

get_styles <- function(x) {
    box <- data.frame(
        node = unique(x[is.na(x$dependency_effect), c("dependency")]),
        shape = "box", fillcolor = "'#cce0ff'"
    )
    circle <- data.frame(
        node = unique(x$effect), shape = "circle", fillcolor = "'#f9ffe6'"
    )
    df <- dplyr::bind_rows(box, circle)
    df$label <- gsub("_[0-9].*", "", df$node)
    paste0(df$node, " [shape=", df$shape, ", style=filled, fillcolor=", df$fillcolor, 
           ", label=", df$label, "]"
    )
}

# Convert edges dataframe into a dotfile format for dataflow graph
# - x: dataframe returned by enrich_edges()
edges_to_dot <- function(x) {
    styles <- get_styles(x)
    paste(
        "digraph {", 
        paste(styles, collapse = " "),
        paste(x$relate, collapse = " "), "}"
    )
}

plot_flow <- function(path_to_file) {
    exprs <- parse_script(path_to_file)
    nodes <- parse_nodes(exprs)
    edges <- parse_edges(nodes) |> enrich_edges()
    dot <- edges_to_dot(edges)
    DiagrammeR::grViz(dot)
}
