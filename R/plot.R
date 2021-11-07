# convert nodes/edges into plots

# Prepare edges dataframe for plotting
# - x: dataframe returned by parse_edges()
enrich_edges <- function(x) {
    x$assign <- paste(x$assign, x$node_id, sep = "_")
    x$effect <- paste(x$effect, x$node_id, sep = "_")
    x$dependency <- paste(x$dependency, x$node_id_dependency, sep = "_")
    dependency2 <- dplyr::distinct(x, .data[["assign"]], .data[["effect"]])
    names(dependency2) <- c("dependency", "dependency_effect")
    dplyr::left_join(x, dependency2, by = "dependency") |>
        dplyr::arrange(.data[["node_id"]], .data[["node_id_dependency"]])
}

# TODO: 
# - rename this to get_attributes(nodes, edges)
# - also pull in nodes to display text on hover
get_styles <- function(x) {
    box <- data.frame(
        node = unique(x[is.na(x[["dependency_effect"]]), c("dependency")]),
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
