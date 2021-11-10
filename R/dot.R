# functions for defining dot specification

# Identify dot attributes for every node
# name, shape, fillcolor, label, tooltips
# input is square, others are circle
get_dot_attributes <- function(nodes, edges) {
    if (length(unique(nodes$node_id)) == nrow(nodes)) {
        n <- nodes[, c("node_id", "assign", "effect", "text")]
    } else {
        n <- collapse_across_nodes(nodes)
    }
    n <- n[is.na(n[["effect"]]) | n[["effect"]] != "function", ]
    e <- dplyr::distinct(edges, node_id) |>
        dplyr::mutate(has_dependency = TRUE)
    x <- dplyr::left_join(n, e, by = "node_id")
    x[["name"]] <- paste0("n", x[["node_id"]])
    x[["shape"]] <- ifelse(is.na(x[["has_dependency"]]), "box", "circle")
    x[["fillcolor"]] <- ifelse(x[["shape"]] == "box", "'#cce0ff'", "'#f9ffe6'")
    x[["label"]] <- ifelse(
        x[["shape"]] == "box" | is.na(x[["effect"]]), 
        x[["assign"]], x[["effect"]]
    )
    x[["text"]] <- gsub('\"', '&quot;', x[["text"]])
    paste0(
        x[["name"]], " [shape=", x[["shape"]], 
        ", style=filled, fillcolor=", x[["fillcolor"]],  
        ", label=", x[["label"]], 
        ", tooltip='", x[["text"]], "']"
    )
}

# Identify dot edge to/from
get_dot_edges <- function(edges) {
    from <- paste0("n", edges[["node_id_dependency"]])
    to <- paste0("n", edges[["node_id"]])
    paste(from, "->", to)
}
