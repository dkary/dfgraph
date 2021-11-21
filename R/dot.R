# functions for defining dot specification

add_dot_attributes <- function(nodes, edges, minimal_label = FALSE) {
    # prepare dataframe
    nodes_to_include <- unique(c(edges[["node_id"]], edges[["node_id_dependency"]]))
    n <- nodes[nodes[["node_id"]] %in% nodes_to_include, ]
    e <- dplyr::distinct(edges, .data[["node_id"]]) |>
        dplyr::mutate(has_dependency = TRUE)
    x <- dplyr::left_join(n, e, by = "node_id")
    x[["effect"]] <- ifelse(is.na(x[["effect"]]), "", x[["effect"]])
    x[["assign"]] <- ifelse(is.na(x[["assign"]]), "", x[["assign"]])
    
    # define attributes
    x[["shape"]] <- ifelse(is.na(x[["has_dependency"]]), "input", "other")
    x[["name"]] <- paste0("n", x[["node_id"]])
    x[["label"]] <- paste(x[["assign"]], "|", x[["effect"]])
    if (minimal_label) {
        x[["label"]] <- ifelse(
            x[["shape"]] == "input" | x[["effect"]] == "", 
            x[["assign"]], x[["effect"]]
        )
    }
    x[["text"]] <- paste0("# Node ", x[["node_id"]], "\n", x[["text"]])
    x[["text"]] <- gsub('\"', '&quot;', x[["text"]])
    x
}

# Make dot code for nodes
make_dot_nodes <- function(nodes, exclude_text = FALSE) {
    x <- split(nodes, nodes[["shape"]]) # splitting by groups for dot subgraphs
    assemble_attributes <- function(x) {
        if (exclude_text) {
            paste0(x$name, " [label='", x$label, "']", collapse = "\n")
        } else {
            paste0(
                x$name, " [label='", x$label, "', tooltip='", x$text, "']", 
                collapse = "\n"
            )
        }
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
    paste(
        assemble_subgraph(x, "input", "#cce0ff"),
        assemble_subgraph(x, "other", "#f9ffe6"),
        sep = "\n\n"
    )
}

# Make dot code for edges
make_dot_edges <- function(edges) {
    from <- paste0("n", edges[["node_id_dependency"]])
    to <- paste0("n", edges[["node_id"]])
    e <- paste(from, "->", to)
    paste(e, collapse = " ")
}
