# functions for defining dot specification

# Identify node labels depending on specified option
get_dot_label <- function(
    assign_label, effect_label, node_type, label_option
) {
    assign_label[is.na(assign_label)] <- ""
    effect_label[is.na(effect_label)] <- ""
    if (label_option == "assign") {
        assign_label
    } else if (label_option == "effect") {
        effect_label
    } else if (label_option == "auto") {
        ifelse(
            node_type == "input" | effect_label == "", 
            assign_label, effect_label
        )
    } else {
        # Graphviz "record" shapes use a pipe to divide subsections of a node
        paste(assign_label, "|", effect_label)
    }
}

# Add columns necessary for the dot specification to the nodes dataframe
add_dot_attributes <- function(nodes, edges, label_option = "both") {
    if (nrow(edges) == 0) {
        stop("The graph has no edges, so there is nothing to see here!", call. = FALSE)
    }
    # prepare dataframe
    nodes_to_include <- unique(c(edges[["node_id"]], edges[["node_id_dependency"]]))
    n <- nodes[nodes[["node_id"]] %in% nodes_to_include, ]
    e <- dplyr::distinct(edges, .data[["node_id"]]) |>
        dplyr::mutate(has_dependency = TRUE)
    x <- dplyr::left_join(n, e, by = "node_id")
    
    # define attributes
    x[["node_type"]] <- ifelse(
        is.na(x[["has_dependency"]]), "input", 
        ifelse(is.na(x[["assign"]]), "terminal", "interim")
    )
    x[["name"]] <- paste0("n", x[["node_id"]])
    x[["label"]] <- get_dot_label(
        x[["assign"]], x[["effect"]], x[["node_type"]], label_option
    )
    x[["text"]] <- paste0("# Node ", x[["node_id"]], "\n", x[["text"]])
    x[["text"]] <- gsub('\"', '&quot;', x[["text"]])
    x
}

# Make dot code for nodes
# Must be run AFTER add_dot_attributes()
# TODO: probably better to remove the add_dot_attributes() dependency
make_dot_nodes <- function(nodes, exclude_text = FALSE) {
    x <- split(nodes, nodes[["node_type"]]) # splitting by groups for dot subgraphs
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
        assemble_subgraph(x, "interim", "#f9ffe6"),
        assemble_subgraph(x, "terminal", "#ffcc80"),
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
