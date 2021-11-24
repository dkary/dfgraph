# functions for defining dot specification

# Determine display type of node
# For example: input, function, mutate, interim, terminal)
# To be called from add_dot_attributes()
add_node_type <- function(nodes, edges) {
    if (nrow(edges) == 0) {
        stop("The graph has no edges, so there is nothing to see here!", call. = FALSE)
    }
    # prepare dataframe
    nodes_to_include <- unique(c(edges[["to"]], edges[["from"]]))
    n <- nodes[nodes[["id"]] %in% nodes_to_include, ]
    e <- dplyr::distinct(edges, "id" = .data[["to"]]) |>
        dplyr::mutate(has_dependency = TRUE)
    x <- dplyr::left_join(n, e, by = "id")
    x[["node_type"]] <- ifelse(
        is.na(x[["has_dependency"]]), "input", 
        ifelse(is.na(x[["assign"]]), "terminal", "interim")
    )
    x
}

# Identify node labels depending on specified option
# To be called from add_dot_attributes()
get_dot_label <- function(
    assign_label, member_label, function_label, node_type, label_option
) {
    assign_label[is.na(assign_label)] <- ""
    function_label[is.na(function_label)] <- ""
    assign_label <- ifelse(
        is.na(member_label), assign_label,
        paste0(assign_label, "$", member_label)
    )
    if (label_option == "assign") {
        assign_label
    } else if (label_option == "function") {
        function_label
    } else if (label_option == "auto") {
        ifelse(
            node_type == "input" | function_label == "", 
            assign_label, function_label
        )
    } else {
        # Graphviz "record" shapes use a pipe to divide subsections of a node
        paste(assign_label, "|", function_label)
    }
}

# Add columns necessary for the dot specification to the nodes dataframe
# TODO: seems like this function might be doing too much
add_dot_attributes <- function(nodes, edges, pruned_nodes, label_option = "both") {
    x <- add_node_type(nodes, edges)
    x[["name"]] <- paste0("n", x[["id"]])
    x[["label"]] <- get_dot_label(
        x[["assign"]], x[["member"]], x[["function"]], x[["node_type"]], 
        label_option
    )
    x[["code"]] <- paste0("# Node ", x[["id"]], "\n", x[["code"]])
    x[["code"]] <- gsub('\"', '&quot;', x[["code"]])
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
                x$name, " [label='", x$label, "', tooltip='", x$code, "']", 
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
    from <- paste0("n", edges[["from"]])
    to <- paste0("n", edges[["to"]])
    e <- paste(from, "->", to)
    paste(e, collapse = " ")
}
