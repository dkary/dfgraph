# functions for defining dot specification

# Determine display type of node
# To be called from add_dot_attributes()
add_node_type <- function(nodes, edges) {
    if (nrow(edges) == 0) {
        stop("The graph has no edges, so there is nothing to see here!", call. = FALSE)
    }
    function_ids <- nodes[!is.na(nodes[["function"]]) & nodes[["function"]] == "function", "id"]
    edges_pruned <- prune_node_edges(edges, function_ids)
    non_mutate_ids <- unique(edges_pruned[duplicated(edges_pruned$to), "to"])
    mutate_ids <- edges_pruned[!edges_pruned[["to"]] %in% non_mutate_ids, "to"]
    
    nodes[["node_type"]] <- ifelse(
        nodes[["id"]] %in% function_ids, "function",
        ifelse(
            is.na(nodes[["assign"]]), "terminal",
            ifelse(
                !nodes[["id"]] %in% edges_pruned[["to"]], "input",
                ifelse(
                    nodes[["id"]] %in% mutate_ids, "mutate", "assemble"
                )
            )
        )
    )
    nodes
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

# Get 4-color palette depending on choice
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
        # c("#ffffcc", "#b3d1ff", "#e6f0ff", "#ffc266") # blue in middle?
    }
}

# Make dot code for edges
make_dot_edges <- function(edges) {
    from <- paste0("n", edges[["from"]])
    to <- paste0("n", edges[["to"]])
    e <- paste(from, "->", to)
    paste(e, collapse = " ")
}
