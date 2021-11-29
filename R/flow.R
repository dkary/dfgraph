# functions for working on the flow (nodes, edges) list structure
# i.e., they take both nodes and edges as input

# Determine display type of node (function, input, mutate, assemble, or terminal)
# To be called from add_dot_attributes()
add_node_type <- function(nodes, edges) {
    if (nrow(edges) == 0) {
        stop("The graph has no edges, so there is nothing to see here!", call. = FALSE)
    }
    function_ids <- nodes[
        !is.na(nodes[["function"]]) & nodes[["function"]] == "function", "id"
    ]
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

# Add a column to nodes that shows what will be displayed on hover
# To be called from add_dot_attributes()
add_hover_code <- function(nodes, edges, pruned_ids = NULL, hover_code = "node") {
    function_ids <- nodes[
        !is.na(nodes[["function"]]) & nodes[["function"]] == "function", "id"
    ]
    if (is.null(hover_code)) {
        x <- paste0("# Node", nodes[["id"]])
    } else if (is.null(pruned_ids) && hover_code != "network") {
        # In this case every node shows only it's own code
        x <- paste0("# Node ", nodes[["id"]], "\n", nodes[["code"]])
    } else {
        x <- sapply(1:nrow(nodes), function(node) {
            if (hover_code == "node") {
                ids <- get_network(node, edges, pruned_ids)
                ids <- setdiff(ids, function_ids) # don't want to display function code
            } else {
                ids <- get_network(node, edges)
            }
            ids <- sort(ids)
            counter <- 1:length(ids)
            comments <- ifelse(
                counter == 1 & length(counter) > 1, "# Previous Nodes\n", 
                ifelse(counter == length(ids), paste("\n# Node", ids, "\n"), "")
            )
            codes <- paste0(comments, nodes[nodes[["id"]] %in% ids, "code"])
            paste(codes, collapse = "\n")
        })
    }
    nodes[["hover"]] <- gsub('\"', '&quot;', x)
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

# Add label column to nodes, which will ultimately be displayed as the node name
add_dot_label <- function(nodes, label_option = "auto") {
    x <- nodes
    x[["name"]] <- paste0("n", x[["id"]])
    x[["label"]] <- get_dot_label(
        x[["assign"]], x[["member"]], x[["function"]], x[["node_type"]], 
        label_option
    )
    x
}