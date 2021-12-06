# functions for working on the flow (nodes, edges) list structure
# i.e., they take both nodes and edges as input

# Determine display type of node (function, input, mutate, assemble, or terminal)
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
    
    nodes[["type"]] <- ifelse(
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
add_node_hover <- function(nodes, edges, prune_ids = NULL, hover_code = "node") {
    # NOTE: this function will only work correctly if nodes is sorted by ID
    # Not great, but seems okay at this stage
    nodes <- nodes[order(nodes[["id"]]),]
    rownames(nodes) <- NULL
    
    function_ids <- nodes[
        !is.na(nodes[["function"]]) & nodes[["function"]] == "function", "id"
    ]
    if (is.null(hover_code)) {
        x <- paste0("# Node", nodes[["id"]])
    } else if (is.null(prune_ids) && hover_code != "network") {
        # In this case every node shows only it's own code
        x <- paste0("# Node ", nodes[["id"]], "\n", nodes[["code"]])
    } else {
        x <- sapply(1:nrow(nodes), function(node) {
            if (hover_code == "node") {
                ids <- get_network(node, edges, prune_ids)
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
# To be called from add_node_label()
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
            node_type %in% c("input", "function") | function_label == "", 
            assign_label, function_label
        )
    } else {
        # Graphviz "record" shapes use a pipe to divide subsections of a node
        paste(assign_label, "|", function_label)
    }
}

# Add label column to nodes, which will ultimately be displayed as the node name
add_node_label <- function(nodes, label_option = "auto") {
    x <- nodes
    x[["label"]] <- get_dot_label(
        x[["assign"]], x[["member"]], x[["function"]], x[["type"]], 
        label_option
    )
    x
}

# Define a 4-color palette
# colors for (1) function (2) input (3) interim (4) terminal
get_color_palette <- function(use_colorbrewer = FALSE) {
    if (use_colorbrewer) {
        # A color-blind safe colorbrewer qualitative palette, but with a higher 
        # white-level: regular = 71% 41% 77% 40%, lighter = 90% 80% 95% 75%
        colors <- c("#e4f5d6", "#a7d3f1", "#ecf4f9", "#8dde87")
    } else {
        # A palette that is more pleasing to my eye, but still has alternating 
        # shading, so probably not too bad for colorblindness:
        # 90% 85% 95% 70%
        colors <- c("#ffffcc", "#b3d1ff", "#f9ffe6", "#ffc266")
    }
    data.frame(
        "type" = c("function", "input", "mutate", "assemble", "terminal"),
        "color" = c(colors[1:3], colors[3], colors[4])
    )
}

# Add a color column to nodes
add_node_color <- function(nodes) {
    colors <- get_color_palette()
    x <- merge(nodes, colors, by = "type", all.x = TRUE)
    x <- x[order(x[["id"]]),]
    rownames(x) <- NULL
    x
}
