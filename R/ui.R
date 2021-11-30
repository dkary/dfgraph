# user-facing functions

#' Extract nodes and edges from R code
#'
#' @param path_to_file character: A file path string
#' @param ignore_source character: If not NULL, any file names matching those
#' specified will be ignored in any "source(filename)" within the scripts.
#'
#' @return Returns a list with 2 dataframes (a) nodes with columns id, assign, 
#' member, function, code, and (b) edges with columns to and from
#' @export
get_flow <- function(path_to_file, ignore_source = NULL) {
    exprs <- parse_script(path_to_file, ignore_source)
    nodes <- parse_nodes(exprs)
    edges <- get_depends(nodes)
    nodes <- add_node_type(nodes, edges)
    rownames(nodes) <- NULL
    list("nodes" = nodes, "edges" = edges)
}

#' Identify nodes to prune
#' @param flow list of nodes/edges returned by \code{\link{get_flow}}
#' @param focus_node numeric: ID of a node to focus upon. Only the dependencies
#' of the specified node will be shown.
#' @param prune_labels character: If not NULL, any nodes with labels matching
#' those specified will be pruned (i.e., excluded from the plot) although their 
#' dependencies will be cascaded.
#' @param prune_types character: Optionally prune nodes based on type (typically
#' "function" and/or "mutate"). Use NULL to override the default of "function".
#'
#' @return Returns list of nodes/edges and (optionally) prune_ids
#' @export
add_prune_ids <- function(
    flow, focus_node = NULL, prune_labels = NULL, prune_types = "function"
) {
    prune_ids <- c(
        get_pruned_types(flow[["nodes"]], prune_types),
        get_pruned_labels(flow[["nodes"]], prune_labels)
    )
    if (!is.null(focus_node)) {
        ids <- get_network(focus_node, flow[["edges"]])
        prune_ids <- c(prune_ids, setdiff(flow[["nodes"]][["id"]], ids))
    }
    if (!all(is.na(prune_ids))) {
        flow[["prune_ids"]] <- prune_ids
    }
    flow
}

#' Prepare flow (nodes, edges) for graphing
#'
#' @inheritParams add_prune_ids
#' @param label_option character: Either "both", "assign", "function" or
#' "auto" (which uses "assign" for input nodes and "function" for others).
#' @param hover_code character: Either "node", "network", or NULL. If "node", code
#' for the current node will be displayed on hover, all dependent code for
#' "network", and none if NULL.
#'
#' @return Returns a modified list of nodes/edges
#' @export
add_graph_detail <- function(flow, label_option = "auto", hover_code = "node") {
    nodes <- add_node_label(flow[["nodes"]], label_option)
    nodes <- add_node_color(nodes)
    nodes <- add_node_hover(nodes, flow[["edges"]], flow[["prune_ids"]], hover_code)
    
    if (!is.null(flow[["prune_ids"]])) {
        flow[["edges"]] <- prune_node_edges(flow[["edges"]], flow[["prune_ids"]])
    }
    # nodes without edges will always be dropped
    nodes <- nodes[
        nodes[["id"]] %in% c(flow[["edges"]][["to"]], flow[["edges"]][["from"]]), 
    ]
    flow[["nodes"]] <- nodes
    flow
}

#' Convert nodes/edges into a dotfile format for dataflow graph
#'
#' @inheritParams add_prune_ids
#'
#' @return Returns a string in a dotfile-compatible format
#' @export
make_dot <- function(flow) {
    n <- make_dot_nodes(flow[["nodes"]])
    e <- make_dot_edges(flow[["edges"]])
    paste("digraph {", n, e, "}", sep = "\n\n")
}

#' Plot dataflow graph from R code
#'
#' @inheritParams get_flow
#' @inheritParams add_prune_ids
#' @inheritParams add_graph_detail
#' @param interactive logical: If TRUE, will use \code{\link[visNetwork]{visNetwork}}
#' for interactivity, otherwise \code{\link[DiagrammeR]{grViz}}
#'
#' @return Returns a rendered data flow graph
#' @export
graph <- function(
    path_to_file, ignore_source = NULL, interactive = TRUE,
    focus_node = NULL, prune_labels = NULL, prune_types = "function",
    label_option = "auto", hover_code = "node"
) {
    flow <- get_flow(path_to_file, ignore_source)
    flow <- add_prune_ids(flow, focus_node, prune_labels, prune_types)
    flow <- add_graph_detail(flow, label_option, hover_code)
    dot <- make_dot(flow)
    if (interactive) {
        prep_visjs(flow) |> graph_visjs()
    } else {
        make_dot(flow) |> DiagrammeR::grViz()
    }
}
