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
    list("nodes" = nodes, "edges" = edges)
}

#' Prepare flow (nodes, edges) for graphing
#'
#' @param flow list of nodes/edges returned by \code{\link{get_flow}}
#' @param focus_node numeric: ID of a node to focus upon. Only the dependencies
#' of the specified node will be shown.
#' @param prune_labels character: If not NULL, any nodes with labels matching
#' those specified will be pruned (i.e., excluded from the plot) although their 
#' dependencies will be cascaded.
#' @param prune_all_functions logical: If TRUE, assigned function nodes will be 
#' pruned from the plot.
#' @param prune_all_mutates logical: If TRUE, intermediate mutated nodes (those
#' with only a self-dependency) will pruned from the plot.
#' @param label_option character: Either "both", "assign", "function" or
#' "auto" (which uses "assign" for input nodes and "function" for others).
#' @param hover_code character: Either "node", "network", or NULL. If "node", code
#' for the current node will be displayed on hover, all dependent code for
#' "network", and none if NULL.
#'
#' @return Returns a modified list of nodes/edges
#' @export
prep_flow <- function(
    flow, focus_node = NULL,
    prune_labels = NULL, prune_all_functions = TRUE, prune_all_mutates = FALSE,
    label_option = "auto", hover_code = "node"
) {
    # 1. identify prune IDs
    pruned_ids <- get_pruned_ids(
        flow[["nodes"]], prune_labels, prune_all_functions, prune_all_mutates
    )
    if (!is.null(focus_node)) {
        ids <- get_network(focus_node, flow[["edges"]])
        pruned_ids <- c(pruned_ids, setdiff(flow[["nodes"]][["id"]], ids))
    }
    # 2. add node attributes (label, tooltip)
    nodes <- add_dot_label(flow[["nodes"]], label_option)
    nodes <- add_hover_code(nodes, flow[["edges"]], pruned_ids, hover_code )
    
    # 3. prune nodes/edges
    edges_pruned <- prune_node_edges(flow[["edges"]], pruned_ids)
    nodes_pruned <- nodes[
        nodes[["id"]] %in% c(edges_pruned[["to"]], edges_pruned[["from"]]), 
    ]
    list("nodes" = nodes_pruned, "edges" = edges_pruned)
}

#' Convert nodes/edges into a dotfile format for dataflow graph
#'
#' @inheritParams prep_flow
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
#' @inheritParams prep_flow
#' @param interactive logical: If TRUE, will use \code{\link[visNetwork]{visNetwork}}
#' for interactivity, otherwise \code{\link[DiagrammeR]{grViz}}
#'
#' @return Returns a rendered data flow graph
#' @export
plot_flow <- function(
    path_to_file, focus_node = NULL, interactive = TRUE,
    prune_labels = NULL, prune_all_functions = TRUE, prune_all_mutates = FALSE,
    label_option = "auto", hover_code = "node", ignore_source = NULL
) {
    flow <- get_flow(path_to_file, ignore_source)
    flow <- prep_flow(
        flow, focus_node,
        prune_labels, prune_all_functions, prune_all_mutates,
        label_option, hover_code
    )
    if (interactive) {
        plot_visjs(flow)
    } else {
        make_dot(flow) |> DiagrammeR::grViz()
    }
}
