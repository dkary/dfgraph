# user-facing functions

#' Extract nodes from R code
#'
#' @param path_to_file character: A file path string
#' @param ignore_source character: If not NULL, any file names matching those
#' specified will be ignored in any "source(filename)" within the scripts.
#'
#' @return Returns a dataframe with columns id, assign, member, function, code
#' @export
get_nodes <- function(path_to_file, ignore_source = NULL) {
    exprs <- parse_script(path_to_file, ignore_source)
    parse_nodes(exprs)
}

#' Extract edges from nodes
#'
#' @param nodes dataframe returned by \code{\link{get_nodes}}
#'
#' @return Returns a dataframe with two columns (to and from) that each hold node IDs
#' @export
get_edges <- function(nodes) {
    get_depends(nodes)
}

#' Convert nodes/edges into a dotfile format for dataflow graph
#'
#' @param nodes dataframe with one row per diagram node
#' @param edges dataframe with on row per diagram edge
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
#' @param hover_code logical: If TRUE, code (for a node) will be displayed on 
#' hover. Code for pruned nodes will be displayed as part of the hovered text 
#' for their immediate downstream nodes.
#'
#' @return Returns a string in a dotfile-compatible format
#' @export
make_dot <- function(
    nodes, edges, focus_node = NULL,
    prune_labels = NULL, prune_all_functions = TRUE, prune_all_mutates = FALSE,
    label_option = "auto", hover_code = TRUE
) {
    nodes <- add_node_type(nodes, edges)
    pruned_ids <- get_pruned_ids(
        nodes, prune_labels, prune_all_functions, prune_all_mutates
    )
    if (!is.null(focus_node)) {
        ids <- get_network(focus_node, edges)
        pruned_ids <- c(pruned_ids, setdiff(nodes[["id"]], ids))
    }
    nodes <- add_dot_attributes(nodes, edges, pruned_ids, label_option)
    edges_pruned <- prune_node_edges(edges, pruned_ids)
    nodes_pruned <- nodes[
        nodes[["id"]] %in% c(edges_pruned[["to"]], edges_pruned[["from"]]), 
    ]
    n <- make_dot_nodes(nodes_pruned, hover_code)
    e <- make_dot_edges(edges_pruned)
    paste("digraph {", n, e, "}", sep = "\n\n")
}

#' Plot dataflow graph from R code
#'
#' @inheritParams get_nodes
#' @inheritParams get_edges
#' @inheritParams make_dot
#'
#' @return Returns a data flow diagram rendered by \code{\link[DiagrammeR]{grViz}}
#' @export
plot_flow <- function(
    path_to_file, focus_node = NULL,
    prune_labels = NULL, prune_all_functions = TRUE, prune_all_mutates = FALSE,
    label_option = "auto", hover_code = TRUE, ignore_source = NULL
) {
    nodes <- get_nodes(path_to_file, ignore_source)
    edges <- get_edges(nodes)
    dot <- make_dot(
        nodes, edges, focus_node,
        prune_labels, prune_all_functions, prune_all_mutates,
        label_option, hover_code
    )
    DiagrammeR::grViz(dot)
}
