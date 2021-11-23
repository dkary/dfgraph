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
#' @param prune_labels character: If not NULL, any nodes with labels matching
#' those specified will be dropped (although their dependencies will be cascaded).
#'
#' @return Returns a dataframe with two columns (to and from) that each hold node IDs
#' @export
get_edges <- function(nodes, prune_labels = NULL) {
    edges <- get_depends(nodes)
    prune_node_edges(edges, nodes, prune_labels)
}

#' Convert nodes/edges into a dotfile format for dataflow graph
#'
#' @param nodes dataframe with one row per diagram node
#' @param edges dataframe with on row per diagram edge
#' @param label_option character: Either "both" (default), "assign", "function" or
#' "auto" (which uses "assign" for input nodes and "function" for others).
#' @param exclude_text logical: If TRUE, code for a node will not be available
#' on hover
#'
#' @return Returns a string in a dotfile-compatible format
#' @export
make_dot <- function(nodes, edges, label_option = "both", exclude_text = FALSE) {
    n <- add_dot_attributes(nodes, edges, label_option)
    n <- make_dot_nodes(n, exclude_text)
    e <- make_dot_edges(edges)
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
    path_to_file, ignore_source = NULL, prune_labels = NULL,
    label_option = "both", exclude_text = FALSE
) {
    nodes <- get_nodes(path_to_file, ignore_source)
    edges <- get_edges(nodes, prune_labels)
    dot <- make_dot(nodes, edges, label_option, exclude_text)
    DiagrammeR::grViz(dot)
}
