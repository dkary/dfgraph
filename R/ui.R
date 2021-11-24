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
#' @param prune_labels character: If not NULL, any nodes with labels matching
#' those specified will be dropped (although their dependencies will be cascaded).
#' @param prune_all_functions logical: If TRUE, assinged functions will be pruned 
#' from the plot
#' @param label_option character: Either "both" (default), "assign", "function" or
#' "auto" (which uses "assign" for input nodes and "function" for others).
#' @param exclude_text logical: If TRUE, code for a node will not be available
#' on hover
#'
#' @return Returns a string in a dotfile-compatible format
#' @export
make_dot <- function(
    nodes, edges, prune_labels = NULL, prune_all_functions = TRUE, 
    label_option = "both", exclude_text = FALSE
) {
    pruned_nodes <- get_pruned_nodes(nodes, prune_labels, prune_all_functions)
    n <- add_dot_attributes(nodes, edges, pruned_nodes, label_option)
    e <- prune_node_edges(edges, pruned_nodes)
    n <- n[n[["id"]] %in% c(e[["to"]], e[["from"]]), ]
    n <- make_dot_nodes(n, exclude_text)
    e <- make_dot_edges(e)
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
    prune_all_functions = TRUE, label_option = "both", exclude_text = FALSE
) {
    nodes <- get_nodes(path_to_file, ignore_source)
    edges <- get_edges(nodes)
    dot <- make_dot(
        nodes, edges, prune_labels, prune_all_functions, 
        label_option, exclude_text
    )
    DiagrammeR::grViz(dot)
}
