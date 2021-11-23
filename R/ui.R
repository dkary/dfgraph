# user-facing functions

#' Extract nodes and edges from R code
#'
#' @param path_to_file character: A file path string
#' @param collapse_nodes logical: If TRUE, any dependencies on "mutate" nodes
#' will be set to their parent "non-mutate" nodes.
#' @param prune_labels character: If not NULL, any nodes with labels matching
#' those specified will be dropped (although their dependencies will be cascaded).
#' @param ignore_source character: If not NULL, any file names matching those
#' specified will be ignored in any "source(filename)" within the scripts.
#'
#' @return Returns a list of two dataframes ("nodes" and "edges")
#' @export
get_flow_data <- function(
    path_to_file, collapse_nodes = FALSE, prune_labels = NULL, ignore_source = NULL
) {
    exprs <- parse_script(path_to_file, ignore_source)
    nodes <- parse_nodes(exprs)
    edges <- get_dependencies(nodes)
    edges <- prune_node_edges(edges, nodes, prune_labels)
    list("nodes" = nodes, "edges" = edges)
}

#' Convert nodes/edges into a dotfile format for dataflow graph
#'
#' @param flow_data list of 2 dataframes returned by \code{\link{get_flow_data}}
#' @param collapse_nodes logical: If TRUE, collapse "mutate" nodes into their
#' parent non-mutate nodes.
#' @param exclude_text logical: If TRUE, code for a node will not be available
#' on hover
#' @param label_option character: Either "both" (default), "assign", "effect" or
#' "auto" (which uses "assign" for input nodes and "effect" for others).
#'
#' @return Returns a string in a dotfile-compatible format
#' @export
make_dot <- function(
    flow_data, collapse_nodes = FALSE, exclude_text = FALSE, 
    label_option = "both"
) {
    f <- flow_data
    f[["nodes"]] <- f[["nodes"]][, c("node_id", "assign", "effect", "text")]
    n <- add_dot_attributes(f[["nodes"]], f[["edges"]], label_option)
    n <- make_dot_nodes(n, exclude_text)
    e <- make_dot_edges(f[["edges"]])
    paste("digraph {", n, e, "}", sep = "\n\n")
}

#' Plot dataflow graph from R code
#'
#' @inheritParams get_flow_data
#' @inheritParams make_dot
#'
#' @return Returns a data flow diagram rendered by \code{\link[DiagrammeR]{grViz}}
#' @export
plot_flow <- function(
    path_to_file, collapse_nodes = FALSE, exclude_text = FALSE,
    label_option = "both", prune_labels = NULL, ignore_source = NULL
) {
    f <- get_flow_data(path_to_file, collapse_nodes, prune_labels, ignore_source)
    dot <- make_dot(f, collapse_nodes, exclude_text, label_option)
    DiagrammeR::grViz(dot)
}
