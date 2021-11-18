# user-facing functions

#' Extract nodes and edges from R code
#'
#' @param path_to_file character: A file path string
#' @param collapse_nodes logical: If TRUE, any dependencies on "mutate" nodes
#' will be set to their parent "non-mutate" nodes.
#'
#' @return Returns a list of two dataframes ("nodes" and "edges")
#' @export
get_flow_data <- function(path_to_file, collapse_nodes = TRUE) {
    exprs <- parse_script(path_to_file)
    nodes <- parse_nodes(exprs)
    edges <- get_dependencies(nodes)
    nodes <- add_node_type(nodes, edges)
    if (collapse_nodes) {
        edges <- cascade_depends(edges, nodes[nodes[["type"]] == "mutate", "node_id"])
    }
    edges <- cascade_depends(edges, nodes[nodes[["type"]] == "function", "node_id"])
    list("nodes" = nodes, "edges" = edges)
}

#' Convert nodes/edges into a dotfile format for dataflow graph
#'
#' @param flow_data list of 2 dataframes returned by \code{\link{get_flow_data}}
#' @param collapse_nodes logical: If TRUE, collapse "mutate" nodes into their
#' parent non-mutate nodes.
#'
#' @return Returns a string in a dotfile-compatible format
#' @export
make_dot <- function(flow_data, collapse_nodes = TRUE) {
    f <- flow_data
    f[["edges"]] <- drop_function_edges(f[["edges"]], f[["nodes"]])
    if (collapse_nodes) {
        f[["nodes"]] <- collapse_across_nodes(f[["nodes"]], f[["edges"]])
        f[["edges"]] <-  dplyr::semi_join(f[["edges"]], f[["nodes"]], by = "node_id")
    } else {
        f[["nodes"]] <- f[["nodes"]][, c("node_id", "assign", "effect", "text")]
    }
    n <- add_dot_attributes(f[["nodes"]], f[["edges"]])
    n <- make_dot_nodes(n)
    e <- make_dot_edges(f[["edges"]])
    paste("digraph {", n, e, "}", sep = "\n\n")
}

#' Plot dataflow graph from R code
#'
#' @inheritParams get_flow_data
#'
#' @return Returns a data flow diagram rendered by \code{\link[DiagrammeR]{grViz}}
#' @export
plot_flow <- function(path_to_file, collapse_nodes = TRUE) {
    f <- get_flow_data(path_to_file, collapse_nodes)
    dot <- make_dot(f, collapse_nodes)
    DiagrammeR::grViz(dot)
}
