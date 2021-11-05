# convert nodes/edges into plots

# Convert edges dataframe into a dotfile format for dataflow graph
# - x: dataframe returned by parse_edges()
edges_to_dot <- function(x) {
    x$from <- paste0(x$dependency, x$node_id_dependency)
    x$to <- paste0(x$target, x$node_id)
    x$relate <- paste(x$from, "->", x$to)
    paste("dag{", paste(x$relate, collapse = " "), "}")
}

plot_flow <- function(path_to_file) {
    exprs <- parse_script(path_to_file)
    nodes <- parse_nodes(exprs)
    edges <- parse_edges(nodes)
    # plot 
    dot <- edges_to_dot(edges)
    dag <- dagitty::dagitty(dot)
    tidy_dag <- ggdag::tidy_dagitty(dag)
    ggdag::ggdag(tidy_dag)
}
