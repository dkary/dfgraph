# functions for defining diagram edges

# Pull dependency node numbers
get_dependency_nodes <- function(dependencies, nodes) {
    # prepare all possible dependency node identifiers (for joining)
    n <- nodes[!is.na(nodes[["assign"]]), c("node_id", "assign")]
    names(n) <-  c("node_id_dependency", "dependency")
    
    # merge to get possible dependency nodes
    d <- merge(dependencies, n, by = "dependency")
    
    # get closest dependency node
    d <- d[d[["node_id_dependency"]] < d[["node_id"]], ]
    out <- aggregate(
        d[["node_id_dependency"]], 
        by = list(d[["node_id"]], d[["dependency"]]), 
        FUN = "max"
    )
    # tidy up
    names(out) <- c("node_id", "dependency", "node_id_dependency")
    out <- out[order(out[["node_id"]]),]
    rownames(out) <- NULL
    out
}

# Make a dataframe that defines edges (i.e., data flow arrows)
# This is a thin wrapper for the two dependency functions
# - nodes: dataframe returned by parse_nodes()
parse_edges <- function(nodes) {
    d <- get_dependencies(nodes)
    e <- get_dependency_nodes(d, nodes)
    n <- nodes[, c("node_id", "assign", "effect")]
    out <- merge(n, e, by = "node_id")
    out[, c("node_id", "assign", "effect", "node_id_dependency", "dependency")]
}


# Prepare edges dataframe for plotting
# - x: dataframe returned by parse_edges()
enrich_edges <- function(x) {
    prepend_node_id <- function(x, var, id = "node_id") {
        # ensures every node has a unique identifier for the dotfile
        x[[var]] <- ifelse(is.na(x[[var]]), NA, paste0("n", x[[id]], "_", x[[var]]))
        x
    }
    x <- prepend_node_id(x, "assign")
    x <- prepend_node_id(x, "effect")
    x <- prepend_node_id(x, "dependency", "node_id_dependency")
    dependency2 <- x[, c("assign", "effect")]
    dependency2 <- dependency2[!duplicated(dependency2), ]
    names(dependency2) <- c("dependency", "dependency_effect")
    out <- merge(x, dependency2, by = "dependency", all.x = TRUE)
    out[order(out[["node_id"]]), 
        c("node_id", "assign", "effect", 
          "node_id_dependency", "dependency", "dependency_effect")
    ]
}
