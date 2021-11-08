# convert nodes/edges into plots

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

# Identify node attributes for the dot specification
# - also pull in nodes to display text on hover
get_node_dot_attributes <- function(nodes, edges) {
    # NOTE: This is clunky. Probably eventually pull all this from nodes instead.
    input <- edges[is.na(edges[["dependency_effect"]]), c("node_id_dependency", "dependency")]
    names(input) <- c("node_id", "name")
    input[["shape"]] <- "box"
    input[["fillcolor"]] <- "'#cce0ff'"
    
    mutate <- edges[, c("node_id", "effect")]
    names(mutate) <- c("node_id", "name")
    mutate <- mutate[!duplicated(mutate),]
    mutate[["shape"]] <- "circle"
    mutate[["fillcolor"]] <- "'#f9ffe6'"
    
    df <- rbind(input, mutate)
    df[["label"]] <- gsub("n[0-9]*_", "", df[["name"]])
    df <- merge(df, nodes[, c("node_id", "text")], by = "node_id")
    df[["text"]] <- gsub('\"', '&quot;', df[["text"]])
    
    paste0(
        df[["name"]], " [shape=", df[["shape"]], 
        ", style=filled, fillcolor=", df[["fillcolor"]],  
        ", label=", df[["label"]], 
        ", tooltip='", df[["text"]], "']"
    )
}
