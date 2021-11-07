# convert nodes/edges into plots

# Prepare edges dataframe for plotting
# - x: dataframe returned by parse_edges()
enrich_edges <- function(x) {
    x$assign <- paste(x$assign, x$node_id, sep = "_")
    x$effect <- paste(x$effect, x$node_id, sep = "_")
    x$dependency <- paste(x$dependency, x$node_id_dependency, sep = "_")
    dependency2 <- dplyr::distinct(x, .data[["assign"]], .data[["effect"]])
    names(dependency2) <- c("dependency", "dependency_effect")
    dplyr::left_join(x, dependency2, by = "dependency") |>
        dplyr::arrange(.data[["node_id"]], .data[["node_id_dependency"]])
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
    # TODO: this might not be what we want for labelling numbered assignments
    # (since all numbers will be stripped)
    df[["label"]] <- gsub("_[0-9].*", "", df[["name"]])
    df <- merge(df, nodes[, c("node_id", "text")], by = "node_id")
    df[["text"]] <- gsub('\"', '&quot;', df[["text"]])
    
    paste0(
        df[["name"]], " [shape=", df[["shape"]], 
        ", style=filled, fillcolor=", df[["fillcolor"]],  
        ", label=", df[["label"]], 
        ", tooltip='", df[["text"]], "']"
    )
}
