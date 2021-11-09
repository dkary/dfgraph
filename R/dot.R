# functions for defining dot specification

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
