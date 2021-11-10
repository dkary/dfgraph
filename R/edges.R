# functions for defining diagram edges

# Pull dependencies from parsed dataframe
# - nodes: dataframe returned by parse_nodes()
get_dependencies <- function(nodes) {
    identify_one <- function(df, assigned) {
        x <- rlang::parse_expr(df[["text"]])
        if (!is.na(df[["assign"]])) {
            df_parsed <- get_parse_data(x[[3]])
        } else {
            df_parsed <- get_parse_data(x)
        }
        out <- data.frame(
            dependency = unique(df_parsed[df_parsed[["token"]] == "SYMBOL", "text"])
        )
        if (!is.na(df[["member"]])) {
            # if membership is assigned, then there's a depedency on itself
            out <- rbind(out, data.frame(dependency = df[["assign"]]))
        }
        if (nrow(out) != 0) {
            out[["node_id"]] <- df[["node_id"]]
            out[out[["dependency"]] %in% assigned, c("node_id", "dependency")]
        }
    }
    assigned <- unique(nodes[["assign"]])
    out <- lapply(1:nrow(nodes), function(i) {
        identify_one(nodes[i,], assigned) 
    })
    do.call(rbind, out)
}

# Get a relation table between node IDs and dependency node IDs
# I think this is inherently unvectorizable because it's position-based (due
#  to the possiblity of reassignment)
get_dependency_crosswalk <- function(dependencies, nodes) {
    n <- nodes
    d <- dependencies
    d[["node_id_dependency"]] <- NA # initialize
    get_dependency_id <- function(d, n, i) {
        x <- d[i, ]
        dependency <- d[i, "dependency"]
        if (length(unique(n[["node_id"]])) == nrow(n)) {
            last_ref_node <- x[["node_id"]] - 1
        } else {
            last_ref_node <- x[["node_id"]]
        }
        ref_ids <- n[!is.na(n[["assign"]]) 
                     & n[["assign"]] == dependency 
                     & n[["node_id"]] <= last_ref_node, ]
        max(ref_ids[["node_id"]])
    }
    for (i in 1:nrow(d)) {
        d[["node_id_dependency"]][i] <- get_dependency_id(d, n, i)
    }
    dplyr::select(d, node_id, node_id_dependency) |> 
        dplyr::distinct() |> 
        dplyr::filter(node_id != node_id_dependency)
}
