# functions for defining diagram edges

# Parse dependencies (vector) for a single R statement
# The statement will be represented by an expression
parse_statement_depends <- function(expr) {
    parse_one <- function(x) {
        df <- get_parse_data(x)
        unique(df[df[["token"]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"), "text"])
    }
    if (rlang::is_call(expr, "<-")) {
        expr <- expr[[3]]
    }
    if (!rlang::is_call(expr, "function")) {
        parse_one(expr)
    } else {
        # An assigned function needs global dependencies pulled (recursively)
        # 1. identify named arguments
        f_formals <- expr[[2]] # function formals get stored in a pairlist
        locals <- names(f_formals) # function args
        # 2. identify any globals called by default argument values
        globals <- c() 
        for (i in seq_along(f_formals)) {
            if (is.name(f_formals[[i]]) & f_formals[[i]] != "") {
                globals <- c(globals, rlang::as_string(f_formals[[i]]))
            }
        }
        # 3. parse nodes from the function expression
        n <- lapply(expr, parse_expression)
        n <- do.call(rbind, n)
        if (is.null(n)) {
            return(unique(globals))
        }
        # 4. Trace locals/globals over function statements
        for (i in 1:nrow(n)) {
            x <- n[i, "text"] |> rlang::parse_expr()
            locals <- c(locals, n[i, "assign"])
            depends <- parse_statement_depends(x)
            globals <- c(globals, setdiff(depends, locals))
        }
        unique(globals)
    }
}

# Get dependencies for a single node
# - node: one row from a row dataframe
get_node_depends <- function(node) {
    x <- rlang::parse_expr(node[["text"]])
    d <- parse_statement_depends(x)
    if (!is.na(node[["member"]])) {
        # if membership is assigned, then there's also a dependency on itself
        d <- c(d, node[["assign"]])
    }
    if (length(d) > 0) {
        data.frame(node_id = node[["node_id"]], dependency = d)
    }
}

# Pull dependencies from parsed dataframe
# - nodes: dataframe returned by parse_nodes()
get_dependencies <- function(nodes) {
    depends <- data.frame()
    for (i in 1:nrow(nodes)) {
        d <- get_node_depends(nodes[i, ])
        if (is.null(d)) {
            next
        }
        for (j in 1:nrow(d)) {
            n <- nodes[
                !is.na(nodes[["assign"]])
                & nodes[["node_id"]] < i 
                & nodes[["assign"]] == d[j, "dependency"], ]
            if (nrow(n) > 0) {
                d[j, "node_id_dependency"] <- max(n[["node_id"]])
            } else {
                d[j, "node_id_dependency"] <- NA
            }
        }
        depends <- rbind(depends, d)
    }
    depends <- depends[!is.na(depends[["node_id_dependency"]]), ]
    rownames(depends) <- NULL
    depends[, c("node_id", "node_id_dependency")]
}

# Recode node_ids for "mutate" nodes
# This will ultimately allow mutate nodes to be collapsed into parent nodes
recode_nodes <- function(nodes, edges, type = "mutate") {
    # 1. identify relation table between new and old node IDs
    e <- merge(edges, nodes[nodes[["type"]] == type, "node_id", drop = FALSE], 
          by = "node_id")
    if (nrow(e) == 0) {
        return(list("nodes" = nodes, "edges" = edges))
    }
    for (i in 1:nrow(e)) {
        # recode current node
        e[i, "new_id"] <- e[i, "node_id_dependency"]
        # propagate to dependencies of current node
        e[["node_id_dependency"]] <- ifelse(
            e[["node_id_dependency"]] == e[i, "node_id"],
            e[i, "new_id"],
            e[["node_id_dependency"]]
        )
    }
    e <- e[c("node_id", "new_id")]
    # 2. Apply relation table to nodes and edges (i.e., recode the node IDs)
    recode_id <- function(df, e, col) {
        names(e)[1] <- col
        out <- merge(df, e, by = col, all.x = TRUE)
        out[[col]] <- ifelse(!is.na(out[["new_id"]]), out[["new_id"]], out[[col]])
        out <- out[order(out[["node_id"]]), ]
        rownames(out) <- NULL
        out[names(df)]
    }
    # a. recode dependencies
    edges <- recode_id(edges, e, "node_id_dependency")
    # b. recode nodes
    # only to be done for linear (1-to-1) dependencies, where the nodes
    #  will be collapsed (i.e., "mutate" nodes)
    if (type == "mutate") {
        edges <- recode_id(edges, e, "node_id") |> 
            dplyr::filter(node_id != node_id_dependency) |>
            dplyr::distinct()
        nodes[["node_id_og"]] <- nodes[["node_id"]]
        nodes <- recode_id(nodes, e, "node_id")
    }
    list("nodes" = nodes, "edges" = edges)
}

# Remove assigned function dependencies from edges dataframe
# This should occur after any relevant global dependencies have been propagated
drop_function_edges <- function(edges, nodes) {
    funcs <- nodes[nodes[["effect"]] == "function", "node_id"]
    edges[
        !edges[["node_id"]] %in% funcs 
        & !edges[["node_id_dependency"]] %in% funcs,
    ]
}
