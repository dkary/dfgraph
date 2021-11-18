# functions for defining diagram edges

# Parse dependencies (vector) for a single R statement
# The statement will be represented by an expression
parse_statement_depends <- function(expr) {
    parse_one <- function(x) {
        df <- get_parse_data(x)
        unique(df[df[["token"]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"), "text"])
    }
    if (rlang::is_call(expr, c("<-", "="))) {
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

# Cascade dependencies for selected node IDs
# Ultimately for collapsing across specified node IDs (e.g., mutate nodes)
cascade_depends <- function(edges, ids) {
    e <- edges
    for (id in ids) {
        x <- e[e[["node_id"]] == id, ]
        names(x) <- c("node_id_dependency", "new")
        e <- merge(e, x, by = "node_id_dependency", all.x = TRUE)
        e[["node_id_dependency"]] <- ifelse(
            is.na(e[["new"]]), e[["node_id_dependency"]], e[["new"]]
        )
        e[["new"]] <- NULL
        e <- e[order(e[["node_id"]]), c("node_id", "node_id_dependency")]
    }
    rownames(e) <- NULL
    dplyr::distinct(e)
}

# Remove assigned function dependencies from edges dataframe
drop_function_edges <- function(edges, nodes) {
    funcs <- nodes[nodes[["effect"]] == "function", "node_id"]
    edges[
        !edges[["node_id"]] %in% funcs 
        & !edges[["node_id_dependency"]] %in% funcs,
    ]
}
