# functions for defining diagram edges

# Parse dependencies (vector) for a single R statement
# The statement will be represented by an expression
parse_statement_depends <- function(expr) {
    parse_one <- function(x) {
        df <- get_parse_data(x)
        unique(df[df[["token"]] %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"), "code"])
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
            if (is.name(f_formals[i]) & f_formals[i] != "") {
                globals <- c(globals, rlang::as_string(f_formals[i]))
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
            x <- n[i, "code"] |> rlang::parse_expr()
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
    x <- rlang::parse_expr(node[["code"]])
    d <- parse_statement_depends(x)
    if (!is.na(node[["member"]])) {
        # if membership is assigned, then there's also a dependency on itself
        d <- c(d, node[["assign"]])
    }
    if (length(d) > 0) {
        data.frame("to" = node[["id"]], "from_name" = d)
    }
}

# Get dependencies for all nodes in a dataframe
# - nodes: dataframe returned by parse_nodes()
get_depends <- function(nodes) {
    depends <- data.frame()
    for (i in 1:nrow(nodes)) {
        d <- get_node_depends(nodes[i, ])
        if (is.null(d)) {
            next
        }
        for (j in 1:nrow(d)) {
            n <- nodes[
                !is.na(nodes[["assign"]])
                & nodes[["id"]] < i 
                & nodes[["assign"]] == d[j, "from_name"], ]
            if (nrow(n) > 0) {
                d[j, "from"] <- max(n[["id"]])
            } else {
                d[j, "from"] <- NA
            }
        }
        depends <- rbind(depends, d)
    }
    depends <- depends[!is.na(depends[["from"]]), ]
    rownames(depends) <- NULL
    depends[, c("to", "from")]
}

# Cascade dependencies for selected node IDs
# Ultimately for collapsing across specified node IDs (e.g., mutate nodes)
cascade_depends <- function(edges, ids) {
    e <- edges
    for (id in ids) {
        x <- e[e[["to"]] == id, ]
        names(x) <- c("from", "new")
        e <- merge(e, x, by = "from", all.x = TRUE)
        e[["from"]] <- ifelse(
            is.na(e[["new"]]), e[["from"]], e[["new"]]
        )
        e[["new"]] <- NULL
        e <- e[order(e[["to"]]), c("to", "from")]
    }
    rownames(e) <- NULL
    dplyr::distinct(e)
}

# Prune function nodes (from edges dataframe) and optionally additional nodes 
# Additional nodes are selected based on matching node labels (in assign or function)
prune_node_edges <- function(edges, nodes, prune_labels = NULL) {
    # We'll always prune function IDs (at least for now)
    ids <- nodes[nodes[["function"]] == "function", "id"]
    
    if (!is.null(prune_labels)) {
        function_ids <- nodes[
            !is.na(nodes[["function"]]) & nodes[["function"]] %in% prune_labels, "id"
        ]
        assign_ids <- nodes[
            !is.na(nodes[["assign"]]) & nodes[["assign"]] %in% prune_labels, "id"
        ]
        ids <- c(ids, assign_ids, function_ids)
    }
    edges <- cascade_depends(edges, ids)
    edges[
        !edges[["to"]] %in% ids
        & !edges[["from"]] %in% ids,
    ]
}
