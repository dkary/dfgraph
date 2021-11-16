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
        d <- c(d, node[["member"]])
    }
    if (length(d) > 0) {
        data.frame(node_id = node[["node_id"]], dependency = d)
    }
}

# Pull dependencies from parsed dataframe
# - nodes: dataframe returned by parse_nodes()
get_dependencies <- function(nodes) {
    assigned <- unique(nodes[["assign"]])
    out <- lapply(1:nrow(nodes), function(i) {
        x <- get_node_depends(nodes[i,])
        x[x[["dependency"]] %in% assigned, ]
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
