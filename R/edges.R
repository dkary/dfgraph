# functions for defining diagram edges

# TODO:
# [ ] probably use this lower-level function elsewhere as well
# Get dependencies (vector) for a single R statement
# The statement will be represented by an expression
# To be called from get_func_depends()
parse_statement_depends <- function(x){
    # for an assignment, go one level done (I think)
    if (rlang::is_call(x, "<-")) {
        df <- get_parse_data(x[[3]])
    } else {
        df <- get_parse_data(x)
    }
    df[df[["token"]] == "SYMBOL", "text"]
}

# TODO: 
# [ ] should return function dependency (not just assignment)
# Get global dependencies (vector) of a function represented by an expression
get_func_depends <- function(expr) {
    # 1. identify named arguments
    # This will initialize locally-scoped function variables
    f <- expr[[3]]
    f_formals <- f[[2]] # function formals get stored in a pairlist
    locals <- names(f_formals) # function args
    
    # 2. identify any globals called by default argument values
    globals <- c() 
    for (i in seq_along(f_formals)) {
        if (is.name(f_formals[[i]]) & f_formals[[i]] != "") {
            globals <- c(globals, rlang::as_string(f_formals[[i]]))
        }
    }
    
    # 3. parse nodes from the function expression
    n <- lapply(f, parse_expression)
    n <- do.call(rbind, n)
    
    # 4. Trace locals/globals over function statements
    for (i in 1:nrow(n)) {
        x <- n[i, "text"] |> rlang::parse_expr()
        locals <- c(locals, n[i, "assign"])
        if (rlang::is_call(x, "<-")) {
            if (rlang::is_call(x[[3]], "function")) {
                globals <- c(globals, get_func_depends(x))
            } else {
                depends <- parse_statement_depends(x)
                globals <- c(globals, setdiff(depends, locals))
            }
        } else {
            depends <- parse_statement_depends(x)
            globals <- c(globals, setdiff(depends, locals))
        }
    }
    unique(globals)
}

# Pull dependencies from parsed dataframe
# Dependencies are identified by the "SYMBOL" token from base::getParseData()
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
