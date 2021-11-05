# functions to parse dependencies from an R script

# Parse an R file path into a list of expressions
# A placeholder. Will probably want more to it than this 
# (e.g., pulling sourced files recursively and reading Rmd)
parse_script <- function(path_to_file) {
    # TODO: determine whether this provides value over base::parse()
    rlang::parse_exprs(file(path_to_file))
}

# Get parsed data detail from expression
# - x: one element of list of expressions returned by parse_script()
get_parse_data <- function(x_eval, includeText = TRUE) {
    getParseData(
        # a hack to get a dataframe with just one expression (seems inefficient)
        parse(text = deparse(x_eval)), 
        includeText = includeText
    )
}

# Convert an expression into a parsed dataframe
# - x: one element of list of expressions returned by parse_script()
# - type: either "assign" (i.e., assignment) or "effect" (i.e., side-effect)
expression_to_df <- function(x, type) {
    out <- get_parse_data(x)
    if (type == "assign") {
        # we want the dataframe target for column assignment
        if (rlang::is_call(x[[2]], "$")) {
            out$target <- rlang::as_string(x[[2]][[2]])
        } else {
            out$target <- rlang::as_string(x[[2]])
        }
    } else {
        # we want the last function if there's a pipeline
        if (rlang::is_call(x, c("%>%", "|>"))) {
            out$target <- rlang::as_string(x[[3]][[1]])
        } else {
            out$target <- rlang::as_string(x[[1]])
        }
    }
    out$type <- type
    out[out$parent==0, c("target", "type", "text")]
}

# Pull node information for a given expression (assignment or effect)
# - x: one element of list of expressions returned by parse_script()
# - exclude: expressions beginning with these functions will be excluded from output
# - recurse: expressions beginning with these will lead to recursion
parse_expression <- function(
    x, 
    exclude = c("library", "print"), 
    recurse = c("if", "==", "{", "for", ":")
) {
    if (is.call(x)) {
        if (rlang::is_call(x, exclude)) {
            # an empty dataframe simplifies downstream operations
            data.frame()
        } else if (rlang::is_call(x, "<-")) {
            expression_to_df(x, "assign")
        } else if (!rlang::is_call(x, recurse)) {
            expression_to_df(x, "effect")
        } else {
            dplyr::bind_rows(lapply(x, parse_expression))
        }
    }
}

# Pull together all node information into a dataframe
# - exprs: list of expressions returned by parse_script()
parse_nodes <- function(exprs) {
    nodes <- lapply(seq_along(exprs), function(i) {
        parse_expression(exprs[[i]]) |> dplyr::mutate(expr_id = i)
    }) |> 
        dplyr::bind_rows() |>
        tibble::as_tibble()
    nodes$node_id <- as.integer(row.names(nodes))
    nodes[, c("node_id", "expr_id", "target", "type", "text")]
}

# Pull dependencies from parsed dataframe
# - nodes: dataframe returned by parse_nodes()
get_dependencies <- function(nodes) {
    identify_one <- function(df, assigned) {
        x <- rlang::parse_expr(df$text)
        if (df$type == "assign") {
            df_parsed <- get_parse_data(x[[3]])
        } else {
            df_parsed <- get_parse_data(x)
        }
        out <- data.frame(
            dependency = unique(df_parsed[df_parsed$token == "SYMBOL", ]$text)
        )
        if (nrow(out) != 0) {
            out$node_id <- df[["node_id"]]
            out[out$dependency %in% assigned, c("node_id", "dependency")]
        }
    }
    assigned <- unique(nodes[nodes$type == "assign", ]$target)
    lapply(1:nrow(nodes), function(i) {
        identify_one(nodes[i,], assigned) 
    }) |> 
        dplyr::bind_rows()
}

# Pull dependency node numbers
get_dependency_nodes <- function(dependencies, nodes) {
    # prepare all possible dependency node identifiers (for joining)
    n <- nodes[nodes$type == "assign", c("node_id", "target")]
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
    n <- nodes[, c("node_id", "target")]
    out <- merge(n, e, by = "node_id")
    out[, c("node_id", "target", "node_id_dependency", "dependency")]
}
