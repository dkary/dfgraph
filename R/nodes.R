# functions for defining diagram nodes

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

# Get the "effect" (i.e., primary function) of a statement (in an expression)
# Intended to be called from parse_statement()
# - x: one element of list of expressions returned by parse_script()
get_effect <- function(x) {
    if (rlang::is_call(x, c("%>%", "|>"))) {
        rlang::as_string(x[[3]][[1]])
    } else if (is.call(x)) {
        rlang::as_string(x[[1]])
    } else {
        NA
    }
}

# Get assignments, effects, and text of an expression
# - x: one element of list of expressions returned by parse_script()
parse_statement <- function(x) {
    out <- get_parse_data(x)
    out[["assign"]] <- NA
    out[["member"]] <- NA
    if (rlang::is_call(x, "<-")) {
        if (rlang::is_call(x[[2]], c("$", "[", "[["))) {
            out[["assign"]] <- rlang::as_string(x[[2]][[2]])
            out[["member"]] <- rlang::as_string(x[[2]][[3]])
        } else {
            out[["assign"]] <- rlang::as_string(x[[2]])
        }
        out[["effect"]] <- get_effect(x[[3]])
    } else {
        out[["effect"]] <- get_effect(x)
    }
    out[out[["parent"]]==0, c("assign", "member", "effect", "text")]
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
        } else if (!rlang::is_call(x, recurse)) {
            parse_statement(x)
        } else {
            out <- lapply(x, parse_expression)
            do.call(rbind, out)
        }
    }
}

# Pull together all node information into a dataframe
# - exprs: list of expressions returned by parse_script()
parse_nodes <- function(exprs) {
    nodes <- lapply(seq_along(exprs), function(i) {
        x <- parse_expression(exprs[[i]])
        if (nrow(x) > 0) {
            x[["expr_id"]] <- i
        }
        x
    })
    nodes <- do.call(rbind, nodes)
    nodes[["node_id"]] <- 1:nrow(nodes)
    # both "<" and "-" can't be used in node names in dotfiles
    nodes[["effect"]] <- gsub("<-", "assign", nodes[["effect"]])
    nodes[, c("node_id", "expr_id", "assign", "member", "effect", "text")]
}

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

# Add a depends column to nodes which specifies the nodes' dependencies
add_dependencies <- function(nodes, dependencies) {
    d <- dplyr::group_by(dependencies, node_id) |>
        dplyr::mutate(rownum = paste0("x", dplyr::row_number())) |>
        dplyr::ungroup()
    p <- tidyr::pivot_wider(
        d, 
        names_from = .data[["rownum"]], 
        values_from = .data[["dependency"]]
    )
    d_combined <- tidyr::unite(p, "depends", -node_id, na.rm = TRUE, sep = ", ")
    out <- dplyr::left_join(nodes, d_combined, by = "node_id")
    out[, c("node_id", "expr_id", "assign", "member", "effect", "depends", "text")]
}

# Add a type column to nodes
# - input: a raw input assignment (which has no dependencies)
# - mutate: an assignment with a single dependency (i.e., self-dependency)
# - combine: an assignment with multiple dependencies
# - effect: a node with no assignment
add_node_type <- function(nodes, dependencies) {
    d_count <- dplyr::count(dependencies, .data[["node_id"]])
    n <- dplyr::left_join(nodes, d_count, by = "node_id")
    n[["type"]] <- ifelse(is.na(n[["assign"]]), "effect",
        ifelse(is.na(n[["n"]]), "input", 
            ifelse(n[["n"]] == 1, "mutate", "combine")
    ))
    n[, c("node_id", "expr_id", "assign", "member", "effect", "depends", "type", "text")]
}
