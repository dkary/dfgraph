# functions for defining diagram nodes

# Parse an R (or Rmd) file into a list of expressions
parse_script <- function(path_to_file, ignore_source = NULL, is_sourced = FALSE) {
    if (!file.exists(path_to_file)) {
        error_message <- paste0("File '", path_to_file, "' does not exist.")
        if (is_sourced) {
            error_message <- paste0(
                error_message, "\nThis file was expected because of a 'source(",  
                path_to_file, ")' expression.\n",  
                "You may need to 'setwd()' appropriately before running the function.\n",
                "Alternatively, set 'ignore_source = ", path_to_file, "'."
            )
        }
        stop(error_message, call. = FALSE)
    }
    if (tolower(tools::file_ext(path_to_file)) == "rmd") {
        path_to_file <- knitr::purl(
            path_to_file, documentation = 0, output = tempfile(), quiet = TRUE
        )
    }
    exprs <- rlang::parse_exprs(file(path_to_file))
    # recursively pull in any sourced files
    # NOTE: a couple limitations currently:
    # - won't evaluate a variable within source (e.g., "source(path_to_file)")
    # - won't account for setwd() (assumes same env used to call parse_script())
    for (x in exprs) {
        if (rlang::is_call(x, "source")) {
            if (x[[2]] %in% ignore_source) {
                next
            }
            exprs <- c(parse_script(x[[2]], is_sourced = TRUE), exprs)
        }
    } 
    exprs
}

# Get parsed data detail from expression
# - x: one element of list of expressions returned by parse_script()
get_parse_data <- function(x_eval, includeText = TRUE) {
    utils::getParseData(
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
        as.character(x[[3]][[1]])[1]
    } else if (is.call(x)) {
        as.character(x[[1]])[1]
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
    if (rlang::is_call(x, c("<-", "="))) {
        if (rlang::is_call(x[[2]], c("$", "[", "[["))) {
            out[["assign"]] <- as.character(x[[2]][[2]])[1]
            out[["member"]] <- as.character(x[[2]][[3]])[1]
        } else {
            out[["assign"]] <- as.character(x[[2]])[1]
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
    exclude = c("library", "print", "source"), 
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
        if (is.data.frame(x)) {
            if (nrow(x) > 0 ){
                x[["expr_id"]] <- i
            }
        }
        x
    })
    nodes <- do.call(rbind, nodes)
    if (!is.data.frame(nodes)) {
        stop("No eligible nodes were found.")
    }
    nodes[["node_id"]] <- 1:nrow(nodes)
    # some symbols can't be used in dotfile label attributes
    replace_symbol <- function(pattern, replacement) {
        gsub(pattern, replacement, nodes[["effect"]])
    }
    nodes[["effect"]] <- replace_symbol("<-", "assign")
    nodes[["effect"]] <- replace_symbol("=", "assign")
    nodes[["effect"]] <- replace_symbol(">", "gt")
    nodes[["effect"]] <- replace_symbol("<", "lt")
    nodes[, c("node_id", "expr_id", "assign", "member", "effect", "text")]
}

# Add a type column to nodes
# - function: an assigned function
# - input: a raw input assignment (which has no dependencies)
# - mutate: an assignment with a single dependency (i.e., self-dependency)
# - combine: an assignment with multiple dependencies
# - effect: a node with no assignment
add_node_type <- function(nodes, dependencies) {
    # assigned functions will be their own category
    is_assigned_func <- function(text) {
        expr <- rlang::parse_expr(text)
        if (rlang::is_call(expr, c("<-", "="))) {
            expr <- expr[[3]]
        }
        if (rlang::is_call(expr, "function")) {
            TRUE
        } else {
            FALSE
        }
    }
    n <- nodes
    n[["is_func"]] <- lapply(n[["text"]], is_assigned_func) |> unlist()
    
    # identify "mutate" nodes
    func_ids <- n[n[["is_func"]], "node_id"]
    d_count <- dependencies |>
        dplyr::filter(!.data[["node_id_dependency"]] %in% func_ids) |>
        dplyr::count(.data[["node_id"]]) 
    n <- dplyr::left_join(n, d_count, by = "node_id")
    n[["type"]] <- ifelse(is.na(n[["assign"]]), "effect",
        ifelse(n[["is_func"]], "function",                   
            ifelse(is.na(n[["n"]]), "input", 
                ifelse(n[["n"]] == 1, "mutate", "combine")
    )))
    rownames(n) <- NULL
    n[, c("node_id", "expr_id", "assign", "member", "effect", "type", "text")]
}

# Collapse mutate nodes into their input nodes
collapse_across_nodes <- function(nodes, edges) {
    mutate_ids <- nodes[nodes[["type"]] == "mutate", "node_id"]
    x <- edges[edges[["node_id"]] %in% mutate_ids, ]
    names(x)[2] <- "new"
    nodes |>
        dplyr::left_join(x, by = "node_id") |>
        dplyr::mutate(node_id = ifelse(
            is.na(.data[["new"]]), .data[["node_id"]], .data[["new"]]
        )) |>
        dplyr::group_by(.data[["node_id"]]) |>
        dplyr::summarise(
            assign = dplyr::first(.data[["assign"]]),
            effect = dplyr::last(.data[["effect"]]),
            text = paste(.data[["text"]], collapse = "\n")
        )
}
