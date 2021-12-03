# functions for defining diagram nodes

# Create a temporary R script (from Rmd) and return it's filepath
rmd_to_r <- function(path_to_file) {
    if (!requireNamespace("knitr", quietly = TRUE)) {
        stop("Package \"knitr\" needed to parse Rmd files. Please install it.",
             call. = FALSE)
    }
    knitr::purl(
        path_to_file, documentation = 0, output = tempfile(), quiet = TRUE
    )
}

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
        path_to_file <- rmd_to_r(path_to_file)
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
    df <- utils::getParseData(
        # a hack to get a dataframe with just one expression (seems inefficient)
        parse(text = deparse(x_eval)), 
        includeText = includeText
    )
    if ("text" %in% names(df)) {
        names(df)[names(df) == "text"] <- "code"
    }
    df
}

# Get the "function" (i.e., primary function) of a statement (in an expression)
# Intended to be called from parse_statement()
# - x: one element of list of expressions returned by parse_script()
get_function <- function(x) {
    if (rlang::is_call(x, c("%>%", "|>"))) {
        as.character(x[[3]][[1]])[1]
    } else if (is.call(x)) {
        as.character(x[[1]])[1]
    } else {
        NA
    }
}

# Get assignments, functions, and text of an expression
# - x: one element of list of expressions returned by parse_script()
parse_statement <- function(x) {
    v_assign <- NA
    v_member <- NA
    v_code <- paste(deparse(x), collapse = "\n")
    if (rlang::is_call(x, c("<-", "="))) {
        if (rlang::is_call(x[[2]], c("$", "[", "[["))) {
            v_assign <- as.character(x[[2]][[2]])[1]
            v_member <- as.character(x[[2]][[3]])[1]
        } else {
            v_assign <- as.character(x[[2]])[1]
        }
        v_function <- get_function(x[[3]])
    } else {
        v_function <- get_function(x)
    }
    out <- data.frame(v_assign, v_member, v_function, v_code)
    names(out) <- c("assign", "member", "function", "code")
    out
}

# Pull node information for a given expression (assignment or function)
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
        parse_expression(exprs[[i]])
    })
    nodes <- do.call(rbind, nodes)
    if (!is.data.frame(nodes)) {
        stop("No eligible nodes were found.")
    }
    nodes[["id"]] <- 1:nrow(nodes)
    # some symbols can't be used in dotfile label attributes
    replace_symbol <- function(pattern, replacement) {
        gsub(pattern, replacement, nodes[["function"]])
    }
    nodes[["function"]] <- replace_symbol("<-", "assign")
    nodes[["function"]] <- replace_symbol("=", "assign")
    nodes[["function"]] <- replace_symbol(">", "gt")
    nodes[["function"]] <- replace_symbol("<", "lt")
    nodes[, c("id", "assign", "member", "function", "code")]
}

# Identify node IDs to prune (based on "type")
get_pruned_types <- function(nodes, prune_types) {
    if (is.null(prune_types)) {
        return(NULL)
    }
    nodes[nodes[["type"]] %in% prune_types, "id"]
}

# Identify node IDs to prune (based on the node label)
# Either "assign" or "function" can be used as a label, so they are both checked
get_pruned_labels <- function(nodes, prune_labels) {
    if (is.null(prune_labels)) {
        return(NULL)
    }
    function_ids <- nodes[
        !is.na(nodes[["function"]]) & nodes[["function"]] %in% prune_labels, "id"
    ]
    assign_ids <- nodes[
        !is.na(nodes[["assign"]]) & nodes[["assign"]] %in% prune_labels, "id"
    ]
    unique(c(assign_ids, function_ids))
}
