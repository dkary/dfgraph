# functions to parse dependencies from an R script

# Parse an R file path into a list of expressions
# A placeholder. Will probably want more to it than this 
# (e.g., pulling sourced files recursively and reading Rmd)
parse_script <- function(path_to_file) {
    # TODO: determine whether this provides value over base::parse()
    rlang::parse_exprs(file(path_to_file))
}

# Get parsed data detail from expression
# - x = an expression returned from parse_script()
get_parse_data <- function(x_eval, includeText = TRUE) {
    getParseData(
        # a hack to get a dataframe with just one expression (seems inefficient)
        parse(text = deparse(x_eval)), 
        includeText = includeText
    )
}

# Pull assigned variables and associated code
# - x: one element of list of expressions returned by parse_script()
get_assignments <- function(x) {
    if (is.call(x)) {
        # we only care about called expressions (I think)
        # the is.name() excludes assignment of the df$col variety
        # we'll only grab one assignment (unless control flow is present)
        # i.e., for nested assignments, we only get the first one
        if (rlang::is_call(x, "<-") && (is.name(x[[2]]))) {
            out <- get_parse_data(x)
            out$target <- rlang::as_string(x[[2]])
            out$type <- "assign"
            out[out$parent==0, c("target", "type", "text")]
        } else {
            # this catches assignments that occur later (e.g., within if/else)
            # since they will be further down the tree (hence recursion)
            dplyr::bind_rows(lapply(x, get_assignments))
        }
    }
}

# Pull first function in non-assignment expression (and associated code)
# - x: one element of list of expressions returned by parse_script()
get_effects <- function(
    x, exclude = c("library", "print"), ignore = c("if", "==", "{", "for", ":")
) {
    if (is.call(x)) {
        # we'll exclude assignment or specified functions
        if (rlang::is_call(x, c("<-", exclude))) {
            # an empty dataframe simplifies downstream calculations
            data.frame()
        } else if (rlang::is_call(x, ignore)) {
            # encountering certain functions (usually control flow) leads to 
            # recursing down the tree to find those that may act on data
            data.frame(dplyr::bind_rows(lapply(x, get_effects)))
        } else {
            out <- get_parse_data(x)
            # we want the last function in a pipeline
            if (rlang::is_call(x, c("%>%", "|>"))) {
                out$target <- rlang::as_string(x[[3]][[1]])
                # otherwise, we pull the first function
            } else {
                out$target <- rlang::as_string(x[[1]])
            }
            out$type <- "effect"
            out[out$parent==0, c("target", "type", "text")]
        }
    }
}

# Pull together all assignments and effects into a dataframe
# - exprs: list of expressions returned by parse_script()
get_parsed <- function(exprs) {
    parsed <- lapply(seq_along(exprs), function(i) {
        dplyr::bind_rows(
            get_assignments(exprs[[i]]) |> dplyr::mutate(expr_id = i),
            get_effects(exprs[[i]]) |> dplyr::mutate(expr_id = i)
        )
    }) |> 
        dplyr::bind_rows()
    parsed$parse_id <- as.integer(row.names(parsed))
    parsed[, c("parse_id", "expr_id", "target", "type", "text")]
}

# Pull dependencies from parsed dataframe
# - parsed: dataframe returned by get_parsed()
identify_dependencies <- function(parsed) {
    identify_one <- function(df, assigned) {
        x <- rlang::parse_expr(df$text)
        if (df$type == "assign") {
            df_parsed <- get_parse_data(x[[3]])
        } else {
            df_parsed <- get_parse_data(x)
        }
        out <- data.frame(
            depends = unique(df_parsed[df_parsed$token == "SYMBOL", ]$text)
        )
        if (nrow(out) != 0) {
            out$parse_id <- df[["parse_id"]]
            out[out$depends %in% assigned, c("parse_id", "depends")]
        }
    }
    assigned <- unique(parsed[parsed$type == "assign", ]$target)
    dependencies <- lapply(1:nrow(parsed), function(i) {
        identify_one(parsed[i,], assigned) 
    }) |> 
        dplyr::bind_rows()
}