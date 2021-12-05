# functions to parse if/else expressions

# Parse an if/else expression into individual parsed statements
# Returns a dataframe like from parse_statement(), but with a "conditional" col
# - x: one element of list of expressions returned by parse_script()
ifelse_parse_statement <- function(x, con = "if") {
    if (rlang::is_call(x, "if")) {
        # element 1 = "if", element 2 = condition
        v_conditional <- paste0(con, " (", deparse(x[[2]]), ")")
        # element 3 = the statements executed when condition is true
        t <- x[[3]]
    } else {
        # a final else has no if condition
        v_conditional <- "else"
        t <- x
    }
    # TODO: may want to specifically handle "{", rather than implicitly here
    out <- lapply(2:length(t), function(i) parse_statement(t[[i]]))
    out <- do.call(rbind, out)
    out[["conditional"]] <- v_conditional
    # element 4 is present when there are more if/else conditions
    if (length(x) == 4) {
        out <- rbind(out, ifelse_parse_statement(x[[4]], con = "else if"))
    }
    out
}

# Add a "name" column which determines how unique nodes are defined
# Example: the name will be the assignment or the whole code if no assignment
# - df: dataframe returned by ifelse_parse_statement()
ifelse_add_name <- function(df) {
    df[["id"]] <- 1:nrow(df)
    df[["name"]] <- ifelse(
        !is.na(df[["assign"]]) & !is.na(df[["member"]]), 
        paste(df[["assign"]], df[["member"]], sep = "$"),
        ifelse(!is.na(df[["assign"]]), df[["assign"]], df[["code"]])
    )
    df
}

# Create an interim relation table of id, node (for one name)
# To be called from ifelse_collapse()
# - df: dataframe returned by ifelse_add_name()
# - name: a single "name", one of those in df$name
ifelse_flag_node <- function(df, name) {
    x <- df[df[["name"]] == name, c("conditional", "id", "name")]
    x[["grp"]] <- 0
    y <- x
    i <- 0
    while(nrow(y) > 0) {
        i <- i + 1
        ids <- split(y, y$conditional) |> sapply(function(y) max(y[["id"]]))
        x[["grp"]] = ifelse(x[["id"]] %in% ids, i, x[["grp"]])
        y <- y[!y[["id"]] %in% ids, ]
    }
    x[["node"]] <- paste0(x[["name"]], x[["grp"]])
    x[c("id", "node")]
}

# Combine symmetric nodes in a parsed dataframe
# Returns a dataframe in the same format as parse_statement()
# - df: dataframe returned by ifelse_add_name()
ifelse_collapse <- function(df) {
    # Flag each row with the relevant node (for combining)
    relate <- lapply(unique(df[["name"]]), function(nm) ifelse_flag_node(df, nm))
    df_relate <- do.call(rbind, relate)
    
    # Aggregate by "node" from df_relate
    df_merge <- merge(df, df_relate, by = "id", all.x = TRUE)
    df_merge[["code_all"]] <- paste0(
        df_merge[["conditional"]], " {\n  ", df_merge[["code"]], " \n}"
    )
    out <- split(df_merge, df_merge$node) |> lapply(function(x) {
        data.frame(
            "id" = max(x[["id"]]),
            "assign" = unique(x[["assign"]])[1], # should be only 1
            "member" = unique(x[["member"]])[1], # should be only 1
            "function" = unique(x[["function"]])[1],
            "code" = paste(x[["code_all"]], collapse = " ")
        )
    })
    out <- do.call(rbind, out)
    names(out)[4] <- "function"
    out[order(out[["id"]]), c("assign", "member", "function", "code")]
}

# Parse an if/else expression into multiple nodes
# Returns a dataframe in the same format as parse_statement()
# - x: one element of list of expressions returned by parse_script()
ifelse_parse <- function(x) {
    df <- ifelse_parse_statement(x)
    df <- ifelse_add_name(df)
    ifelse_collapse(df)
}