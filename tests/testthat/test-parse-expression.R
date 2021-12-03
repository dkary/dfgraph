library(dfgraph)

# TODO: 
# - expressions that return multiple rows (i.e., multi-node expressions)

expect_col_equal <- function(expr, expected, col) {
    df <- parse_expression(expr)
    eval(bquote(expect_equal(df[[.(col)]], .(expected))))
}

test_that("values are correctly parsed for a simple expression", {
    x <- quote(
        svy$person <- svy$person %>% 
            left_join(flags$flag_totals, by = "Vrid") %>%  
            mutate(flag = ifelse(is.na(flag), 0, flag))
    )
    expect_col_equal(x, "svy", col = "assign")
    expect_col_equal(x, "mutate", col = "function")
    expect_col_equal(x, "person", col = "member")
})

test_that("values are correctly parsed for an if/else expression", {
    # These will return multiple dataframe rows for one expression
    # 1. symmetric nodes (e.g, assignment in all conditions)
    # 2. assymetric nodes (e.g., assignment in a subset of conditions)
    
})
