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

test_that("values are correctly parsed for an assymetric if/else expression", {
    # The if/else resolves to completely different expressions in this case, b/c
    # a. There are no assignments of the same name in the different paths, and
    # b. There are no equivalent function operation in the different paths
    x <- quote(
        if (param == 1) {
            a <- f(b)
        } else {
            b <- f(a)
        }
    )
    expect_col_equal(x, c("a", "b"), col = "assign")
    expect_col_equal(x, c("f", "f"), col = "function")
    # TODO: the string expectation is clunky, will probably want to check
    # equivalence in expression
    expect_col_equal( 
        x, c(
            "if (param == 1) {\na <- f(b)\n}", 
            "if (param == 1) {\n} else {\nb <- f(a)\n}"
        ), col = "code"
    )
})

test_that("values are correctly parsed for a symmetric if/else expression", {
    # The if/else should resolve to one expression in this case b/c we have
    # exactly one assignment regardless of the condition
    x <- quote(
        if (param == 1) {
            a <- f(b)
        } else {
            a <- f1(b)
        }
    )
    expect_col_equal(x, "a", col = "assign")
    expect_col_equal(x, c("f or f1"), col = "function")
    expect_col_equal(x, "if param == 1 {\na <- f(b)\n} {\na <- f1(b)\n}", 
                     col = "code")
})