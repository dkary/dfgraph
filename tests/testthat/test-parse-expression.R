library(dfgraph)

expect_col_equal <- function(expr, expected, col) {
    df <- parse_expression(expr)
    eval(bquote(expect_equal(df[[.(col)]], .(expected))))
}

expect_code_equal <- function(rownum, expr, expected) {
    df <- parse_expression(expr)
    eval(bquote(
        expect_equal(rlang::parse_expr(df[["code"]][.(rownum)]), expected)
    ))
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

test_that("values are correctly parsed for a for loop expression with if/else", {
    x <- quote(
        for (i in stuff) {
            if (param == 1) {
                x <- f(i)
            } else {
                x <- g(i)
            }
            output(x)
        }
    )
    expect_col_equal(x, c("x", NA), col = "assign")
    expect_col_equal(x, c("f", "output"), col = "function")
    expect_code_equal(1, x, quote(
        if (param == 1) {
            x <- f(i)
        } else {
            x <- g(i)
        }
    ))
    expect_code_equal(2, x, quote(
        output(x)
    ))
})