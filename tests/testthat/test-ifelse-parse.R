library(dfgraph)

# TODO: fix the 5 failing tests:
# - 1 fails for asymmetric conditionals
# - 4 fail for nested if/else

expect_col_equal <- function(expr, expected, col) {
    df <- ifelse_parse(expr)
    eval(bquote(expect_equal(df[[.(col)]], .(expected))))
}

expect_code_equal <- function(rownum, expr, expected) {
    df <- ifelse_parse(expr)
    expect_equal(rlang::parse_expr(df[["code"]][rownum]), expected)
}

test_that("values are correctly parsed for an if/else asymmetric expression", {
    # The if/else should resolve to 2 nodes
    # Called "asymmetric" because they are defined in different conditionals
    x <- quote(
        if (param == 1) {
            a <- f(b)
        } else {
            c <- g(b)
        }
    )
    expect_col_equal(x, c("a", "c"), col = "assign")
    expect_col_equal(x, c("f", "g"), col = "function")
    expect_code_equal(1, x, quote(
        if (param == 1) { a <- f(b)}
    ))
    # TODO: this one fails b/c it doesn't currently include the 1st condition
    expect_code_equal(2, x, quote(
        if (param == 1) { } else { c <- g(b)}
    ))
})

test_that("values are correctly parsed for an if/else symmetric expression", {
    # The if/else should resolve to 2 nodes, one per assigned variable
    # Called "symmetric" since each variable is defined in every condition
    x <- quote(
        if (param == 1) {
            b <- f(a)
            c <- g(b)
        } else if (param == 2) {
            b <- f1(a)
            c <- g1(b)
        } else {
            b <- f2(a)
            c <- g2(b)
        }
    )
    expect_col_equal(x, c("b", "c"), col = "assign")
    expect_col_equal(x, c("f", "g"), col = "function")
    expect_code_equal(1, x, quote(
        if (param == 1) {
            b <- f(a)
        } else if (param == 2) {
            b <- f1(a)
        } else {
            b <- f2(a)
        }
    ))
    expect_code_equal(2, x, quote(
        if (param == 1) {
            c <- g(b)
        } else if (param == 2) {
            c <- g1(b)
        } else {
            c <- g2(b)
        }
    ))
})

test_that("values are correctly parsed for a nested if/else expression", {
    x <- quote(
        if (param == 1) {
            b <- f(a)
            if (stuff) {
                b <- g(b)    
            }
        } else {
            b <- h(a)
        } 
    )
    # TODO: these 4 fail because I'm not recursively handling if yet
    expect_col_equal(x, c("b", "b"), col = "assign")
    expect_col_equal(x, c("f", "g"), col = "function")
    expect_code_equal(1, x, quote(
        if (param == 1) { 
            b <- f(a) 
        }
    ))
    expect_code_equal(2, x, quote(
        if (param == 1) { 
            if (stuff) {
                b <- g(b)
            }
        } else {
            b <- h(a)
        }
    ))
})

test_that("values are correctly parsed for an if/else with overwrite assignments", {
    # In this case, we have multiple assignments of the same name in the same condition
    x <- quote(
        if (param == 1) {
            a <- f(b)
            a <- h(a)
        } else {
            a <- g(b)
        }
    )
    expect_col_equal(x, c("a", "a"), col = "assign")
    expect_col_equal(x, c("f", "h"), col = "function")
    expect_code_equal(1, x, quote(
        if (param == 1) { a <- f(b) }
    ))
    expect_code_equal(2, x, quote(
        if (param == 1) { 
            a <- h(a)    
        } else {
            a <- g(b)
        }
    ))
})