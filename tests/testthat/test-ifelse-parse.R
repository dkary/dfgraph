library(dfgraph)

expect_col_equal <- function(expr, expected, col) {
    df <- ifelse_parse(expr)
    eval(bquote(expect_equal(df[[.(col)]], .(expected))))
}

expect_code_equal <- function(rownum, expr, expected) {
    df <- ifelse_parse(expr)
    eval(bquote(
        expect_equal(rlang::parse_expr(df[["code_all"]][.(rownum)]), expected)
    ))
}

test_that("values are correctly parsed for an if/else symmetric expression", {
    # The if/else should resolve to 2 nodes (assigned "a" and output func)
    # Called "symmetric" since all conditions have matching statements
    x <- quote(
        if (param) {
            a <- f(b)
            output(a)
        } else {
            a <- g(b)
            output(a)
        }
    )
    expect_col_equal(x, c("a", NA), col = "assign")
    expect_col_equal(x, c("f", "output"), col = "function")
    expect_code_equal(1, x, quote(
        if (param) { a <- f(b) } 
        else { a <- g(b) }
    ))
    expect_code_equal(2, x, quote(
        if (param) { output(a) } 
        else { output(a) }
    ))
})

test_that("values are correctly parsed for an if/else asymmetric expression", {
    # The if/else should resolve to 2 nodes
    # Called "asymmetric" because they are defined in different conditionals
    x <- quote(
        if (param == 1) {
            a <- f(b)
        } else if (param == 2) {
            c <- g(b)
        } else {
            a <- g(b)
            c <- h(b)
        }
    )
    expect_col_equal(x, c("a", "c"), col = "assign")
    expect_col_equal(x, c("f", "g"), col = "function")
    expect_code_equal(1, x, quote(
        if (param == 1) { 
            a <- f(b)
        } else if (param == 2) {
        } else {
            a <- g(b)
        }
    ))
    expect_code_equal(2, x, quote(
        if (param == 1) { 
        } else if (param == 2) { 
            c <- g(b) 
        } else { 
            c <- h(b)
        }
    ))
})

test_that("values are correctly parsed for a long if/else symmetric expression", {
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

test_that("values are correctly parsed for a complex if/else expression", {
    # Expecting 5 nodes returned (1 for "a", 3 for "c", 1 for "output")
    x <- quote(
        if (param == 1) {
            a <- f(b)
            c <- g(a)
            output(a)
        } else if (stuff) {
            a <- g(b)
            if (a > 2) {
                output(a)
            }
        } else {
            a <- z(b)
            c <- h(a)
            c <- z(c)
            c$person <- "humphry"
            output(a)
        }
    )
    expect_col_equal(x, c("a", "c", "c", "c", NA), col = "assign")
    expect_col_equal(x, c("f", "h", "g", NA, "output"), col = "function")
    expect_code_equal(2, x, quote(
        if (param == 1) {} 
        else if (stuff) {} 
        else { c <- h(a) }
    ))
    expect_code_equal(3, x, quote(
        if (param == 1) { c <- g(a) } 
        else if (stuff) {} 
        else { c <- z(c) }
    ))
    expect_code_equal(5, x, quote(
        if (param == 1) { output(a) } 
        else if (stuff) { 
            if (a > 2) { output(a)} 
        } else { output(a) }
    ))
    
})