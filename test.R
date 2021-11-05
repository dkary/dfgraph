# test the workflow

source("R/parse.R")
exprs <- parse_script("example.R")

# some exploration
length(exprs) # the number of unique expression elements
exprs[[9]] # expression with a pipeline
lobstr::ast(!!exprs[[9]]) # viewing a tree structure

# parse away
nodes <- parse_nodes(exprs)
dependencies <- parse_dependencies(nodes)
