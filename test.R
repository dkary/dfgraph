# test the workflow

source("R/parse.R")
source("R/plot.R")

# all-in-one version
plot_flow("stirfry.R")

# step-by-step version
exprs <- parse_script("example.R")
lobstr::ast(!!exprs[[11]]) # viewing a tree structure
nodes <- parse_nodes(exprs)
edges <- parse_edges(nodes)
dot <- edges_to_dot(edges)
dag <- dagitty::dagitty(dot)
tidy_dag <- ggdag::tidy_dagitty(dag)
ggdag::ggdag(tidy_dag)
