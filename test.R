# test the workflow

source("R/parse.R")
source("R/plot.R")
source("R/ui.R")

# all-in-one version
plot_flow("stirfry.R")
plot_flow("example.R")

# step-by-step version
nodes <- get_nodes("stirfry.R")
edges <- get_edges(nodes)
dot <- get_dot(nodes, edges)
DiagrammeR::grViz(dot)

# working on a smaller set
# TODO: collapsing the "d" assignments in example.R
exprs <- parse_script("example.R")[1:15]
nodes <- parse_nodes(exprs)
edges <- get_edges(nodes)
dot <- get_dot(nodes, edges)
DiagrammeR::grViz(dot)
