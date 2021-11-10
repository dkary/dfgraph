# test the workflow

source("R/nodes.R")
source("R/edges.R")
source("R/dot.R")
source("R/ui.R")

# all-in-one versions
plot_flow("testdat/stirfry.R", F)
plot_flow("testdat/example.R", T)
plot_flow("testdat/collapse.R", F)

# step-by-step version
nodes <- get_nodes("stirfry.R")
edges <- get_edges(nodes)
dot <- get_dot(nodes, edges)
DiagrammeR::grViz(dot)
