# test the workflow

source("R/nodes.R")
source("R/edges.R")
source("R/dot.R")
source("R/ui.R")

# all-in-one versions
plot_flow("testdat/stirfry.R", F)
plot_flow("testdat/stirfry.R", T)
plot_flow("testdat/stirfry-pasta.R", F)
plot_flow("testdat/stirfry-pasta.R", T)
plot_flow("testdat/example.R", F)
plot_flow("testdat/example.R", T)
plot_flow("testdat/collapse.R", F)
plot_flow("testdat/collapse.R", T)

# step-by-step version
flow_data <- get_flow_data("testdat/example.R", F)
dot <- make_dot(flow_data, F)
DiagrammeR::grViz(dot)
