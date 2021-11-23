# test the workflow

# all-in-one versions
plot_flow("testdat/svy-weight.R", 
          prune_labels = c("count", "summary", "sapply", "glimpse", "all.equal"))
plot_flow("testdat/stirfry.R", label_option = "auto")
plot_flow("testdat/stirfry.R")
plot_flow("testdat/stirfry-pasta.R")
plot_flow("testdat/example.R")
plot_flow("testdat/collapse.R")

setwd("testdat")
plot_flow("stirfry.Rmd") 
setwd("..")

# step-by-step version
nodes <- get_nodes("testdat/collapse.R")
edges <- get_edges(nodes)
dot <- make_dot(nodes, edges)
DiagrammeR::grViz(dot)
