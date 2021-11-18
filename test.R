# test the workflow

# all-in-one versions
plot_flow("testdat/stirfry.R", F)
plot_flow("testdat/stirfry.R", T)
plot_flow("testdat/stirfry-pasta.R", F)
plot_flow("testdat/stirfry-pasta.R", T)
plot_flow("testdat/example.R", F)
plot_flow("testdat/example.R", T)
plot_flow("testdat/collapse.R", F)
plot_flow("testdat/collapse.R", T)

setwd("testdat")
plot_flow("stirfry.Rmd", F) 
plot_flow("stirfry.Rmd", T) 
setwd("..")

# step-by-step version
flow_data <- get_flow_data("testdat/example.R", F)
dot <- make_dot(flow_data, F)
DiagrammeR::grViz(dot)
