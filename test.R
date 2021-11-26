# test the workflow

# all-in-one versions
plot_flow("testdat/svy-weight.R", 
          prune_labels = c("count", "summary", "sapply", "glimpse", "all.equal"))
plot_flow("testdat/svy-weight.R", prune_all_mutates = TRUE, label_option = "auto",
          prune_labels = c("count", "summary", "sapply", "glimpse", "all.equal"))
plot_flow("testdat/stirfry.R")
plot_flow("testdat/stirfry.R", label_option = "auto")
plot_flow("testdat/stirfry-pasta.R")
plot_flow("testdat/example.R")
plot_flow("testdat/collapse.R")
plot_flow("testdat/collapse.R", prune_all_mutates = TRUE)

setwd("testdat")
plot_flow("stirfry.Rmd") 
setwd("..")

# step-by-step version
nodes <- get_nodes("testdat/svy-weight.R")
edges <- get_edges(nodes)
dot <- make_dot(nodes, edges)
DiagrammeR::grViz(dot) |> DiagrammeRsvg::export_svg() |> charToRaw() |> rsvg::rsvg_svg("test.svg") 
