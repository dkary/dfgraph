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
flow <- get_flow("testdat/svy-weight.R")
flow <- prep_flow(flow)
dot <- make_dot(flow)
DiagrammeR::grViz(dot) |> DiagrammeRsvg::export_svg() |> charToRaw() |> rsvg::rsvg_svg("test.svg") 
