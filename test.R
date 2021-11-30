# test the workflow

# all-in-one versions
graph("testdat/svy-weight.R", prune_labels = c("count", "summary", "glimpse"))
graph("testdat/svy-weight.R", label_option = "auto",
          prune_labels = c("count", "summary", "sapply", "glimpse", "all.equal"))
graph("testdat/stirfry.R")
graph("testdat/stirfry.R", label_option = "auto")
graph("testdat/stirfry-pasta.R")
graph("testdat/stirfry-pasta.R", prune_labels = c("locations", "search_device"))
graph("testdat/example.R")
graph("testdat/example.R", prune_types = c("function", "mutate"))
graph("testdat/collapse.R")
graph("testdat/collapse.R", prune_types = c("function", "mutate"))

setwd("testdat")
graph("stirfry.Rmd") 
setwd("..")

# step-by-step version
flow <- get_flow("testdat/svy-weight.R")
flow <- prune_flow(flow)
flow <- parameterize_flow(flow)
make_dot(flow) |> DiagrammeR::grViz()
prep_visjs(flow) |> graph_visjs()

# saving for README
DiagrammeR::grViz(dot) |> DiagrammeRsvg::export_svg() |> charToRaw() |> rsvg::rsvg_svg("test.svg") 
