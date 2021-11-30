# test the workflow

# all-in-one versions
plot_flow("testdat/svy-weight.R", prune_labels = c("count", "summary", "glimpse"))
plot_flow("testdat/svy-weight.R", label_option = "auto",
          prune_labels = c("count", "summary", "sapply", "glimpse", "all.equal"))
plot_flow("testdat/stirfry.R")
plot_flow("testdat/stirfry.R", label_option = "auto")
plot_flow("testdat/stirfry-pasta.R")
plot_flow("testdat/stirfry-pasta.R", prune_labels = c("locations", "search_device"))
plot_flow("testdat/example.R")
plot_flow("testdat/example.R", prune_types = c("function", "mutate"))
plot_flow("testdat/collapse.R")
plot_flow("testdat/collapse.R", prune_types = c("function", "mutate"))

setwd("testdat")
plot_flow("stirfry.Rmd") 
setwd("..")

# step-by-step version
flow <- get_flow("testdat/svy-weight.R")
flow <- prune_flow(flow)
flow <- parameterize_flow(flow)
make_dot(flow) |> DiagrammeR::grViz()
prep_visjs(flow) |> plot_visjs()

# saving for README
DiagrammeR::grViz(dot) |> DiagrammeRsvg::export_svg() |> charToRaw() |> rsvg::rsvg_svg("test.svg") 
