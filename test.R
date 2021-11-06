# test the workflow

source("R/parse.R")
source("R/plot.R")

# all-in-one version
plot_flow("stirfry.R")

# step-by-step version
exprs <- parse_script("stirfry.R")
lobstr::ast(!!exprs[[16]]) # viewing a tree structure
nodes <- parse_nodes(exprs)
edges <- parse_edges(nodes) |> enrich_edges()
dot <- edges_to_dot(edges)
DiagrammeR::grViz(dot)
