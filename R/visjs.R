
# TODO:
# - continue working on getting some nice spacing/appearance
#   having a hard time avoiding edge crossing, maybe igraph layout?
# - highlight direction only upstream
# - figure out newline required by vis.js (\n doesn't work), probably html is needed
# - colors for nodes (store these in a color column of course)
# TODO Features
# - highlight whole network
# - interactively edit (node pruning, etc.)
# - node clustering
plot_visjs <- function(flow) {
    # some prep required by visNetwork()
    nodes <- flow[["nodes"]]
    nodes[["group"]] <- nodes[["node_type"]]
    nodes[["title"]] <- nodes[["hover"]]
    nodes[["node_type"]] <- NULL
    nodes[["hover"]] <- NULL
    edges <- flow[["edges"]]
    edges[["arrows"]] <- "to"
    visNetwork::visNetwork(nodes = nodes, edges = edges) |>
        visNetwork::visOptions(
            highlightNearest = list(
                enabled = T, degree = 20, hover = TRUE, algorithm = "hierarchical"
            ),
            collapse = TRUE,
            width = "100%"
        ) |>
        visNetwork::visHierarchicalLayout(
            enable = TRUE,
            direction = "UD",
            blockShifting = TRUE,
            edgeMinimization = TRUE,
            sortMethod = "directed",
            levelSeparation = 150,
            parentCentralization = TRUE,
            nodeSpacing = 150,
            shakeTowards = "leaves"
        ) |>
        visNetwork::visPhysics(
            enabled = TRUE,
            hierarchicalRepulsion = list(
                avoidOverlap = 1, nodeDistance = 120
            )
        ) |>
        visNetwork::visInteraction(multiselect = TRUE)
}