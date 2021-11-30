# functions for plotting using vis.js

# Do some preparation for getting the format for visNetwork()
prep_visjs <- function(flow) {
    nodes <- flow[["nodes"]]
    nodes[["group"]] <- nodes[["node_type"]]
    nodes[["title"]] <- nodes[["hover"]]
    nodes[["node_type"]] <- NULL
    nodes[["hover"]] <- NULL
    edges <- flow[["edges"]]
    edges[["arrows"]] <- "to"
    list("nodes" = nodes, "edges" = edges)
}

# Plot the flow using the visNetwork package
plot_visjs <- function(flow) {
    visNetwork::visNetwork(nodes = flow[["nodes"]], edges = flow[["edges"]]) |>
        visNetwork::visOptions(
            highlightNearest = list(
                degree = list(from = 10, to = 10), 
                hover = TRUE, 
                algorithm = "hierarchical"
            ),
            collapse = TRUE
        ) |>
        visNetwork::visHierarchicalLayout(
            direction = "UD",
            sortMethod = "directed",
            levelSeparation = 80,
            nodeSpacing = 100,
            shakeTowards = "leaves"
        ) |>
        visNetwork::visPhysics(
            hierarchicalRepulsion = list(
                avoidOverlap = 1, 
                nodeDistance = 50,
                springLength = 120
            )
        ) |>
        visNetwork::visInteraction(multiselect = TRUE) |>
        visNetwork::visNodes(
            shape = "box",
            font = list(size = 16),
            margin = 8
        ) |>
        visNetwork::visEdges(
            color = "#828282"
        )
}