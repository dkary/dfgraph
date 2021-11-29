
# TODO: Planning
# What do you really want from the interactivity?
# Probably the quickest way to get there is with Shiny, worth prototyping there at least
# - moving the plot_flow() arguments to interactive Shiny session
# - How important are the outstanding features?
#   + nice scrollable display of network of code (might be easier in Shiny)
#   + interactive node collapsing/highlighting

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

# TODO:
# - probably make the border color darker, likely with visGroups defined
# - having a hard time avoiding edge crossing, maybe igraph layout?
# - figure out newline required by vis.js (\n doesn't work), probably html is needed
# TODO Features
# - highlight whole network
# - interactively edit (node pruning, etc.)
# - node clustering
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