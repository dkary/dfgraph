
get <- function(item, location) { 
    path <- find_shortest_path(location)
    retrieve(item, path)
}