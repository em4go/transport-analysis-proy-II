
# Function to find the nearest node from a osmnx graph to a given point
find_nearest_node <- function(graph, x_given, y_given) {
  distances <- sqrt((V(grafo)$y - y_given)^2 + (V(grafo)$x - x_given)^2)
  nearest_node_index <- as.numeric(which.min(distances))
  
  return(V(graph)[nearest_node_index])
}