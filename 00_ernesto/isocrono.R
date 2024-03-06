.lib<- c("igraph","osmdata", "dplyr", "leaflet")


.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst])
lapply(.lib, require, character.only=TRUE)


coords_bb <- getbb("Calle músico martínez coll, 2. Valencia, Spain")
rownames(coords_bb)
coords_bb["x", ]

# center of the bounding box
coords <- list(x=mean(coords_bb["x", ]), y=mean(coords_bb["y", ]))
coords

# Cargar los datos
grafo <- read_graph("./data/networks_data/valencia_walk.graphml", format = "graphml")

V(grafo)$x <- as.numeric(V(grafo)$x)
V(grafo)$y <- as.numeric(V(grafo)$y)
V(grafo)$x
V(grafo)$y

E(grafo)$length <- as.numeric(E(grafo)$length)


coords["x"][[1]]
dis <- sqrt((V(grafo)$y - coords["y"][[1]])^2 + (V(grafo)$x - coords["x"][[1]])^2)
as.numeric(which.min(dis))

find_nearest_node <- function(graph, x_given, y_given) {
  distances <- sqrt((V(grafo)$y - y_given)^2 + (V(grafo)$x - x_given)^2)
  nearest_node_index <- as.numeric(which.min(distances))
  
  return(V(graph)[nearest_node_index])
}

start_node <- find_nearest_node(grafo, coords["x"][[1]], coords["y"][[1]])
start_node$x
start_node$y

# Calcular los nodos más cercanos a start_node

distancias <- distances(grafo, start_node, V(grafo), weights = E(grafo)$length)

class(distancias)
colnames(distancias)
rownames(distancias)

sort(distancias)
which(distancias <= 300)


nodos_cercanos <- V(grafo)[which(distancias <= 300)]

caminos_cercanos <- shortest_paths(grafo, start_node, nodos_cercanos, weights = E(grafo)$length)


subgrafo <- induced_subgraph(grafo, nodos_cercanos)

edge_attr_names(subgrafo)
E(subgrafo)$osmid

edge_list <- as_edgelist(subgrafo)
edge_list
m = leaflet() %>% addTiles()

edges_data <- data.frame(longitude=c(), latitude=c())

for (i in 1:nrow(edge_list)) {
  edge <- edge_list[i,]
  head_edge <- V(subgrafo)[edge[1]]
  tail_edge <- V(subgrafo)[edge[2]]
  
  
  m <- m %>%
    addPolylines(lng = c(head_edge$x, tail_edge$x), lat = c(head_edge$y, tail_edge$y))
  
}

# add nodes to map

m <- m %>% addCircleMarkers(lng = V(subgrafo)$x, lat = V(subgrafo)$y, radius = 5, color = "red")

# add start node to map
m <- m %>% addCircleMarkers(lng = start_node$x, lat = start_node$y, radius = 5, color = "yellow")
m


