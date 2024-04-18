.lib<- c("igraph","osmdata", "dplyr", "leaflet")


.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst])
lapply(.lib, require, character.only=TRUE)

source("01_main/utils.R")

coords_bb <- getbb("Plaza de toros de Valencia")
rownames(coords_bb)
coords_bb["x", ]

# center of the bounding box
coords <- list(x=mean(coords_bb["x", ]), y=mean(coords_bb["y", ]))
coords


# Cargar los datos
grafo <- read_graph("./data/networks_data/valencia_drive.graphml", format = "graphml")


is_directed(grafo)

V(grafo)$x <- as.numeric(V(grafo)$x)
V(grafo)$y <- as.numeric(V(grafo)$y)
V(grafo)$x
V(grafo)$y

E(grafo)$length <- as.numeric(E(grafo)$length)

E


coords["x"][[1]]
dis <- sqrt((V(grafo)$y - coords["y"][[1]])^2 + (V(grafo)$x - coords["x"][[1]])^2)
as.numeric(which.min(dis))


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

# Inicializa un vector para almacenar las aristas únicas
aristas_unicas <- c()

# Itera sobre cada camino en vpath
for (camino in caminos_cercanos$vpath) {
  # Verifica si el camino tiene más de un vértice
  if(length(camino) > 1) {
    # Procesa solo los caminos con más de un vértice
    for (i in 1:(length(camino) - 1)) {
      # Encuentra la arista que conecta el par de vértices actual
      arista_actual <- get.edge.ids(grafo, c(camino[i], camino[i+1]), directed = FALSE)
      # Agrega la arista a la lista de aristas únicas si no está ya incluida
      aristas_unicas <- union(aristas_unicas, arista_actual)
    }
  }
}

# Crea un subgrafo con las aristas seleccionadas
subgrafo <- subgraph.edges(grafo, eids = aristas_unicas, delete.vertices = TRUE)

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


# create function to get the subgraph containing the places where the user can
# go based on a certain metric (for example, the distance), which will be used as
# the weight for the shortest_paths algortihm

isochron <- function(graph, start_node_x, start_node_y, metric, threshold) {
  # Conseguir el nodo más cercano a las coordenadas de inicio
  start_node <- find_nearest_node(graph, start_node_x, start_node_y)
  
  # Calcular los nodos más cercanos a start_node
  distancias <- distances(graph, start_node, V(graph), weights = metric)
  nodos_cercanos <- V(graph)[which(distancias <= threshold)]
  caminos_cercanos <- shortest_paths(graph, start_node, nodos_cercanos, weights = metric)
  
  # Inicializa un vector para almacenar las aristas únicas
  aristas_unicas <- c()
  
  # Itera sobre cada camino en vpath
  for (camino in caminos_cercanos$vpath) {
    # Verifica si el camino tiene más de un vértice
    if(length(camino) > 1) {
      # Procesa solo los caminos con más de un vértice
      for (i in 1:(length(camino) - 1)) {
        # Encuentra la arista que conecta el par de vértices actual
        arista_actual <- get.edge.ids(graph, c(camino[i], camino[i+1]), directed = FALSE)
        # Agrega la arista a la lista de aristas únicas si no está ya incluida
        aristas_unicas <- union(aristas_unicas, arista_actual)
      }
    }
  }
  
  # Crea un subgrafo con las aristas seleccionadas
  subgrafo <- subgraph.edges(graph, eids = aristas_unicas, delete.vertices = TRUE)
  
  return(subgrafo)
}