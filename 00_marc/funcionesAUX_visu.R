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

plot_isochron <- function(subgrafo, start_node, gtfs.stop, dist) {
  
  edge_list <- as_edgelist(subgrafo)
  
  m = leaflet() %>% addTiles()
  
  for (i in 1:nrow(edge_list)) {
    edge <- edge_list[i,]
    head_edge <- V(subgrafo)[edge[1]]
    tail_edge <- V(subgrafo)[edge[2]]
    
    m <- m %>%
      addPolylines(lng = c(head_edge$x, tail_edge$x), lat = c(head_edge$y, tail_edge$y))
  }
  
  # add nodes to map
  #m <- m %>% addCircleMarkers(lng = V(subgrafo)$x, lat = V(subgrafo)$y, radius = 5, color = "red")
  
  lista_iconos1 <- icons(iconUrl = "https://cdn-icons-png.freepik.com/512/4225/4225606.png", NA,
                         iconWidth = c(45), iconHeight = c(45))
  
  m <- m %>% addMarkers(lng = start_node$x, lat = start_node$y, popup = "Nodo Inicial", icon = lista_iconos1)
  
  #añadimos los puntos de las paradas de metrovalencia
  
  X <- gtfs.stop[gtfs.stop$distance_to_start < dist,]$x
  Y <- gtfs.stop[gtfs.stop$distance_to_start < dist,]$y
  
  lista_iconos <- icons(iconUrl = "https://cdn-icons-png.freepik.com/512/684/684908.png", NA,
                        iconWidth = c(40), iconHeight = c(40))
  
  m <- m %>% addMarkers(lng = X, lat = Y, popup = paste0(gtfs.stop[gtfs.stop$distance_to_start < dist,]$station_name, "-METRO"), icon = lista_iconos)
  
  return(m)
}


get_gtfs_distance <- function(gtfs, grafo, nodo_entrada, distance) {
  
  gtfs.stops.distance <- data.frame(station_id = character(), station_name = character(), point_id = character(), x = numeric(), y = numeric(),
                                    distance_to_start = numeric())
  
  for (i in 1:nrow(gtfs$stops)) {
    stop <- gtfs$stops[i,]
    
    punto_mas_cercano <- find_nearest_node(grafo, stop$stop_lon, stop$stop_lat)
    
    distancia <- distances(grafo, nodo_entrada, punto_mas_cercano, weights = E(grafo)$length)
    
    gtfs.stops.distance <- rbind(gtfs.stops.distance, data.frame(station_id = stop$stop_id, station_name = stop$stop_name, point_id = 
                                                                   punto_mas_cercano$id, x = punto_mas_cercano$x, y = punto_mas_cercano$y,
                                                                 distance_to_start = distancia))
  }
  
  return(gtfs.stops.distance[gtfs.stops.distance$distance_to_start < distance,])
}


find_nearest_node <- function(grafo, x_given, y_given) {
  distances <- sqrt((as.numeric(V(grafo)$y) - y_given)^2 + (as.numeric(V(grafo)$x) - x_given)^2)
  
  nearest_node_index <- as.numeric(which.min(distances))
  
  return(V(grafo)[nearest_node_index])
}

get_start_node <- function(grafo, location_address) {
  coords_bb <- getbb(location_address)
  coords <- list(x=mean(coords_bb["x", ]), y=mean(coords_bb["y", ]))
  start_node <- find_nearest_node(grafo, coords["x"][[1]], coords["y"][[1]])
  return(start_node)
}

add_path_to_map <- function(m, grafo, path) {
  for (i in 1:(length(path) - 1)) {
    node1 <- V(grafo)[path[i]]
    node2 <- V(grafo)[path[i + 1]]
    m <- m %>%
      addPolylines(lng = c(node1$x, node2$x), lat = c(node1$y, node2$y), color = "red")
  }
  return(m)
}

connect_nearest_nodes <- function(grafo, x_given, y_given) {
  nodo <- find_nearest_node(grafo, x_given, y_given)
  distances <- sqrt((as.numeric(V(grafo)$y) - y_given)^2 + (as.numeric(V(grafo)$x) - x_given)^2)
  nearest_node_indices <- order(distances)[1:2]
  nearest_nodes <- V(grafo)[nearest_node_indices]
  
  # Conectar los nodos más cercanos con el nodo adicional mediante aristas
  grafo <- add_edges(grafo, edges = cbind(rep(nodo, 2), nearest_nodes))
}

plot_isochronBUS <- function(subgrafo, start_node, gtfs.stop, dist) {
  
  edge_list <- as_edgelist(subgrafo)
  
  m = leaflet() %>% addTiles()
  
  for (i in 1:nrow(edge_list)) {
    edge <- edge_list[i,]
    head_edge <- V(subgrafo)[edge[1]]
    tail_edge <- V(subgrafo)[edge[2]]
    
    m <- m %>%
      addPolylines(lng = c(head_edge$x, tail_edge$x), lat = c(head_edge$y, tail_edge$y))
  }
  
  # add nodes to map
  #m <- m %>% addCircleMarkers(lng = V(subgrafo)$x, lat = V(subgrafo)$y, radius = 5, color = "red")
  lista_iconos1 <- icons(iconUrl = "https://cdn-icons-png.freepik.com/512/4225/4225606.png", NA,
                         iconWidth = c(45), iconHeight = c(45))
  
  m <- m %>% addMarkers(lng = start_node$x, lat = start_node$y, popup = "Nodo Inicial", icon = lista_iconos1)
  #añadimos los puntos de las paradas de metrovalencia
  
  X <- gtfs.stop[gtfs.stop$distance_to_start < dist,]$x
  Y <- gtfs.stop[gtfs.stop$distance_to_start < dist,]$y
  #NEGRO BUS "https://cdn.icon-icons.com/icons2/916/PNG/512/Marker_icon-icons.com_71852.png"
  #"https://cdn-icons-png.freepik.com/512/684/684908.png" ROJO METRO
  lista_iconos <- icons(iconUrl = "https://cdn.icon-icons.com/icons2/916/PNG/512/Marker_icon-icons.com_71852.png", NA,
                        iconWidth = c(60), iconHeight = c(60))
  
   
  m <- m %>% addMarkers(lng = X, lat = Y, popup = paste0(gtfs.stop[gtfs.stop$distance_to_start < dist,]$station_name, "-BUS"), icon = lista_iconos)
  
  return(m)
}

add_path_to_mapBUS <- function(m, grafo, path) {
  for (i in 1:(length(path) - 1)) {
    node1 <- V(grafo)[path[i]]
    node2 <- V(grafo)[path[i + 1]]
    m <- m %>%
      addPolylines(lng = c(node1$x, node2$x), lat = c(node1$y, node2$y), color = "black")
  }
  return(m)
}
