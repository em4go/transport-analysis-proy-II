library(igraph)
library(sf)
library(jsonlite)
library(dplyr)


find_nearest_node <- function(graph, x_given, y_given) {
  distances <- sqrt((V(grafo)$y - y_given)^2 + (V(grafo)$x - x_given)^2)
  nearest_node_index <- as.numeric(which.min(distances))
  
  return(V(graph)[nearest_node_index])
}

obtener_poligonos <- function(df){
  polygons <- lapply(barrios$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  polygons_barrios <- do.call(rbind, polygons)
  
  polygons_barrios$Nombre <- barrios$Nombre
  return(polygons_barrios)
}



grafo <- read_graph("data/networks_data/valencia_bike.graphml", format = "graphml")
V(grafo)$x <- as.numeric(V(grafo)$x)
V(grafo)$y <- as.numeric(V(grafo)$y)
E(grafo)$length <- as.numeric(E(grafo)$length)



estaciones <- read.csv2("00_ferran/estaciones_valenbisi.csv")

'
puntos <- lapply(estaciones$geo_shape, function(x) st_read(x, quiet = TRUE))

puntos_estaciones <- do.call(rbind, puntos)

puntos_estaciones$direccion <- estaciones$Direccion

for (i in 1:length(puntos_estaciones$geometry)) {
  coord <- st_coordinates(puntos_estaciones$geometry[i])
  x <- coord[1]
  y <- coord[2]
  puntos_estaciones$nodos[i] <- find_nearest_node(grafo, x, y)
}
'


barrios <- read.csv2("data/barrios_valencia.csv")


# Hay que pasarle un dataframe con las variable geo_shape y Nombre obligatorias
polygons_barrios <- obtener_poligonos(barrios) 



aristas <- get.data.frame(grafo, what = "edges")

aristas <- aristas %>% select(from, to, length, geometry)

aux <- data.frame()


for (carril in 1:nrow(aristas)) {
  from <- aristas$from[carril]
  to <- aristas$to[carril]
  
  from_point <- st_point(c(V(grafo)[from]$x, V(grafo)[from]$y))
  to_point <- st_point(c(V(grafo)[to]$x, V(grafo)[to]$y))
  
  b_from <- unlist(st_within(from_point, polygons_barrios$geometry)) # recive un punto con la lista de poligonos
  b_to <- unlist(st_within(to_point, polygons_barrios$geometry))     # y devuelve el indice del poligono en el que está el nodo
                                                                     # si no esta devuelve un elemento vacio
  if (length(b_from) > 0 && length(b_to) > 0  && b_from == b_to) {
    aux <- rbind(aux, data.frame(barrio = polygons_barrios$Nombre[b_from]))
  }else {
    aux <- rbind(aux, data.frame(barrio = NA))
  }
}

aristas <- cbind(aristas, aux)

metros_carril <- aristas %>% group_by(barrio) %>% summarise(metros_carril = sum(length))



