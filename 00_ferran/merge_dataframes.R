library(leaflet)
library(igraph)
library(osmdata)
library(sf)
library(ggplot2)
library(jsonlite)
library(dplyr)
library(geojsonsf)
library(arrow)



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




estaciones <- read.csv2("00_ferran/estaciones_valenbisi.csv")


puntos <- lapply(estaciones$geo_shape, function(x) st_read(x, quiet = TRUE))

puntos_estaciones <- do.call(rbind, puntos)

puntos_estaciones$direccion <- estaciones$Direccion


barrios <- read.csv2("data/barrios_valencia.csv", sep = ";")

polygons_barrios <- obtener_poligonos(barrios)

names(polygons_barrios) <- c("geometry", "barrio")


est_barrio <- data.frame()


for (est in 1:nrow(puntos_estaciones)) {
  point <- puntos_estaciones$geometry[est]
  b <- unlist(st_within(point, polygons_barrios$geometry)) # recive un punto con la lista de poligonos 
  # y devuelve el indice del poligono en el que está el nodo
  if (length(b) > 0) {
    est_barrio <- rbind(est_barrio, data.frame(polygons_barrios$barrio[b],
                                               puntos_estaciones$direccion[est],
                                               point,
                                               polygons_barrios$geometry[b]))
  }else {
    est_barrio <- rbind(est_barrio, data.frame(polygons_barrios$barrio[b],
                                               'Otro',
                                               point,
                                               NULL))
  }
}

names(est_barrio) <- c("barrio", "estacion", "geometry_estacion", "geometry_barrio")


num_estaciones <- est_barrio %>% group_by(barrio) %>% summarise(num_estaciones = n())

grafo <- read_graph("data/networks_data/valencia_bike.graphml", format = "graphml")
V(grafo)$x <- as.numeric(V(grafo)$x)
V(grafo)$y <- as.numeric(V(grafo)$y)
E(grafo)$length <- as.numeric(E(grafo)$length)


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
  if (length(b_from) > 0 && length(b_to) > 0) {
    if (b_from == b_to){
      aux <- rbind(aux, data.frame(barrio = polygons_barrios$barrio[b_from],
                                   longitud = aristas$length[carril]))
    }else {
    dist <- aristas$length[carril]/2
    
    aux <- rbind(aux, data.frame(barrio = polygons_barrios$barrio[b_from],
                                 longitud = dist))
    aux <- rbind(aux, data.frame(barrio = polygons_barrios$barrio[b_to],
                                 longitud = dist))
    }
  }else {
    aux <- rbind(aux, data.frame(barrio = "Others",
                                 longitud = aristas$length[carril]))
  }
}


metros_carril <- aux %>% group_by(barrio) %>% summarise(metros_carril = sum(longitud))

valenba_barrio <- merge(metros_carril, num_estaciones, by = "barrio", all = TRUE)

valenba_barrio[is.na(valenba_barrio)] <- 0

valenba_barrio <- merge(valenba_barrio, polygons_barrios, by = "barrio", all.y = T)

valenba_barrio$geo_shape <- sfc_geojson(valenba_barrio$geometry)

# Guardamos el dataframe en formato parquet

valenba_barrio <- valenba_barrio %>% select(-geometry)

write_parquet(valenba_barrio, "data/valenbisi_barrio.parquet")

