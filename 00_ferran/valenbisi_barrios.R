library(leaflet)
library(igraph)
library(osmdata)
library(sf)
library(ggplot2)


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


est_barrio <- data.frame()


for (est in 1:nrow(puntos_estaciones)) {
  point <- puntos_estaciones$geometry[est]
  b <- unlist(st_within(point, polygons_barrios$geometry)) # recive un punto con la lista de poligonos 
                                         # y devuelve el indice del poligono en el que está el nodo
  if (length(b) > 0) {
    est_barrio <- rbind(est_barrio, data.frame(polygons_barrios$Nombre[b],
                                               puntos_estaciones$direccion[est],
                                               point,
                                               polygons_barrios$geometry[b]))
  }else {
    est_barrio <- rbind(est_barrio, data.frame(polygons_barrios$Nombre[b],
                                               'Otro',
                                               point,
                                               NULL))
  }
}

names(est_barrio) <- c("barrio", "estacion", "geometry_estacion", "geometry_barrio")


num_estaciones <- est_barrio %>% group_by(barrio) %>% summarise(num_estaciones = n())


#metros_carril <- aristas %>% group_by(barrio) %>% summarise(metros_carril = sum(length))
