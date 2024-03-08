library(leaflet)
library(igraph)
library(osmdata)
library(sf)
library(ggplot2)

estaciones <- read.csv2("00_ferran/estaciones_valenbisi.csv")


puntos <- lapply(estaciones$geo_shape, function(x) st_read(x, quiet = TRUE))

puntos_estaciones <- do.call(rbind, puntos)

puntos_estaciones$direccion <- estaciones$Direccion


barrios <- read.csv2("data/distritos_valencia.csv", sep = ";")


polygons <- lapply(barrios$geo_shape, function(x) st_read(x, quiet = TRUE))

# Combina todos los objetos sf en un solo objeto sf
polygons_barrios <- do.call(rbind, polygons)

polygons_barrios$Nombre <- barrios$Nombre


est_barrio <- data.frame()


for (est in 1:nrow(puntos_estaciones)) {
  point <- puntos_estaciones$geometry[est]
  b <- unlist(st_within(point, polygons_barrios$geometry)) # recive un punto con la lista de poligonos 
                                         # y devuelve el indice del poligono en el que está el nodo
  if (b > 0) {
    est_barrio <- rbind(est_barrio, data.frame(barrio = polygons_barrios$Nombre[b],
                                               estacion = puntos_estaciones$direccion[est],
                                               geometry_est = point,
                                               geometry_barrio = polygons_barrios$geometry[b]))
  }else {
    est_barrio <- rbind(est_barrio, data.frame(barrio = polygons_barrios$Nombre[b],
                                               estacion = 'Otro',
                                               loc_estación = point))
  }
}






