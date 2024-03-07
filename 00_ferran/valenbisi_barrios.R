library(leaflet)
library(igraph)
library(osmdata)
library(sf)
library(ggplot2)

estaciones <- read.csv2("00_ferran/estaciones_valenbisi.csv", sep = ";")

coord_est <-  estaciones$coords

coord_estaciones <- data.frame(matrix(unlist(strsplit(coord_est, ",")),
                                      ncol = 2, byrow = TRUE))

colnames(coord_estaciones) <- c("lat", "lng")

coord_estaciones$lat <- as.numeric(coord_estaciones$lat)
coord_estaciones$lng <- as.numeric(coord_estaciones$lng)

puntos_estaciones <- st_as_sf(coord_estaciones, coords = c("lng", "lat"), crs = 4326)

puntos_estaciones$direccion <- estaciones$Direccion


barrios <- read.csv2("data/barrios_valencia.csv", sep = ";")


polygons <- lapply(barrios$geo_shape, function(x) st_read(x, quiet = TRUE))

# Combina todos los objetos sf en un solo objeto sf
polygons_barrios <- do.call(rbind, polygons)

polygons_barrios$Nombre <- barrios$Nombre


est_barrio <- data.frame()


for (est in 1:nrow(puntos_estaciones)) {
  point <- puntos_estaciones$geometry[est]
  b <- unlist(st_within(point, polygons_barrios$geometry)) # recive un punto con la lista de poligonos 
                                         # y devuelve el indice del poligono en el que está el nodo
  if (length(b) > 0) {
    est_barrio <- rbind(est_barrio, data.frame(id = est, 
                                               barrio = polygons_barrios$Nombre[b],
                                               estacion = puntos_estaciones$direccion[est],
                                               punto = point))
  }else {
    est_barrio <- rbind(est_barrio, data.frame(id = est, 
                                               barrio = polygons_barrios$Nombre[b],
                                               estacion = 'Otro',
                                               punto = point))
  }
}






