library(leaflet)
library(dplyr)
library(tidyr)
library(sf)
library(jsonlite)
library(arrow)

obtener_poligonos <- function(df){
  polygons <- lapply(df$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  polygons_barrios <- do.call(rbind, polygons)
  
  polygons_barrios$Nombre <- df$Nombre
  return(polygons_barrios)
}

data_dist <-read.csv('./GitHub/transport-analysis-proy-II/data/distritos_valencia.csv', header = TRUE, sep = ';')
data_dist

parkings <- read.csv('./GitHub/transport-analysis-proy-II/00_marc/dataTratada/dataParkings.csv')
parkings

poligons <- obtener_poligonos(data_dist)
poligons

park_dist <- data.frame()
# Iterate over each plaza
for (pl in 1:nrow(parkings)) {
  point <- st_point(c(as.numeric(parkings$Longitude[pl]), as.numeric(parkings$Latitude[pl])), dim = "XY")
  b <- unlist(st_within(point, poligons$geometry))
  
  if (length(b) > 0) {
    park_dist <- rbind(park_dist, data.frame(
      nombre = parkings$Name[pl],
      plazas_parking = parkings$Total.parking.spaces[pl],
      districto = poligons$Nombre[b]
    ))
  } else {
    park_dist <- rbind(park_dist, data.frame(
      nombre = parkings$Name[pl],
      plazas_parking = parkings$Total.parking.spaces[pl],
      districto = NA
    ))
  }
}

park_dist

park_dist$plazas_parking <- as.integer(park_dist$plazas_parking)

resultado <- aggregate(plazas_parking ~ districto, data = park_dist, FUN=sum)

resultado$plazas_parking[resultado$districto == "L'OLIVERETA"]

write.csv(resultado, './GitHub/transport-analysis-proy-II/00_marc/dataTratada/dataParkings_por_dist.csv')
