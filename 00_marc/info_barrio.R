library(leaflet)
library(dplyr)
library(tidyr)
library(sf)
library(jsonlite)
library(arrow)


data_barrios <- read.csv2("./data/barrios_valencia.csv", sep = ";")

data_mob_red <- read.csv("./00_marc/dataTratada/plazas_mobred_geo.csv", sep = ",")
  
data_parkings <- read.csv("./00_marc/dataTratada/dataParkings.csv", sep = ",")

data_taxis <- read.csv2("./00_marc/dataPre/data_taxis.csv", sep = ";")

data_taxis <- separate(data_taxis, geo_point_2d, into = c("Latitude", "Longitude"), sep = ",")
data_taxis$Latitude <- as.numeric(data_taxis$Latitude)
data_taxis$Longitude <- as.numeric(data_taxis$Longitude)

obtener_poligonos <- function(df){
  polygons <- lapply(df$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  polygons_barrios <- do.call(rbind, polygons)
  
  polygons_barrios$Nombre <- df$Nombre
  return(polygons_barrios)
}

poligons_barrios <- obtener_poligonos(data_barrios)
poligons_barrios

plazas <- data.frame()
# Iterate over each plaza
for (pl in 1:nrow(data_mob_red)) {
  point <- st_point(c(as.numeric(data_mob_red$longitud[pl]), as.numeric(data_mob_red$latitud[pl])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligons_barrios$geometry)) 
  
  if (length(b) > 0) {
    plazas <- rbind(plazas, data.frame(
      id_plaza = data_mob_red$id_plaza[pl],
      plazas_mob = data_mob_red$n_plazas[pl],
      barrio = poligons_barrios$Nombre[b]
    ))
  } else {
    plazas <- rbind(plazas, data.frame(
      id_plaza = NA,
      plazas_mob = NA,
      barrrio = "OTRO"
    ))
  }
}


resultado_plazas_mob <- aggregate(plazas_mob ~ barrio, data = plazas, FUN=sum)

parkings <- data.frame()
for (pl in 1:nrow(data_parkings)) {
  point <- st_point(c(as.numeric(data_parkings$Longitude[pl]), as.numeric(data_parkings$Latitude[pl])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligons_barrios$geometry)) 
  
  if (length(b) > 0) {
    parkings <- rbind(parkings, data.frame(
      id_plaza = data_parkings$Id[pl],
      plazas_parkings = data_parkings$Total.parking.spaces[pl],
      barrio = poligons_barrios$Nombre[b]
    ))
  } else {
    parkings <- rbind(parkings, data.frame(
      id_plaza = NA,
      plazas_parkings = NA,
      barrio = "OTRO"
    ))
  }
}

resultado_parkings <- aggregate(plazas_parkings ~ barrio, data = parkings, FUN=sum)

taxis <- data.frame()
for (pl in 1:nrow(data_taxis)) {
  point <- st_point(c(as.numeric(data_taxis$Longitude[pl]), as.numeric(data_taxis$Latitude[pl])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligons_barrios$geometry)) 
  
  if (length(b) > 0) {
    taxis <- rbind(taxis, data.frame(
      id_plaza = data_taxis$OBJECTID[pl],
      paradas_taxis = 1,
      barrio = poligons_barrios$Nombre[b]
    ))
  } else {
    taxis <- rbind(taxis, data.frame(
      id_plaza = NA,
      paradas_taxis = 0,
      barrrio = "OTRO"
    ))
  }
}

resultado_taxis <- aggregate(paradas_taxis ~ barrio, data = taxis, FUN=sum)


info_barrio <- data.frame('barrio' = data_barrios$Nombre)

info_barrio <- merge(info_barrio, resultado_plazas_mob, by = "barrio", all.x = TRUE)
info_barrio <- merge(info_barrio, resultado_taxis, by = "barrio", all.x = TRUE)
info_barrio <- merge(info_barrio, resultado_parkings, by = "barrio", all.x = TRUE)
info_barrio

write.csv(info_barrio, file = "./00_marc/dataTratada/info_barrio.csv", row.names = FALSE)
write_parquet(info_barrio,"./00_marc/dataTratada/info_barrio.parquet")
