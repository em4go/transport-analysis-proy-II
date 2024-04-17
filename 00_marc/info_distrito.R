library(leaflet)
library(dplyr)
library(tidyr)
library(sf)
library(jsonlite)
library(arrow)
library(stringi)
library(plyr)

obtener_poligonos <- function(df){
  polygons <- lapply(df$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  polygons_barrios <- do.call(rbind, polygons)
  
  polygons_barrios$Nombre <- df$Nombre
  return(polygons_barrios)
}

data_distrito <- read.csv2("./data/distritos_valencia.csv", sep = ";")

data_hervas <- read.csv2('./00_hervas/Distritos_pob.csv', sep = ';')

data_taxis <- read.csv2("./00_marc/dataPre/data_taxis.csv", sep = ";")

data_taxis <- separate(data_taxis, geo_point_2d, into = c("Latitude", "Longitude"), sep = ",")
data_taxis$Latitude <- as.numeric(data_taxis$Latitude)
data_taxis$Longitude <- as.numeric(data_taxis$Longitude)

data_mob_red <- read.csv("./00_marc/dataTratada/plazas_mobred_geo.csv", sep = ",")

data_parkings <- read.csv("./00_marc/dataTratada/dataParkings.csv", sep = ",")

data_plazas_ora_coches <- read.csv('./00_marc/dataTratada/dataPlazasAparcamiento.csv')

poligons_distritos <- obtener_poligonos(data_distrito)
poligons_distritos

plazas_mob <- data.frame()
# Iterate over each plaza
for (pl in 1:nrow(data_mob_red)) {
  point <- st_point(c(as.numeric(data_mob_red$longitud[pl]), as.numeric(data_mob_red$latitud[pl])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligons_distritos$geometry)) 
  
  if (length(b) > 0) {
    plazas_mob <- rbind(plazas_mob, data.frame(
      id_plaza = data_mob_red$id_plaza[pl],
      plazas_mob = data_mob_red$n_plazas[pl],
      distritos = poligons_distritos$Nombre[b]
    ))
  } else {
    plazas_mob <- rbind(plazas_mob, data.frame(
      id_plaza = NA,
      plazas_mob = NA,
      distritos = poligons_distritos$Nombre[b]
    ))
  }
}

resultado_plazas_mob <- aggregate(plazas_mob ~ distritos, data = plazas_mob, FUN=sum)

parkings <- data.frame()
for (pl in 1:nrow(data_parkings)) {
  point <- st_point(c(as.numeric(data_parkings$Longitude[pl]), as.numeric(data_parkings$Latitude[pl])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligons_distritos$geometry)) 
  
  if (length(b) > 0) {
    parkings <- rbind(parkings, data.frame(
      id_plaza = data_parkings$Id[pl],
      plazas_parkings = data_parkings$Total.parking.spaces[pl],
      distritos = poligons_distritos$Nombre[b]
    ))
  } else {
    parkings <- rbind(parkings, data.frame(
      id_plaza = NA,
      plazas_parkings = NA,
      distritos = "OTRO"
    ))
  }
}

resultado_parkings <- aggregate(plazas_parkings ~ distritos, data = parkings, FUN=sum)

taxis <- data.frame()
for (pl in 1:nrow(data_taxis)) {
  point <- st_point(c(as.numeric(data_taxis$Longitude[pl]), as.numeric(data_taxis$Latitude[pl])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligons_distritos$geometry)) 
  
  if (length(b) > 0) {
    taxis <- rbind(taxis, data.frame(
      id_plaza = data_taxis$OBJECTID[pl],
      paradas_taxis = 1,
      distritos = poligons_distritos$Nombre[b]
    ))
  } else {
    taxis <- rbind(taxis, data.frame(
      id_plaza = NA,
      paradas_taxis = 0,
      distritos = "OTRO"
    ))
  }
}

resultado_taxis <- aggregate(paradas_taxis ~ distritos, data = taxis, FUN=sum)

data_ora_coches <- data.frame("distritos" = data_plazas_ora_coches$Disrtict, "plazas_ora" = data_plazas_ora_coches$Total, "coches" = data_plazas_ora_coches$Tourism)

info_distrito <- data.frame('distritos' = data_plazas_ora_coches$Disrtict)

info_distrito <- merge(info_distrito, resultado_plazas_mob, by = "distritos", all.x = TRUE)
info_distrito <- merge(info_distrito, resultado_taxis, by = "distritos", all.x = TRUE)
info_distrito <- merge(info_distrito, resultado_parkings, by = "distritos", all.x = TRUE)
info_distrito <- merge(info_distrito, data_ora_coches, by = "distritos", all.x = TRUE)
info_distrito

filtrados <- data_hervas[2:20,]

#Eliminamos el ". " de la columna X (disrito)
filtrados$X <- sapply(strsplit(filtrados$X, "\\. "), function(x) x[2])
filtrados$X

dist <- toupper(stri_trans_general(filtrados$X, "Latin-ASCII"))
dist

filtrados$distritos <- dist
filtrados <- select(filtrados, -c(X))
filtrados 

filtrados$distritos <- revalue(filtrados$distritos, c("POBLES DEL NORD"="POBLATS DEL NORD", "POBLES DE L'OEST"="POBLATS DE L'OEST", "POBLES DEL SUD"="POBLATS DEL SUD"))
filtrados$distritos

info_distrito <- merge(info_distrito, filtrados, by = "distritos", all.x = TRUE)
info_distrito

write.csv(info_distrito, file = "./00_marc/dataTratada/info_barrio.csv", row.names = FALSE)
write_parquet(info_distrito,"./00_marc/dataTratada/info_barrio.parquet")
