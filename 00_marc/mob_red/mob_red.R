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
data_mob <- read.csv('./parkings/dataPre/aparcaments-persones-mobilitat-reduida.csv', header = TRUE, sep = ';')
data_mob

mob_red <- separate(data_mob, geo_point_2d, into = c("Latitude", "Longitude"), sep = ",")
mob_red$Latitude <- as.numeric(mob_red$Latitude)
mob_red$Longitude <- as.numeric(mob_red$Longitude)

plazas <- lapply(mob_red$geo_shape, function(x) st_read(x, quiet = TRUE))

plazas_mob <- do.call(rbind, plazas)

a <- obtener_poligonos(data_dist)
a

mapa <- leaflet() %>%
  addTiles() %>%
  setView(lng = -0.3763, lat = 39.4699, zoom = 12)

colorpal <- colorFactor(palette = "Set3", domain = a$Nombre)

mapa <- mapa %>%
  addMarkers(data = mob_red, lng = ~Longitude, lat = ~Latitude, popup = "Plaza Mobilidad Reducida", clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = a, color = ~colorpal(Nombre), fillOpacity = 0.6, popup = ~Nombre)

mapa

unique(mob_red$Nombre.Places...Número.Plazas)

table(mob_red$Nombre.Places...Número.Plazas)

est_dist <- data.frame()

point <- plazas_mob$geometry[1]
class(st_within(point, a$geometry))
b <- unlist(st_within(point, a$geometry)) 
class(b)

# Create an empty data frame to store results
est_dist <- data.frame()
# Iterate over each plaza
for (pl in 1:nrow(plazas_mob)) {
  
  point <- plazas_mob$geometry[pl]
  b <- unlist(st_within(point, a$geometry)) 
  
  if (length(b) > 0) {
    est_dist <- rbind(est_dist, data.frame(
      id_plaza = mob_red$OBJECTID[pl],
      n_plazas = mob_red$Nombre.Places...Número.Plazas[pl],
      districto = a$Nombre[b],
      latitud = mob_red$Latitude[pl],
      longitud = mob_red$Longitude[pl]
    ))
  } else {
    est_dist <- rbind(est_dist, data.frame(
      id_plaza = NA,
      n_plazas = NA,
      districto = 'Otro',
      latitud = NA,
      longitud = NA
    ))
  }
}

# Convert id_plaza and n_plazas to appropriate types
est_dist$id_plaza <- as.integer(est_dist$id_plaza)
est_dist$n_plazas <- as.integer(est_dist$n_plazas)
est_dist$districto <- as.character(est_dist$districto)

  
est_dist

est_dist <- subset(est_dist, districto != 'Otro')
est_dist

c <-est_dist$districto == 'Otro'
est_dist[c,]

resultado <- aggregate(n_plazas ~ districto, data = est_dist, FUN=sum)

resultado$n_plazas[resultado$districto == "L'OLIVERETA"]

write.csv(mob_red, "./parkings/dataTratada/plazas_mo.csv")

write.csv(est_dist, "./parkings/dataTratada/plazas_mobred_geo.csv")
write_parquet(est_dist, "./parkings/dataTratada/plazas_mobred_geo.parquet")

write.csv(resultado, "./parkings/dataTratada/plazas_mobred_dist.csv")
write_parquet(resultado, "./parkings/dataTratada/plazas_mobred_dist.parquet")
