library(osmdata)
library(dplyr)
library(sf)
library(leaflet)
library(dplyr)
library(arrow)


# Get all the tags available in the OSM database from differente categories
food <- c("bar", "biergarten", "cafe", "fast_food", "food_court", "ice_cream", "pub", "restaurant")
food <- data.frame(Key = "amenity", Value = food) # Create a data.frame for food with the columns Key and Value

# Create a data.frame for sports with the columns Key and Value
sports2 <-  available_tags(feature="sport")

sports <- rbind(sports, sports2) # Combine sports and sports2
sports
monuments <- available_tags(feature="historic")

shops <- c("department_store", "general", "kiosk", "mall", "supermarket", "wholesale")
shops <- data.frame(Key = "shop", Value = shops) # Create a data.frame for shops with the columns Key and Value

vlc_bb <- getbb("Valencia, Spain")


data_barrios <- read.csv2("./data/barrios_valencia.csv", sep = ";")

obtener_poligonos <- function(df){
  polygons <- lapply(df$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  polygons_barrios <- do.call(rbind, polygons)
  
  polygons_barrios$Nombre <- df$Nombre
  return(polygons_barrios)
}

poligonos_barrios<-obtener_poligonos(data_barrios)


#AMENITY
consulta_amenity <- vlc_bb%>% 
  opq() %>%
  add_osm_feature(key = 'amenity', value = c("bar", "biergarten", "cafe", "fast_food", "food_court", "ice_cream", "pub", "restaurant")) 


# Obtener los datos de OpenStreetMap (OSM)
datos_comida <- osmdata_sf(consulta_amenity)
lugares_comida<-datos_comida$osm_points

ubicaciones_comida <- st_coordinates(lugares_comida)
# Crear un data frame con la ubicación y el tipo de lugar de comida
data_comida <- data.frame(
  longitud = ubicaciones_comida[, "X"],
  latitud = ubicaciones_comida[, "Y"]
)
data_comida

amenity<-data.frame()
for (pt in 1:nrow(data_comida)) {
  point <- st_point(c(as.numeric(data_comida$longitud[pt]), as.numeric(data_comida$latitud[pt])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligonos_barrios$geometry)) 
  
  if (length(b) > 0) {
    amenity <- rbind(amenity, data.frame(
      barrio = poligonos_barrios$Nombre[b], longitud=data_comida$longitud[pt], latitud=data_comida$latitud[pt]
    ))
  } else {
    amenity <- rbind(amenity, data.frame( barrio = "OTRO", 
      longitud=data_comida$longitud[pt], latitud=data_comida$latitud[pt]
    ))
  }
}

frecuencias_amenity <- table(amenity$barrio)
frecuencias_amenity
# Convertir el resultado en un dataframe
resultado_amenity <- data.frame(nombre = names(frecuencias_amenity), frecuencia = as.vector(frecuencias_amenity))
resultado_amenity<- resultado_amenity[resultado_amenity$nombre != "OTRO", ]


#MONUMENTS
consulta_monuments <- vlc_bb%>% 
  opq() %>%
  add_osm_feature(key = 'historic', value = monuments$Value) 

datos_monuments <- osmdata_sf(consulta_monuments)
lugares_monuments<-datos_monuments$osm_points
ubicaciones_monuments <- st_coordinates(lugares_monuments)

data_monuments <- data.frame(
  longitud = ubicaciones_monuments[, "X"],
  latitud = ubicaciones_monuments[, "Y"]
)

monuments<-data.frame()
for (pt in 1:length(ubicaciones_monuments)) {
  point <- st_point(c(as.numeric(data_monuments$longitud[pt]), as.numeric(data_monuments$latitud[pt])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligonos_barrios$geometry)) 
  
  if (length(b) > 0) {
    monuments <- rbind(monuments, data.frame(
      barrio = poligonos_barrios$Nombre[b]
    ))
  } else {
    monuments <- rbind(monuments, data.frame(
      barrio = "OTRO"
    ))
  }
}
monuments
frecuencias_monuments <- table(monuments$barrio)

# Convertir el resultado en un dataframe
resultado_monuments <- data.frame(nombre = names(frecuencias_monuments), frecuencia = as.vector(frecuencias_monuments))
resultado_monuments<- resultado_monuments[resultado_monuments$nombre != "OTRO", ]



#SPORT
consulta_sport <- vlc_bb%>% 
  opq() %>%
  add_osm_feature(key = 'sport', value = sports2$Value) 

datos_sport <- osmdata_sf(consulta_sport)
lugares_sport<-datos_sport$osm_points
ubicaciones_sport <- st_coordinates(lugares_sport)


data_sport <- data.frame(
  longitud = ubicaciones_sport[, "X"],
  latitud = ubicaciones_sport[, "Y"]
)



sport<-data.frame()
for (pt in 1:nrow(data_sport)) {
  point <- st_point(c(as.numeric(data_sport$longitud[pt]), as.numeric(data_sport$latitud[pt])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligonos_barrios$geometry)) 
  
  if (length(b) > 0) {
    sport <- rbind(sport, data.frame(
      barrio = poligonos_barrios$Nombre[b]
    ))
  } else {
    sport <- rbind(sport, data.frame(
      barrio = "OTRO"
    ))
  }
}
sport
frecuencias_sport <- table(sport$barrio)

# Convertir el resultado en un dataframe
resultado_sport <- data.frame(nombre = names(frecuencias_sport), frecuencia = as.vector(frecuencias_sport))
resultado_sport
resultado_sport<- resultado_sport[resultado_sport$nombre != "OTRO", ]

#SHOP

consulta_shop <- vlc_bb%>% 
  opq() %>%
  add_osm_feature(key = 'shop', value = shops$Value) 

datos_shop <- osmdata_sf(consulta_shop)
lugares_shop<-datos_shop$osm_points
ubicaciones_shop <- st_coordinates(lugares_shop)


data_shop <- data.frame(
  longitud = ubicaciones_shop[, "X"],
  latitud = ubicaciones_shop[, "Y"]
)

nrow(data_shop)

shop<-data.frame()
for (pt in 1:nrow(data_shop)) {
  point <- st_point(c(as.numeric(data_shop$longitud[pt]), as.numeric(data_shop$latitud[pt])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligonos_barrios$geometry)) 
  
  if (length(b) > 0) {
    shop <- rbind(shop, data.frame(
      barrio = poligonos_barrios$Nombre[b]
    ))
  } else {
    shop <- rbind(shop, data.frame(
      barrio = "OTRO"
    ))
  }
}

frecuencias_shop <- table(shop$barrio)

# Convertir el resultado en un dataframe
resultado_shop <- data.frame(nombre = names(frecuencias_shop), frecuencia = as.vector(frecuencias_shop))
resultado_shop<- resultado_shop[resultado_shop$nombre != "OTRO", ]


write_parquet(resultado_amenity, "00_hervas/amenity.parquet")
write_parquet(resultado_monuments,"00_hervas/monuments.parquet")
write_parquet( resultado_sport,"00_hervas/sport.parquet")
write_parquet(resultado_shop,"00_hervas/shop.parquet")


