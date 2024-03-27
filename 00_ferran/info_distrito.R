library(leaflet)
library(igraph)
library(osmdata)
library(sf)
library(ggplot2)
library(jsonlite)
library(dplyr)
library(arrow)
library(geojsonsf)
library(tidyr)
library(arrow)
library(tidytransit)
library(stringi)


establecer_localizacion <- function(punto, info_poligonos) {
  # Vamos a crear una función para que devuelva directamente el poligono al que
  # pertenece un punto
  b <- unlist(st_within(punto, info_poligonos$geometry))
  if (length(b) == 0) {
    return(NA)
  } else {
    return(info_poligonos$distrito[b])
  }
}



# vamos a crear un dataframe con la información de todos los distritos de valencia,
# ya se información a cerca de servicios públicos como terreno o población.



# Lectura del fichero con información del valenbici por distritos de valencia
valenba_distrito <- read_parquet("data/valenbisi_distrito.parquet")

# Fichero con los datos de población por distrito
poblacion <- read.csv2("00_hervas/Distritos_Pob.csv", sep = ";", header = TRUE, encoding = "UTF-8")

# Eliminación de la primera fila con el valor de total, para quedarnos con los distritos
poblacion <- poblacion[2:20,]

# Cambio de nombre de la columna para poder realizar el merge, necesario quitar los acentos
poblacion$distrito <- toupper(sapply(strsplit(stri_trans_general(poblacion$X, "Latin-ASCII"), "\\. "), function(x) x[2]))

poblacion <- poblacion %>% select(-X)

poblacion$distrito[poblacion$distrito == "POBLES DEL NORD"] <- "POBLATS DEL NORD"
poblacion$distrito[poblacion$distrito == "POBLES DEL SUD"] <- "POBLATS DEL SUD"
poblacion$distrito[poblacion$distrito == "POBLES DE L'OEST"] <- "POBLATS DE L'OEST"

valenba_distrito <- merge(valenba_distrito, poblacion, by = "distrito", all.x = TRUE)




# OTROS DATOS DE LOS distritoS

valenba_distrito$geometry <- geojson_sf(valenba_distrito$geo_shape)

# datos sobre la superficie de los distritos
valenba_distrito$superficie_distrito <- st_area(valenba_distrito$geometry)

# datos sobre transporte público
emt <- read_gtfs("data/gtfs_data/transit_emt_valencia.zip")

paradas <- emt$stops
paradas$stop_lat <- as.numeric(paradas$stop_lat)
paradas$stop_lon <- as.numeric(paradas$stop_lon)

#Creación de un objeto de tipo sf con las paradas
coordenadas <- subset(paradas, select = c("stop_lat", "stop_lon"))

b <- st_as_sf(coordenadas, coords = c("stop_lon", "stop_lat"), crs = 4326)
paradas <- cbind(paradas, b)

# Añadimos al data.frame de paradas la información de los distritos

paradas$distrito <- lapply(paradas$geometry, function(x) establecer_localizacion(x, valenba_distrito))


# Contamos cuantas paradas tiene cada distrito y lo añadimos al data.frame
emt_distrito <- paradas %>% group_by(distrito) %>% summarise(paradas_emt = n())

valenba_distrito <- merge(valenba_distrito, emt_distrito, by = "distrito", na.rm = TRUE, all.x = TRUE)

# Tratamos los posibles valores faltantes
valenba_distrito$paradas_emt[is.na(valenba_distrito$paradas_emt)] <- 0

# Datos sobre la superficie de carril bus






# Datos sobre el metro y tranvía

metro <- read_gtfs("data/gtfs_data/transit_metrovalencia.zip")

paradas_metro <- metro$stops

paradas_metro$stop_lat <- as.numeric(paradas_metro$stop_lat)
paradas_metro$stop_lon <- as.numeric(paradas_metro$stop_lon)

#Creación de un objeto de tipo sf con las paradas
coordenadas_metro <- subset(paradas_metro, select = c("stop_lat", "stop_lon"))

b <- st_as_sf(coordenadas_metro, coords = c("stop_lon", "stop_lat"), crs = 4326)
paradas_metro <- cbind(paradas_metro, b)

# Añadimos al data.frame de paradas la información de los distritos

paradas_metro$distrito <- lapply(paradas_metro$geometry, function(x) establecer_localizacion(x, valenba_distrito))


# Contamos cuantas paradas tiene cada distrito y lo añadimos al data.frame
metro_distrito <- paradas_metro %>% group_by(distrito) %>% summarise(paradas_metro = n())

valenba_distrito <- merge(valenba_distrito, metro_distrito, by = "distrito", all.x = TRUE, na.rm = TRUE)

# AL haber distritos sin metro se generan NA, los sustituimos por 0
valenba_distrito$paradas_metro[is.na(valenba_distrito$paradas_metro)] <- 0

# Datos sobre la superficie de vias de metro y tanvía






# DATOS DE PARKINGS PÚBLICOS EN LOS DISTRITOS

parkings <- read_parquet("00_marc/dataTratada/info_dist.parquet")
# Eliminación de los acentos y paso a mayúsculas
parkings$distrito <- toupper(stri_trans_general(parkings$distritos, "Latin-ASCII"))

prk2merge <- select(parkings, setdiff(colnames(parkings), colnames(valenba_distrito)))

valenba_distrito <- merge(valenba_distrito, prk2merge, by.x = "distrito", by.y = "distritos", all.x = TRUE, na.rm = TRUE)



# ADICIÓN DEL COSTE DE ALQULER POR DISTRITO

alquiler <- read.csv("data/precio-alquiler-vivienda.csv", sep = ";")

alquiler_dist <- select(alquiler, c("DISTRITO", "Precio_2022..Euros.m2."))
alquiler_dist <- group_by(alquiler_dist, DISTRITO) %>% summarise(precio_alquiler_m2 = mean(Precio_2022..Euros.m2.))

colnames(alquiler_dist) <- c("distrito", "precio_alquiler_m2")

valenba_distrito <- merge(valenba_distrito, alquiler_dist, by = "distrito", all.x = TRUE, na.rm = TRUE)






# GUARDADO DE LA INFORMACIÓN EN UN PARQUET
valenba_distrito <- valenba_distrito %>% select(-geometry)
#colnames(valenba_barrio) <- c("distrito", colnames(valenba_barrio)[2:length(colnames(valenba_barrio))])
# Ejecutar solo cuando se quiera guardar el parquet
write_parquet(valenba_distrito, "data/info_general_distrito.parquet")
write.csv(valenba_distrito, "data/info_general_distrito.csv", row.names = FALSE)

