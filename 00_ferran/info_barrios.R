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
    return(info_poligonos$barrio[b])
  }
}

obtener_poligonos <- function(df){
  polygons <- lapply(distritos$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  poligonos <- do.call(rbind, polygons)
  
  poligonos$Nombre <- distritos$Nombre
  return(poligonos)
}



# vamos a crear un dataframe con la información de todos los barrios de valencia,
# ya se información a cerca de servicios públicos como terreno o población.



# Lectura del fichero con información del valenbici por barrios de valencia
valenba_barrio <- read_parquet("data/valenbisi_barrio.parquet")

# Fichero con los datos de población por barrio
poblacion <- read.csv2("00_hervas/Barrios_Pob.csv", sep = ";", header = TRUE, encoding = "UTF-8")

# Eliminación de los distritos
poblacion <- poblacion %>% filter(!grepl("^$",Total))

# Eliminación de la primera fila
poblacion <- poblacion %>% filter(!grepl("^$",X))

# Eliminación de la primera fila con el valor de total, para quedarnos con los barrios
poblacion <- poblacion[2:nrow(poblacion),]

# Cambio de nombre de la columna para poder realizar el merge
poblacion$barrio <- toupper(sapply(strsplit(stri_trans_general(poblacion$X,"Latin-ASCII"), "\\. "), function(x) x[2]))

poblacion <- poblacion %>% select(-c(X, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8))

colnames(poblacion) <- c("Total_poblacion", "0-15 años", "16-64 años", "65 o más", "barrio")

poblacion$Total_poblacion <- as.numeric(poblacion$Total_poblacion)
poblacion$`0-15 años` <- as.numeric(poblacion$`0-15 años`)
poblacion$`16-64 años` <- as.numeric(poblacion$`16-64 años`)
poblacion$`65 o más` <- as.numeric(poblacion$`65 o más`)

valenba_barrio <- merge(valenba_barrio, poblacion, by = "barrio", all.x = TRUE)




# OTROS DATOS DE LOS BARRIOS

valenba_barrio$geometry <- geojson_sf(valenba_barrio$geo_shape)

# datos sobre la superficie de los barrios
valenba_barrio$superficie_barrio <- st_area(valenba_barrio$geometry)

# datos sobre transporte público
emt <- read_gtfs("data/gtfs_data/transit_emt_valencia.zip")

paradas <- emt$stops
paradas$stop_lat <- as.numeric(paradas$stop_lat)
paradas$stop_lon <- as.numeric(paradas$stop_lon)

#Creación de un objeto de tipo sf con las paradas
coordenadas <- subset(paradas, select = c("stop_lat", "stop_lon"))

b <- st_as_sf(coordenadas, coords = c("stop_lon", "stop_lat"), crs = 4326)
paradas <- cbind(paradas, b)

# Añadimos al data.frame de paradas la información de los barrios

paradas$barrio <- lapply(paradas$geometry, function(x) establecer_localizacion(x, valenba_barrio))


# Contamos cuantas paradas tiene cada barrio y lo añadimos al data.frame
emt_barrio <- paradas %>% group_by(barrio) %>% summarise(paradas_emt = n())

valenba_barrio <- merge(valenba_barrio, emt_barrio, by = "barrio", na.rm = TRUE, all.x = T)

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

# Añadimos al data.frame de paradas la información de los barrios

paradas_metro$barrio <- lapply(paradas_metro$geometry, function(x) establecer_localizacion(x, valenba_barrio))


# Contamos cuantas paradas tiene cada barrio y lo añadimos al data.frame
metro_barrio <- paradas_metro %>% group_by(barrio) %>% summarise(paradas_metro = n())

valenba_barrio <- merge(valenba_barrio, metro_barrio, by = "barrio", all.x = TRUE, na.rm = TRUE)

# AL haber barrios sin metro se generan NA, los sustituimos por 0

#### valenba_barrio$paradas_metro[is.na(valenba_barrio$paradas_metro)] <- 0

# Datos sobre la superficie de vias de metro y tanvía


# DATOS SOBRE LOS PARKINGS DE CADA BARRIO
parkings <- read_parquet("00_marc/dataTratada/info_barrio.parquet")
# Eliminación de los acentos y paso a mayúsculas

prk2merge <- select(parkings, c("barrio",setdiff(colnames(parkings), colnames(valenba_barrio))))

valenba_barrio <- merge(valenba_barrio, prk2merge, by= "barrio", all.x = TRUE, na.rm = TRUE)


#DATOS SOBRE ZONAS VERDES

zonas_verdes <- read.csv("data/zonas_verdes_por_barrio.csv", sep = ";")

colnames(zonas_verdes) <- c("barrio", "superficie_zonas_verdes")

valenba_barrio <- merge(valenba_barrio, zonas_verdes, by = "barrio", all.x = TRUE, na.rm = TRUE)


# ADICIÓN DEL COSTE DE ALQULER POR BARRIO

alquiler <- read.csv("data/precio-alquiler-vivienda.csv", sep = ";")

alq2merg <- select(alquiler, c("BARRIO", "Precio_2022..Euros.m2."))
colnames(alq2merg) <- c("barrio", "precio_alquiler")

valenba_barrio <- merge(valenba_barrio, alq2merg, by = "barrio", all.x = TRUE, na.rm = TRUE)


# ADICIÓN DEL DISTRITO DE CADA BARRIO
aux_barrios <- read.csv("data/barrios_valencia.csv", sep = ";")
aux_barrios <- aux_barrios %>% select(c("Nombre", "Codigo.distrito"))
colnames(aux_barrios) <- c("barrio", "Codigo.distrito")

aux_distritos <- read.csv("data/distritos_valencia.csv", sep = ";")
aux_distritos <- aux_distritos %>% select(c("Nombre", "Código.distrito"))
aux_distritos <- unique(aux_distritos)
colnames(aux_distritos) <- c("distrito", "Codigo.distrito")

barrio_distrito <- aux_barrios %>% left_join(aux_distritos, by = "Codigo.distrito")
barrio_distrito <- barrio_distrito %>% select(c("barrio", "distrito"))

valenba_barrio <- merge(valenba_barrio, barrio_distrito, by = "barrio", all.x = TRUE, na.rm = TRUE)
valenba_barrio$distrito <- as.factor(valenba_barrio$distrito)







# GUARDADO DE LA INFORMACIÓN EN UN PARQUET
valenba_barrio <- valenba_barrio %>% select(-geometry)
# Ejecutar solo cuando se quiera guardar el parquet
write_parquet(valenba_barrio, "data/info_general_barrio.parquet")
write.csv(valenba_barrio, "data/info_general_barrio.csv", row.names = FALSE)
