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
poblacion$barrio <- toupper(sapply(strsplit(poblacion$X, "\\. "), function(x) x[2]))


valenba_barrio <- merge(valenba_barrio, poblacion, by = "barrio", all = FALSE)

valenba_barrio <- valenba_barrio %>% select(-X)


# OTROS DATOS DE LOS BARRIOS

valenba_barrio$geometry <- geojson_sf(valenba_barrio$geo_shape)

# datos sobre la superficie de los barrios
valenba_barrio$superficie <- st_area(valenba_barrio$geometry)

# GUARDADO DE LA INFORMACIÓN EN UN PARQUET
valenba_barrio <- valenba_barrio %>% select(-geometry)
write_parquet(valenba_barrio, "data/info_general_barrio.parquet")

