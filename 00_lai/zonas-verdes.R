library(tidyverse)
library(ggplot2)
library(leaflet)
library(jsonlite)
library(sf)
library(geojsonsf)
library(geojsonio)
library(purrr)
library(igraph)
library(osmdata)

getwd()

# Carga el archivo CSV con los datos de las zonas verdes
file <- "./00_lai/zonas-verdes.csv"
df <- read.csv2(file=file)
df$m2_poligon <- round(as.numeric(df$m2_poligon), 0)

names(df)
head(df)

# Quito las columnas que contienen "m2zv"
df <- df[ , !grepl("m2zv", names(df))]

head(df)

summary(df$m2_poligon)
names(df)

sum(df$m2_poligon)

# Comprobamos si hay algún string vacío en la columna 'geo_shape'
hay_string_vacio <- any(grepl("^$", df$geo_shape))
print(hay_string_vacio)

df <- subset(df, !grepl("^$", geo_shape))

# Comprobamos si sigue habiendo algún string vacío en la columna 'geo_shape'
hay_string_vacio <- any(grepl("^$", df$geo_shape))
print(hay_string_vacio)

# Convertimos la columna 'geo_shape' a tipo 'sf'
df$geo_shape <- map(df$geo_shape, geojson_sf)

# Calculamos el área de cada polígono
lista_area <- map(df$geo_shape, st_area)
df$area <- unlist(lista_area)
df$area <- round(df$area, 0)

# Redondeo a la decena por abajo
df$area <- floor(df$area / 10) * 10

head(df)
names(df)
sum(df$area)
sum(df$m2_poligon)

# Ahora, vamos a meter cada polígono en un barrio mediante su geo_point

obtener_poligonos <- function(df){
  polygons <- lapply(barrios$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  polygons_barrios <- do.call(rbind, polygons)
  
  polygons_barrios$Nombre <- barrios$Nombre
  return(polygons_barrios)
}

# Cargamos los datos de los barrios
barrios <- read.csv2("data/barrios_valencia.csv", sep = ";")
polygons_barrios <- obtener_poligonos(barrios)
polygons_barrios

# Cuento los distintos barrios que hay
s_barrios <- unique(polygons_barrios$Nombre)
class(s_barrios)

# Mediante la columna geo_point_2d, vamos a asignar cada polígono a un barrio

# Separo geo_point_2d en dos columnas: x e y.
# Debo hacer un split 
cords <- strsplit(df$geo_point_2d, ",")
df$x <- sapply(cords, function(x) x[1])
df$y <- sapply(cords, function(x) x[2])
df$x <- as.numeric(df$x)
df$y <- as.numeric(df$y)

head(df)

for (zv in 1:nrow(df)) {
  y <- df$x[zv]
  x <- df$y[zv]
  point <- st_point(c(x, y))
  # b devuelve una lista con el índice del polígono en el que está el nodo
  b <- unlist(st_within(point, polygons_barrios$geometry)) 

  # Si el nodo está en un polígono, asigno el nombre del barrio correspondiente
  if (length(b) > 0) {
    df$barrio[zv] <- polygons_barrios$Nombre[b]
  } else {
    df$barrio[zv] <- 'Otro'
  }
}

s_barrios2 <- c(unique(df$barrio))

# Obtengo la diferencia de los dos vectores para saber qué barrios no tienen zonas verdes
setdiff(s_barrios, s_barrios2)

# Obtengo un data frame resumen con la suma de las áreas de las zonas verdes por barrio
df_resumen <- df %>% group_by(barrio) %>% summarise(area = sum(area))
sum(df_resumen$area)

print(df_resumen)

# Guardo el data.frame en un csv
write.csv2(df_resumen, "data/zonas_verdes_por_barrio.csv", row.names = FALSE)
