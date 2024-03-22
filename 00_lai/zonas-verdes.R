getwd()
file <- "./00_lai/zonas-verdes.csv"

df <- read.csv2(file=file)
df$m2_poligon <- round(as.numeric(df$m2_poligon), 0)

summary(df$m2_poligon)
table(df$m2_poligon)

sum(df$m2_poligon)/807.693
names(df)
df$geo_point_2d

library(tidyverse)
library(ggplot2)
library(leaflet)

# Separa la columna 'geo_point_2d' en dos columnas: 'latitud' y 'longitud'
df <- separate(df, geo_point_2d, into = c("latitud", "longitud"), sep = ",", convert = TRUE)

# Convertimos las columnas a tipo numérico si no se han convertido automáticamente
df$latitud <- as.numeric(df$latitud)
df$longitud <- as.numeric(df$longitud)

# Calcula el centro del mapa
centro_lat <- mean(df$latitud)
centro_lon <- mean(df$longitud)

# Crea el mapa Leaflet centrado en el área de los puntos geográficos
mapa <- leaflet(data = df) %>%
  addTiles() %>%
  addMarkers(~longitud, ~latitud) %>%
  setView(lng = centro_lon, lat = centro_lat, zoom = 10)

# Muestra el mapa
mapa

sum(df$m2_poligon)

library(jsonlite)
library(sf)
library(geojsonsf)
library(geojsonio)

# cojo un geoshape de prueba
df$geo_shape
p <- df$geo_shape[1]
sf <- geojson_sf(p)
st_area(sf)

geojson_sf(df$geo_shape)

summary(df$geo_shape)


hay_string_vacio <- any(grepl("^$", df$geo_shape))
print(hay_string_vacio)

df <- subset(df, !grepl("^$", geo_shape))

hay_string_vacio <- any(grepl("^$", df$geo_shape))
df$geo_shape <- geojson_sf(df$geo_shape)
df$area <- st_area(df$geo_shape)
df$area
sum(df$area)
sum(df$m2_poligon)
