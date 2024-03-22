library(leaflet)
library(sf)

plazas_mob <- read.csv('./parkings/dataTratada/plazas_mobred_geo.csv', header = TRUE, sep = ',')
plazas_mob

plazas_ora <- read.csv('./parkings/dataPre/aparcaments-ora-aparcamientos-ora.csv', header = TRUE, sep = ';')
plazas_ora

plazas_ora$Color
unique(plazas_ora$Color)
table(plazas_ora$Color)

plazas_ora <- separate(plazas_ora, geo_point_2d, into = c("Latitude", "Longitude"), sep = ",")
plazas_ora$Latitude <- as.numeric(plazas_ora$Latitude)
plazas_ora$Longitude <- as.numeric(plazas_ora$Longitude)


# Crear un diccionario de traducción
color_dict <- data.frame(
  Color = c("Naranja", "Azul", "Verde", "Amarillo", "Blanco"),
  color_en = c("orange", "blue", "green", "yellow", "grey")
)

# Supongamos que tienes una tabla llamada 'mi_tabla' con una columna 'color_es'
# que contiene los nombres de colores en castellano. Unir la tabla original con el diccionario:
tabla2 <- merge(plazas_ora, color_dict, by="Color", all.x = TRUE)

unique(tabla2$color_en)

data_dist <-read.csv('./GitHub/transport-analysis-proy-II/data/distritos_valencia.csv', header = TRUE, sep = ';')
data_dist


obtener_poligonos <- function(df){
  polygons <- lapply(df$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  polygons_barrios <- do.call(rbind, polygons)
  
  polygons_barrios$Nombre <- df$Nombre
  return(polygons_barrios)
}

dist <- obtener_poligonos(data_dist)
dist

mapa <- leaflet() %>%
  addTiles() %>%
  setView(lng = -0.3763, lat = 39.4699, zoom = 12)

colorpal <- colorFactor(palette = "Set3", domain = dist$Nombre)

mapa <- mapa %>%
  addMarkers(data = plazas_mob, lng = ~longitud, lat = ~latitud, popup = "Plaza Mobilidad Reducida", clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = dist, color = ~colorpal(Nombre), fillOpacity = 0.6, popup = ~Nombre)

mapa

mapa2 <- leaflet() %>%
  addTiles() %>%
  setView(lng = -0.3763, lat = 39.4699, zoom = 12)

mapa2 <- mapa2 %>%
  addCircleMarkers(data= tabla2, lng = ~Longitude, lat = ~Latitude, color= ~color_en, clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = dist, color = ~colorpal(Nombre), fillOpacity = 0.6, popup = ~Nombre)

mapa2
