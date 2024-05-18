library(mapview)
library(webshot)
library(leaflet)

## 'leaflet' objects (image above)

m <- readRDS("./app_shiny/mapas/CiudaddelasArtesylasCiencias500TODOS.rds")
m2 <- readRDS("./app_shiny/mapas/CatedraldeValencia500TODOS.rds")
m3 <- readRDS("./app_shiny/mapas/MercadoCentralValencia1000.rds")

m %>% addProviderTiles(providers$CartoDB.Positron) 
m2 %>% addProviderTiles(providers$CartoDB.Positron)
m3 %>% addProviderTiles(providers$CartoDB.Positron)

mapshot(m, file = "./fotos/Ciudad_artes_ciencias500.png", )
mapshot(m2, file = "./fotos/Catedral500.png")
mapshot(m3, file = "./fotos/mercadocentral1000METROS.png")
