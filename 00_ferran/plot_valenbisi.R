library(leaflet)
library(igraph)
library(osmdata)
library(sf)
library(ggplot2)



find_nearest_node <- function(graph, x_given, y_given) {
  distances <- sqrt((V(grafo)$y - y_given)^2 + (V(grafo)$x - x_given)^2)
  nearest_node_index <- as.numeric(which.min(distances))
  
  return(V(graph)[nearest_node_index])
}


estaciones <- read.csv2("00_ferran/estaciones_valenbisi.csv", sep = ";")

coord_est <-  estaciones$coords

coord_estaciones <- data.frame(matrix(unlist(strsplit(coord_est, ",")),
                                ncol = 2, byrow = TRUE))

colnames(coord_estaciones) <- c("lat", "lng")

coord_estaciones$lat <- as.numeric(coord_estaciones$lat)
coord_estaciones$lng <- as.numeric(coord_estaciones$lng)

grafo <- read_graph("data/networks_data/valencia_bike.graphml", format = "graphml")
V(grafo)$x <- as.numeric(V(grafo)$x)
V(grafo)$y <- as.numeric(V(grafo)$y)
E(grafo)$length <- as.numeric(E(grafo)$length)

m <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = coord_estaciones$lng, lat = coord_estaciones$lat,
                   color = "blue")

if (nrow(coord_estaciones) > 0){
  nodos_est <- c()
  for (i in 1:nrow(coord_estaciones)){
    nodo <- find_nearest_node(grafo, coord_estaciones$lng[i],
                              coord_estaciones$lat[i])
    nodos_est[[i]] <- nodo
    m <- m %>% addCircleMarkers(lng = V(grafo)[nodo]$x, lat = V(grafo)[nodo]$y,
                                color = "red")
  }
}

city <- getbb("Valencia, Spain")
barrios <- opq(city) %>%
  add_osm_feature(key = 'landuse', value = 'residential') %>%
  osmdata_sf() %>%
  .$osm_polygons

m <- m %>% addPolygons(data = barrios, color = "black")

est_barrio <- data.frame(id = c(), barrio = c(), estacion = c(), lat = c(), 
                         lng = c())

for (node in 1:length(nodos_est)) {
  point <- st_point(c(nodos_est[[node]]$x, nodos_est[[node]]$y))
  b <- unlist(st_within(point, barrios))
  if (length(b) > 0) {
    est_barrio <- rbind(est_barrio, data.frame(id = node, 
                                               barrio = barrios$osm_id[b],
                                               estacion = estaciones$Direccion[node],
                                               lat = nodos_est[[node]]$y,
                                               lng = nodos_est[[node]]$x))
  }else {
    est_barrio <- rbind(est_barrio, data.frame(id = node, 
                                               barrio = "No se ha encontrado",
                                               estacion = estaciones$Direccion[node],
                                               lat = nodos_est[[node]]$y,
                                               lng = nodos_est[[node]]$x))
  }
}

est_barrio$barrio <- as.factor(est_barrio$barrio)

colorpal <- colorFactor(palette = "Set2", domain = est_barrio$barrio)

leaflet(est_barrio) %>%
  addTiles() %>%
  addCircleMarkers(~lng, ~lat, color = ~colorpal(barrio), radius = 10, fillOpacity = 1) %>%
  addPolygons(data = barrios, color = "black")
  "addLegend('bottomright', pal = colorpal, values = ~value,
            title = 'Value',
            opacity = 1)"


city <- getbb("Valencia, Spain")
barrio_llocnou <- opq(city) %>%
  add_osm_feature(key = 'boundary', value = 'administrative') %>%
  osmdata_sf() %>%
  .$osm_polygons

m <- m %>% addPolygons(data = barrio_llocnou, color = "red")
"
if (length(nodos_est) > 1){
  rutas <- c()

  for (i in 1:length(nodos_est)){
    dist <- distances(grafo, nodos_est[[i]], nodos_est[-i])
    m <- as.numeric(which.min(dist))
    if (m >= i){
      m <- m + 1
    }
    rutas[[i]] <- shortest_paths(grafo, nodos_est[[i]], nodos_est[[m]])
  }
  if (length(rutas) > 0){
    for (i in 1:length(rutas)){
      if (!is.null(rutas[[i]]$vpath)){
        for (j in 2:length(rutas[[i]]$vpath[[1]])){
          ini <- rutas[[i]]$vpath[[1]][j - 1]
          fin <- rutas[[i]]$vpath[[1]][j]
          m <- m %>%
            addPolylines(lng = c(ini$x, fin$x),
                         lat = c(ini$y, fin$y), color = 'blue')
        }
      }
    }
  }
}
"
