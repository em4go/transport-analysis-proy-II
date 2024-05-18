# Librerías necesarias
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidytransit)
library(sf)
library(lubridate)
library(igraph)
library(osmdata)
library(arrow)
library(geojsonsf)

#Usamos las funciones auxiliares

# find_nearest_node -> Encuentra el nodo más cercano a un punto (graph, x_given, y_given)
# get_start_node -> Encuentra el nodo más cercano a un punto de inicio (grafo, location_address)
# get_gtfs_distance -> Devuelve la tabla de las estaciones al nodo de inicio de arriba, con la distancia a las estaciones metro (gtfs, grafo, nodo_entrada, distance)
# isochron -> Crea el subgrafo (graph, start_node_x, start_node_y, metric, threshold)
# plot_isochron -> Dibuja el subgrafo (subgrafo, start_node, gtfs.stop, dist)

# Cargamos las funciones auxiliares
source("funcionesAUX_visu.R")

# Cargar el grafo de Valencia
grafo <- read_graph("../data/networks_data/valencia_walk.graphml", format = "graphml") 

V(grafo)$x <- as.numeric(V(grafo)$x) # Convertir a numérico
V(grafo)$y <- as.numeric(V(grafo)$y) # Convertir a numérico
E(grafo)$length <- as.numeric(E(grafo)$length) # Convertir a numérico

# Cargar el GTFS de Metrovalencia, para las paradas de metro
gtfs_metro <- read_gtfs("../data/gtfs_data/transit_metrovalencia.zip")
gtfs_metro$stops
# Cargar el GTFS de EMT Valencia, para las paradas de autobús
gtfs_emt <- read_gtfs("../data/gtfs_data/transit_emt_valencia.zip")
gtfs_emt <- data.frame(gtfs_emt$stops)

# ----------------------------------------------------------------------------------------------------------------------------
#GTFS solo valencia

data_barrios <- read.csv2("../data/barrios_valencia.csv", sep = ";")

obtener_poligonos <- function(df){
  polygons <- lapply(df$geo_shape, function(x) st_read(x, quiet = TRUE))
  
  # Combina todos los objetos sf en un solo objeto sf
  polygons_barrios <- do.call(rbind, polygons)
  
  polygons_barrios$Nombre <- df$Nombre
  return(polygons_barrios)
}
poligons_barrios <- obtener_poligonos(data_barrios)
poligons_barrios

paradas_metro_val <- data.frame()
# Iterate over each plaza
for (pl in 1:nrow(gtfs_metro$stops)) {
  point <- st_point(c(as.numeric(gtfs_metro$stops$stop_lon[pl]), as.numeric(gtfs_metro$stops$stop_lat[pl])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligons_barrios$geometry)) 
  
  if (length(b) > 0) {
    paradas_metro_val <- rbind(paradas_metro_val, data.frame(
      stop_id = gtfs_metro$stops$stop_id[pl],
      stop_name = gtfs_metro$stops$stop_name[pl],
      stop_lat = gtfs_metro$stops$stop_lat[pl],
      stop_lon = gtfs_metro$stops$stop_lon[pl],
      barrio = poligons_barrios$Nombre[b]
      ))
  } else {
    paradas_metro_val <- rbind(paradas_metro_val, data.frame(
      stop_id = gtfs_metro$stops$stop_id[pl],
      stop_name = gtfs_metro$stops$stop_name[pl],
      stop_lat = gtfs_metro$stops$stop_lat[pl],
      stop_lon = gtfs_metro$stops$stop_lon[pl],
      barrio = "NO ESTA EN VALENCIA"
    ))
  }
}

paradas_metro_en_valencia <- paradas_metro_val %>% filter(barrio != "NO ESTA EN VALENCIA")
#--------------------------------------------------------------
#--------------------------------------------------------------

#PARADAS PARA BUS

paradas_bus_val <- data.frame()
# Iterate over each plaza
for (pl in 1:nrow(gtfs_emt)) {
  point <- st_point(c(as.numeric(gtfs_emt$stop_lon[pl]), as.numeric(gtfs_emt$stop_lat[pl])), dim = "XY")
  #longitud y latitud porque los poligonos de los barrios estan en ese orden
  b <- unlist(st_within(point, poligons_barrios$geometry)) 
  
  if (length(b) > 0) {
    paradas_bus_val <- rbind(paradas_bus_val, data.frame(
      stop_name = gtfs_emt$stop_name[pl],
      stop_lat = gtfs_emt$stop_lat[pl],
      stop_lon = gtfs_emt$stop_lon[pl],
      barrio = poligons_barrios$Nombre[b]
    ))
  } else {
    paradas_bus_val <- rbind(paradas_bus_val, data.frame(
      stop_name = gtfs_emt$stop_name[pl],
      stop_lat = gtfs_emt$stop_lat[pl],
      stop_lon = gtfs_emt$stop_lon[pl],
      barrio = "NO ESTA EN VALENCIA"
    ))
  }
}
paradas_bus_en_valencia <- paradas_bus_val %>% filter(barrio != "NO ESTA EN VALENCIA")
#--------------------------------------------------------------
#--------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------

# Ejemplo de uso de las funciones auxiliares
# ----------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------
nodo_entrada <- get_start_node(grafo, "Mercado Central Valencia") # Nodo de entrada

X <- nodo_entrada$x # Coordenada X
Y <- nodo_entrada$y # Coordenada Y

medida <- E(grafo)$length # Medida de distancia
distancia <- 500 # Distancia en metros

estaciones_selec <- get_gtfs_distance(gtfs_metro, grafo, nodo_entrada, distancia) # La matriz de estaciones cercanas 
estaciones_selec

g1 <- isochron(grafo, X, Y, medida, distancia) # Subgrafo

start_node <- find_nearest_node(grafo, X, Y) # Nodo de inicio

m <- plot_isochron1(g1, start_node, estaciones_selec, distancia) # Dibujar el subgrafo

if (nrow(estaciones_selec) > 0) {
  for (i in 1:nrow(estaciones_selec)) {
    estacion <- estaciones_selec[i,]
  
    grafo1 <- add_vertices(grafo, 1, name = estacion$station_name, x = estacion$x, y = estacion$y)
  
    nodo <- find_nearest_node(grafo1, estacion$x, estacion$y)
  
    connect_nearest_nodes(grafo1, estacion$x, estacion$y)
  
    nodo_inicio <- get_start_node(grafo1, "Plaza de Toros de Valencia")
  
    camino_mas_corto <- shortest_paths(grafo1, from = nodo_inicio, to = nodo, mode = "out")
  
    m <- add_path_to_map(m, grafo1, camino_mas_corto$vpath[[1]])
  }
}
m
# ----------------------------------------------------------------------------------------------------------------------------
#METROO
# ----------------------------------------------------------------------------------------------------------------------------
posibles_puntos <- c("Plaza de Toros de Valencia", "Estación del Norte Valencia",
                     "Universidad Politecnica de Valencia", "Estadio Mestalla", "Playa de la Malvarrosa", "Bioparc Valencia", 
                     "Estación Valencia-Cabanyal", "Ciudad de las Artes y las Ciencias", "Catedral de Valencia", 
                     "Mercado Central Valencia")

puntos_faltantes <-  c("Pabellón Fuente de San Luís Valencia","Mercado Central Valencia")

lugar_seleccionado <- "Plaza de Toros de Valencia"

posibles_distancias <- c(250, 500, 750, 1000)

posibles_puntos1 <- c("Plaza de Toros de Valencia", "Estación del Norte Valencia")
posibles_distancias1 <- c(300, 500)

for (lugar in posibles_puntos) {
  for (distancia in posibles_distancias) {
    sitio <- paste(lugar, distancia)
    sitio_sin_espacios <- gsub(" ", "", sitio)  
    ruta <- paste0("./mapas/", sitio_sin_espacios, ".rds")
    
    nodo_entrada <- get_start_node(grafo, lugar)
    
    estaciones_selec <- get_gtfs_distance(gtfs_metro, grafo, nodo_entrada, distancia)
    
    subgraf <- isochron(grafo, nodo_entrada$x, nodo_entrada$y, E(grafo)$length, distancia)
    
    start_node <- find_nearest_node(grafo, nodo_entrada$x, nodo_entrada$y)
    
    mapa <- plot_isochron(subgraf, start_node, estaciones_selec, distancia)
    
    if (nrow(estaciones_selec) > 0) {
      for (i in 1:nrow(estaciones_selec)) {
        estacion <- estaciones_selec[i,]
      
        grafo1 <- add_vertices(grafo, 1, name = estacion$station_name, x = estacion$x, y = estacion$y)
      
        nodo <- find_nearest_node(grafo1, estacion$x, estacion$y)
      
        connect_nearest_nodes(grafo1, estacion$x, estacion$y)
      
        nodo_inicio <- get_start_node(grafo1, lugar)
      
        camino_mas_corto <- shortest_paths(grafo1, from = nodo_inicio, to = nodo, mode = "out")
      
        mapa <- add_path_to_map(mapa, grafo1, camino_mas_corto$vpath[[1]])
      }
    }
    saveRDS(mapa, file = ruta)
    print(ruta)
  }
}

# ----------------------------------------------------------------------------------------------------------------------------
#BUS

puntos_faltantes <-  c("Pabellón Fuente de San Luís Valencia") # ESTE EN BUS FALLA

for (lugar in puntos_faltantes) {
  for (distancia in posibles_distancias) {
    sitio <- paste(lugar, distancia)
    sitio_sin_espacios <- gsub(" ", "", sitio)  
    ruta <- paste0("./mapas/", sitio_sin_espacios, "BUS.rds")
    
    nodo_entrada <- get_start_node(grafo, lugar)
    print("estaciones 0")
    estaciones_selec <- get_gtfs_distance(gtfs_emt, grafo, nodo_entrada, distancia)
    print("estaciones 1")
    subgraf <- isochron(grafo, nodo_entrada$x, nodo_entrada$y, E(grafo)$length, distancia)
    
    start_node <- find_nearest_node(grafo, nodo_entrada$x, nodo_entrada$y)
    
    mapa <- plot_isochronBUS(subgraf, start_node, estaciones_selec, distancia)
    print("AQUI PASA")
    print(estaciones_selec)
    if (nrow(estaciones_selec) > 0) {
      for (i in 1:nrow(estaciones_selec)) {
        estacion <- estaciones_selec[i,]
        
        grafo1 <- add_vertices(grafo, 1, name = estacion$station_name, x = estacion$x, y = estacion$y)
        
        nodo <- find_nearest_node(grafo1, estacion$x, estacion$y)
        print("nodo esta bien")
        connect_nearest_nodes(grafo1, estacion$x, estacion$y)
        
        nodo_inicio <- get_start_node(grafo1, lugar)
        
        camino_mas_corto <- shortest_paths(grafo1, from = nodo_inicio, to = nodo, mode = "out")
        
        mapa <- add_path_to_mapBUS(mapa, grafo1, camino_mas_corto$vpath[[1]])
      }
    }
    saveRDS(mapa, file = ruta)
    print(ruta)
  }
}

#TODOS  #FALTA LA FONTENTA 

posibles_puntos1 <- c("Estación del Norte Valencia")
posibles_distancias1 <- c(500)

for (lugar in posibles_puntos) {
  for (distancia in posibles_distancias) {
    sitio <- paste(lugar, distancia)
    sitio_sin_espacios <- gsub(" ", "", sitio)  
    ruta <- paste0("./mapas/", sitio_sin_espacios, "TODOS.rds")
    
    nodo_entrada <- get_start_node(grafo, lugar)
    #print("ESTACIONES METRO")
    estaciones_metro <- get_gtfs_distance(gtfs_metro, grafo, nodo_entrada, distancia)
    #print("ESTACIONES METRO, TERMINA")
    #print("ESTACIONES BUS")
    estaciones_bus <- get_gtfs_distance(gtfs_emt, grafo, nodo_entrada, distancia)
    #print("ESTACIONES BUS, TERMINA")
    subgraf <- isochron(grafo, nodo_entrada$x, nodo_entrada$y, E(grafo)$length, distancia)
    
    start_node <- find_nearest_node(grafo, nodo_entrada$x, nodo_entrada$y)
    
    #print("PLOT_ISOCHRON")
    mapa <- plot_isochronBUS(subgraf, start_node, estaciones_bus, distancia)
    
    X <- estaciones_metro[estaciones_metro$distance_to_start < distancia,]$x
    Y <- estaciones_metro[estaciones_metro$distance_to_start < distancia,]$y
    
    lista_iconos <- icons(iconUrl = "https://cdn-icons-png.freepik.com/512/684/684908.png", NA,
                          iconWidth = c(40), iconHeight = c(40))
    
    mapa <- mapa %>% addMarkers(lng = X, lat = Y, popup = paste0(estaciones_metro[estaciones_metro$distance_to_start < distancia,]$station_name, "-METRO"), icon = lista_iconos)
    
    if (nrow(estaciones_bus) > 0) {
      for (i in 1:nrow(estaciones_bus)) {
        estacion <- estaciones_bus[i,]
        
        grafo1 <- add_vertices(grafo, 1, name = estacion$station_name, x = estacion$x, y = estacion$y)
        
        nodo <- find_nearest_node(grafo1, estacion$x, estacion$y)
        
        connect_nearest_nodes(grafo1, estacion$x, estacion$y)
        
        nodo_inicio <- get_start_node(grafo1, lugar)
        
        camino_mas_corto <- shortest_paths(grafo1, from = nodo_inicio, to = nodo, mode = "out")
        
        mapa <- add_path_to_mapBUS(mapa, grafo1, camino_mas_corto$vpath[[1]])
        #print("MAPA DIKSTRA BUS")
      }
    }
    
    if (nrow(estaciones_metro) > 0) {
      for (i in 1:nrow(estaciones_metro)) {
        estacion <- estaciones_metro[i,]
        
        grafo1 <- add_vertices(grafo, 1, name = estacion$station_name, x = estacion$x, y = estacion$y)
        
        nodo <- find_nearest_node(grafo1, estacion$x, estacion$y)
        
        connect_nearest_nodes(grafo1, estacion$x, estacion$y)
        
        nodo_inicio <- get_start_node(grafo1, lugar)
        
        camino_mas_corto <- shortest_paths(grafo1, from = nodo_inicio, to = nodo, mode = "out")
        
        mapa <- add_path_to_map(mapa, grafo1, camino_mas_corto$vpath[[1]])
        #print("MAPA DIKSTRA METRO")
      }
    }
    
    saveRDS(mapa, file = ruta)
    print(ruta)

  }
}

mapa

#--------------------------------------------------------------
#DIKSTRA
nodo_entrada <- get_start_node(grafo, "Plaza de Toros de Valencia") # Nodo de entrada

X <- nodo_entrada$x # Coordenada X
Y <- nodo_entrada$y # Coordenada Y

medida <- E(grafo)$length # Medida de distancia
distancia <- 300 # Distancia en metros

estaciones_selec <- get_gtfs_distance(gtfs_metro, grafo, nodo_entrada, distancia) # La matriz de estaciones cercanas 
estaciones_selec
xativa <- estaciones_selec[1,]
xativa

grafo1 <- add_vertices(grafo, 1, name = xativa$station_name, x = xativa$x, y = xativa$y)

nodo <- find_nearest_node(grafo1, xativa$x, xativa$y)

connect_nearest_nodes(grafo1, xativa$x, xativa$y)

nodo_entrada <- get_start_node(grafo1, "Plaza de Toros de Valencia")

camino_mas_corto <- shortest_paths(grafo1, from = nodo_entrada, to = nodo, mode = "out")

m <- leaflet() %>% addTiles()

m <- add_path_to_map(m, grafo1, camino_mas_corto$vpath[[1]])
m
m <- m %>% addMarkers(lng = xativa$x, lat = xativa$y)
m
#--------------------------------------------------------------


valencia_map <- leaflet() %>%
  setView(lng = -0.3758, lat = 39.4699, zoom = 13) %>%
  addTiles()

# Muestra el mapa
valencia_map

r = "./mapas/EstacióndelNorteValencia500TODOS.rds"

m <- readRDS(r)
m


#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
establecer_barrio<- function(punto, info_poligonos) {
  # Recibe un objeto de tipo punto y un dataframe con la información de los polígonos y el barrio
  b <- unlist(st_within(punto, info_poligonos$geometry))
  if (length(b) == 0) {
    return(NA)
  } else {
    return(info_poligonos$barrio[b])
  }
}

carac_metro <- read_parquet("../data/caracteristicas_metro.parquet")
poligonos_barrios <- read_parquet("../data/info_general_barrio.parquet")
poligonos_barrios[is.na(poligonos_barrios)] <- 0
# En la siguiente función, el parámetro crs indica el sistema de referencia de coordenadas
estaciones <- st_as_sf(carac_metro, coords = c("stop_lon", "stop_lat"), crs = 4326)
poligonos_barrios <- cbind(poligonos_barrios, geojson_sf(poligonos_barrios$geo_shape))


estaciones$barrio <- sapply(1:nrow(estaciones), function(i) {
  establecer_barrio(estaciones[i,], poligonos_barrios)
})

carac_metro <- merge(carac_metro, estaciones[,c("stop_id", "barrio")], by = "stop_id", all.x = TRUE)

barrios <- read.csv("../00_marc/dataTratada/barrios_indices.csv")
barrios[is.na(barrios)] <- 0
barrios_total <- read.csv("../00_marc/dataTratada/barrios_final.csv")
carac_metro_barrio <- data.frame()

carac_metro_barrio <- barrios %>% select(-paradas_metro, -paradas_emt, -estaciones_valenbisi) %>%
  merge(carac_metro, by = "barrio", all.y = TRUE)
b <- poligonos_barrios %>% select(-paradas_metro, -geo_shape, -precio_alquiler,
                                  -geometry, -distrito)
carac_metro_barrio <- b %>%
  merge(carac_metro_barrio, by = "barrio", all.y = TRUE)
carac_metro_barrio <- barrios_total %>% select(barrio, RENTA_BRUTA, SALARIO, Precio_alquiler.m2, 
                                               Precio_venta.m2) %>%
  merge(carac_metro_barrio, by = "barrio", all.y = TRUE)

carac_metro_barrio <- carac_metro_barrio[!is.na(carac_metro_barrio$barrio) |
                                           carac_metro_barrio$stop_id %in% c("55","56"),]

load("../00_ferran/graph_metro_valencia.RData")

g_valencia <- induced_subgraph(g, V(g)$name %in% carac_metro_barrio$stop_id)


aux <- data.frame(apply(carac_metro[,c("pagerank", "closeness", "betweenness", "eigenvector")],
                        2, function(x) x/max(x)))

carac_metro <- carac_metro %>%
  select(-c("pagerank", "closeness", "betweenness", "eigenvector")) %>%
  cbind(aux)

plot_graph <- function(dataset,color, tamanyo){
  
  library(leaflet)
  
  colors <- colorNumeric(palette = "YlGnBu", domain = color)
  
  b <- st_as_sf(dataset, crs = 4326)
  
  
  plot_isochron <- function(subgrafo) {
    
    edge_list <- as_edgelist(subgrafo)
    
    m = leaflet() %>% addTiles() # urlTemplate = ""
    
    for (i in 1:nrow(edge_list)) {
      edge <- edge_list[i,]
      head_edge <- V(subgrafo)[edge[1]]
      tail_edge <- V(subgrafo)[edge[2]]
      
      m <- m %>%
        addPolylines(lng = c(head_edge$lon, tail_edge$lon), lat = c(head_edge$lat, tail_edge$lat),
                     color = "#35434c", weight = 2, opacity = 0.9)
    }
    
    # add nodes to map
    #m <- m %>% addCircleMarkers(lng = V(subgrafo)$lon, lat = V(subgrafo)$lat, radius = 5, color = "red")
    #m <- m %>% addCircleMarkers(lng = start_node$x, lat = start_node$y, radius = 5, color = "yellow")
    return(m)
  }
  
  
  mapa <- plot_isochron(g)
  
  mapa <- mapa %>%
    addCircleMarkers(data = b, color = ~colors(color), radius = ~((tamanyo^2)*5) + 3, fillOpacity = 1,
                     popup = ~paste(stop_name)) %>%
    setView(lng = -0.377, lat = 39.465, zoom = 12)
  
  
  mapa
}

m <- plot_graph(carac_metro, color = carac_metro$pagerank, tamanyo = carac_metro$pagerank) # pagerank pagerank
ruta1 <- paste0("./mapas/grafo/", "pagerank", "pagerank", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$pagerank, tamanyo = carac_metro$betweenness) # pagerank betweenness
ruta1 <- paste0("./mapas/grafo/","pagerank","betweenness", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$pagerank, tamanyo = carac_metro$closeness) # pagerank closeness
ruta1 <- paste0("./mapas/grafo/","pagerank" , "closeness", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$pagerank, tamanyo = carac_metro$eigenvector) # pagerank eigenvector
ruta1 <- paste0("./mapas/grafo/", "pagerank", "eigenvector", ".rds") 
saveRDS(m, file = ruta1)

#--------------------------------------------------------------
# betweenness pagerank
# betweenness betweenness
# betweenness closeness
# betweenness eigenvector

m <- plot_graph(carac_metro, color = carac_metro$betweenness, tamanyo = carac_metro$pagerank) # pagerank pagerank
ruta1 <- paste0("./mapas/grafo/", "betweenness", "pagerank", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$betweenness, tamanyo = carac_metro$betweenness) # pagerank betweenness
ruta1 <- paste0("./mapas/grafo/","betweenness","betweenness", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$betweenness, tamanyo = carac_metro$closeness) # pagerank closeness
ruta1 <- paste0("./mapas/grafo/","betweenness" , "closeness", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$betweenness, tamanyo = carac_metro$eigenvector) # pagerank eigenvector
ruta1 <- paste0("./mapas/grafo/", "betweenness", "eigenvector", ".rds") 
saveRDS(m, file = ruta1)

#--------------------------------------------------------------

m <- plot_graph(carac_metro, color = carac_metro$closeness, tamanyo = carac_metro$pagerank) # pagerank pagerank
ruta1 <- paste0("./mapas/grafo/", "closeness", "pagerank", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$closeness, tamanyo = carac_metro$betweenness) # pagerank betweenness
ruta1 <- paste0("./mapas/grafo/","closeness","betweenness", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$closeness, tamanyo = carac_metro$closeness) # pagerank closeness
ruta1 <- paste0("./mapas/grafo/","closeness" , "closeness", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$closeness, tamanyo = carac_metro$eigenvector) # pagerank eigenvector
ruta1 <- paste0("./mapas/grafo/", "closeness", "eigenvector", ".rds") 
saveRDS(m, file = ruta1)

#--------------------------------------------------------------

m <- plot_graph(carac_metro, color = carac_metro$eigenvector, tamanyo = carac_metro$pagerank) # pagerank pagerank
ruta1 <- paste0("./mapas/grafo/", "eigenvector", "pagerank", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$eigenvector, tamanyo = carac_metro$betweenness) # pagerank betweenness
ruta1 <- paste0("./mapas/grafo/","eigenvector","betweenness", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$eigenvector, tamanyo = carac_metro$closeness) # pagerank closeness
ruta1 <- paste0("./mapas/grafo/","eigenvector" , "closeness", ".rds") 
saveRDS(m, file = ruta1)

m <- plot_graph(carac_metro, color = carac_metro$eigenvector, tamanyo = carac_metro$eigenvector) # pagerank eigenvector
ruta1 <- paste0("./mapas/grafo/", "eigenvector", "eigenvector", ".rds") 
saveRDS(m, file = ruta1)


#OPCIONES color --> pagerank, betweenness, closeness, eigenvector, c("PageRank", "Intermediación", "Cercanía", "VectorPropio")

#OPCIONES tamanyo --> pagerank, betweenness, closeness, eigenvector, c("PageRank", "Intermediación", "Cercanía", "VectorPropio")


posibles_puntos <- c("Catedral de Valencia")
posibles_distancias <-  c(500)

plot_isochron <- function(subgrafo, start_node, gtfs.stop, dist) {
  
  edge_list <- as_edgelist(subgrafo)
  
  m = leaflet() %>% addTiles()
  
  for (i in 1:nrow(edge_list)) {
    edge <- edge_list[i,]
    head_edge <- V(subgrafo)[edge[1]]
    tail_edge <- V(subgrafo)[edge[2]]
    # a la polyline le pongo este color 8DADAB

    m <- m %>%
      addPolylines(lng = c(head_edge$x, tail_edge$x), lat = c(head_edge$y, tail_edge$y), color = "#8DADAB")
  }
  
  # add nodes to map
  #m <- m %>% addCircleMarkers(lng = V(subgrafo)$x, lat = V(subgrafo)$y, radius = 5, color = "red")
  
  lista_iconos1 <- icons(iconUrl = "https://cdn-icons-png.freepik.com/512/4225/4225606.png", NA,
                         iconWidth = c(45), iconHeight = c(45))
  
  m <- m %>% addMarkers(lng = start_node$x, lat = start_node$y, popup = "Nodo Inicial", icon = lista_iconos1)
  
  #añadimos los puntos de las paradas de metrovalencia
  
  X <- gtfs.stop[gtfs.stop$distance_to_start < dist,]$x
  Y <- gtfs.stop[gtfs.stop$distance_to_start < dist,]$y
  
  lista_iconos <- icons(iconUrl = "https://cdn.icon-icons.com/icons2/916/PNG/512/Marker_icon-icons.com_71852.png", NA,
                        iconWidth = c(40), iconHeight = c(40))
  
  m <- m %>% addMarkers(lng = X, lat = Y, popup = paste0(gtfs.stop[gtfs.stop$distance_to_start < dist,]$station_name, "-METRO"), icon = lista_iconos)
  
  return(m)
}
#CATEDRAL 500, MERCADO CENTRA METRO 1000

for (lugar in posibles_puntos) {
  for (distancia in posibles_distancias) {
    sitio <- paste(lugar, distancia)
    sitio_sin_espacios <- gsub(" ", "", sitio)  
    ruta <- paste0("./mapas/", sitio_sin_espacios, "TODOS.rds")
    
    nodo_entrada <- get_start_node(grafo, lugar)
    #print("ESTACIONES METRO")
    estaciones_metro <- get_gtfs_distance(gtfs_metro, grafo, nodo_entrada, distancia)
    #print("ESTACIONES METRO, TERMINA")
    #print("ESTACIONES BUS")
    estaciones_bus <- get_gtfs_distance(gtfs_emt, grafo, nodo_entrada, distancia)
    #print("ESTACIONES BUS, TERMINA")
    subgraf <- isochron(grafo, nodo_entrada$x, nodo_entrada$y, E(grafo)$length, distancia)
    
    start_node <- find_nearest_node(grafo, nodo_entrada$x, nodo_entrada$y)
    
    #print("PLOT_ISOCHRON")
    mapa <- plot_isochron(subgraf, start_node, estaciones_bus, distancia)
    
    X <- estaciones_metro[estaciones_metro$distance_to_start < distancia,]$x
    Y <- estaciones_metro[estaciones_metro$distance_to_start < distancia,]$y
    
    lista_iconos <- icons(iconUrl = "https://cdn-icons-png.freepik.com/512/684/684908.png", NA,
                          iconWidth = c(40), iconHeight = c(40))
    
    mapa <- mapa %>% addMarkers(lng = X, lat = Y, popup = paste0(estaciones_metro[estaciones_metro$distance_to_start < distancia,]$station_name, "-METRO"), icon = lista_iconos)
    
    if (nrow(estaciones_bus) > 0) {
      for (i in 1:nrow(estaciones_bus)) {
        estacion <- estaciones_bus[i,]
        
        grafo1 <- add_vertices(grafo, 1, name = estacion$station_name, x = estacion$x, y = estacion$y)
        
        nodo <- find_nearest_node(grafo1, estacion$x, estacion$y)
        
        connect_nearest_nodes(grafo1, estacion$x, estacion$y)
        
        nodo_inicio <- get_start_node(grafo1, lugar)
        
        camino_mas_corto <- shortest_paths(grafo1, from = nodo_inicio, to = nodo, mode = "out")
        
        mapa <- add_path_to_mapBUS(mapa, grafo1, camino_mas_corto$vpath[[1]])
        #print("MAPA DIKSTRA BUS")
      }
    }
    
    if (nrow(estaciones_metro) > 0) {
      for (i in 1:nrow(estaciones_metro)) {
        estacion <- estaciones_metro[i,]
        
        grafo1 <- add_vertices(grafo, 1, name = estacion$station_name, x = estacion$x, y = estacion$y)
        
        nodo <- find_nearest_node(grafo1, estacion$x, estacion$y)
        
        connect_nearest_nodes(grafo1, estacion$x, estacion$y)
        
        nodo_inicio <- get_start_node(grafo1, lugar)
        
        camino_mas_corto <- shortest_paths(grafo1, from = nodo_inicio, to = nodo, mode = "out")
        
        mapa <- add_path_to_map(mapa, grafo1, camino_mas_corto$vpath[[1]])
        #print("MAPA DIKSTRA METRO")
      }
    }
    print(ruta)
    
  }
}
library(mapview)
library(webshot)
library(leaflet)


mapa

mapa2 <- mapa %>% addProviderTiles(providers$CartoDB.Positron)
mapa2

mapshot(mapa2, file = "./fotos/CatedralBlanco.png")


posibles_puntos <- c("Mercado Central Valencia")
posibles_distancias <- c(1000)

for (lugar in posibles_puntos) {
  for (distancia in posibles_distancias) {
    sitio <- paste(lugar, distancia)
    sitio_sin_espacios <- gsub(" ", "", sitio)  
    ruta <- paste0("./mapas/", sitio_sin_espacios, ".rds")
    
    nodo_entrada <- get_start_node(grafo, lugar)
    
    estaciones_selec <- get_gtfs_distance(gtfs_metro, grafo, nodo_entrada, distancia)
    
    subgraf <- isochron(grafo, nodo_entrada$x, nodo_entrada$y, E(grafo)$length, distancia)
    
    start_node <- find_nearest_node(grafo, nodo_entrada$x, nodo_entrada$y)
    
    mapa <- plot_isochron(subgraf, start_node, estaciones_selec, distancia)
    
    if (nrow(estaciones_selec) > 0) {
      for (i in 1:nrow(estaciones_selec)) {
        estacion <- estaciones_selec[i,]
        
        grafo1 <- add_vertices(grafo, 1, name = estacion$station_name, x = estacion$x, y = estacion$y)
        
        nodo <- find_nearest_node(grafo1, estacion$x, estacion$y)
        
        connect_nearest_nodes(grafo1, estacion$x, estacion$y)
        
        nodo_inicio <- get_start_node(grafo1, lugar)
        
        camino_mas_corto <- shortest_paths(grafo1, from = nodo_inicio, to = nodo, mode = "out")
        
        mapa <- add_path_to_mapBUS(mapa, grafo1, camino_mas_corto$vpath[[1]])
      }
    }
    print(ruta)
  }
}


mapa

mapa1 <- mapa %>% addProviderTiles(providers$CartoDB.Positron)
mapa1

mapshot(mapa1, file = "./fotos/MercadoBlanco.png")






