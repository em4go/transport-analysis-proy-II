# Librerías necesarias
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidytransit)
library(sf)
library(lubridate)
library(igraph)
library(osmdata)

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

# Cargar el GTFS de EMT Valencia, para las paradas de autobús
gtfs_emt <- read_gtfs("../data/gtfs_data/transit_emt_valencia.zip")


# Ejemplo de uso de las funciones auxiliares
# ----------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------
nodo_entrada <- get_start_node(grafo, "Plaza de Toros de Valencia") # Nodo de entrada

X <- nodo_entrada$x # Coordenada X
Y <- nodo_entrada$y # Coordenada Y

medida <- E(grafo)$length # Medida de distancia
distancia <- 100 # Distancia en metros

estaciones_selec <- get_gtfs_distance(gtfs_metro, grafo, nodo_entrada, distancia) # La matriz de estaciones cercanas 

g1 <- isochron(grafo, X, Y, medida, distancia) # Subgrafo

start_node <- find_nearest_node(grafo, X, Y) # Nodo de inicio

plot_isochron(g1, start_node, estaciones_selec, distancia) # Dibujar el subgrafo

# ----------------------------------------------------------------------------------------------------------------------------
#METROO
# ----------------------------------------------------------------------------------------------------------------------------
posibles_puntos <- c("Plaza de Toros de Valencia", "Estación del Norte Valencia",
                     "Universidad Politecnica de Valencia", "Estadio Mestalla", "Playa de la Malvarrosa", "Bioparc Valencia", 
                     "Estación Valencia-Cabanyal", "Ciudad de las Artes y las Ciencias", "Catedral de Valencia")

lugar_seleccionado <- "Plaza de Toros de Valencia"

posibles_distancias <- c(100, 500, 750, 1000, 2000)



posibles_puntos1 <- c("Plaza de Toros de Valencia", "Estación del Norte Valencia")
posibles_distancias1 <- c(100, 200)


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
    
    saveRDS(mapa, file = ruta)
    print(ruta)
  }
}

# ----------------------------------------------------------------------------------------------------------------------------
#BUS
posibles_puntos1 <- c("Plaza de Toros de Valencia", "Estación del Norte Valencia")
posibles_distancias1 <- c(100, 200)

for (lugar in posibles_puntos1) {
  for (distancia in posibles_distancias1) {
    sitio <- paste(lugar, distancia, "BUS")
    sitio_sin_espacios <- gsub(" ", "", sitio)  
    ruta <- paste0("./mapas/", sitio_sin_espacios, ".rds")
    
    nodo_entrada <- get_start_node(grafo, lugar)
    
    estaciones_selec <- get_gtfs_distance(gtfs_emt, grafo, nodo_entrada, distancia)
    
    subgraf <- isochron(grafo, nodo_entrada$x, nodo_entrada$y, E(grafo)$length, distancia)
    
    start_node <- find_nearest_node(grafo, nodo_entrada$x, nodo_entrada$y)
    
    mapa <- plot_isochron(subgraf, start_node, estaciones_selec, distancia)
    
    saveRDS(mapa, file = ruta)
    print(ruta)
  }
}