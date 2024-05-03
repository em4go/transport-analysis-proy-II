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
# ----------------------------------------------------------------------------------------------------------------------------
posibles_puntos <- c("Plaza de Toros de Valencia", "Estación del Norte Valencia",
                     "Universidad Politecnica de Valencia", "Estadio Mestalla", "Playa de la Malvarrosa", "Bioparc Valencia", 
                     "Estación Valencia-Cabanyal", "Ciudad de las Artes y las Ciencias", "Catedral de Valencia")

lugar_seleccionado <- "Plaza de Toros de Valencia"

crear_mapa <- function(lugar_seleccionado, distancia) {
  if (lugar_seleccionado == "Plaza de Toros de Valencia") {
    nodo_entrada <- get_start_node(grafo,  "Plaza de Toros de Valencia")
  } else if (lugar_seleccionado == "Estación del Norte Valencia") {
    nodo_entrada <- get_start_node(grafo,  "Estación del Norte Valencia")
  } else if (lugar_seleccionado == "Universidad Politecnica de Valencia") {
    nodo_entrada <- get_start_node(grafo,  "Universidad Politecnica de Valencia")
  } else if (lugar_seleccionado == "Estadio Mestalla") {
    nodo_entrada <- get_start_node(grafo,  "Estadio Mestalla")
  } else if (lugar_seleccionado == "Playa de la Malvarrosa") {
    nodo_entrada <- get_start_node(grafo,  "Playa de la Malvarrosa")
  } else if (lugar_seleccionado == "Bioparc Valencia") {
    nodo_entrada <- get_start_node(grafo,  "Bioparc Valencia")
  } else if (lugar_seleccionado == "Estación Valencia-Cabanyal") {
    nodo_entrada <- get_start_node(grafo,  "Estación Valencia-Cabanyal")
  } else if (lugar_seleccionado == "Ciudad de las Artes y las Ciencias") {
    nodo_entrada <- get_start_node(grafo,  "Ciudad de las Artes y las Ciencias")
  } else if (lugar_seleccionado == "Catedral de Valencia") {
    nodo_entrada <- get_start_node(grafo,  "Catedral de Valencia")
  } else {
    stop("El lugar seleccionado no está en la lista de opciones.")
  }
  
  # Definir la distancia
  medida <- E(grafo)$length # Medida de distancia
  
  # Obtener las estaciones cercanas
  estaciones_selec <- get_gtfs_distance(gtfs_metro, grafo, nodo_entrada, distancia) 
  
  # Obtener el subgrafo
  g1 <- isochron(grafo, nodo_entrada$x, nodo_entrada$y, medida, distancia)
  
  # Encontrar el nodo de inicio
  start_node <- find_nearest_node(grafo, nodo_entrada$x, nodo_entrada$y)
  
  # Dibujar el subgrafo
  mi_mapa <- plot_isochron(g1, start_node, estaciones_selec, distancia)
  
  return(mi_mapa)
}

mi_mapa <- crear_mapa(lugar_seleccionado, 1000)
mi_mapa


info_mapas <- data.frame(lugar = character(), mapa = )

for (lugar in posibles_puntos) {
  print(lugar)
  mi_mapa <- crear_mapa(lugar, 1000)
  print(mi_mapa)
}

```{r}






