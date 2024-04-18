.lib<- c("igraph","osmdata", "dplyr", "leaflet", "ggplot2", "tidytransit")


.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst])
lapply(.lib, require, character.only=TRUE)

source("01_main/utils.R")


grafo <- read_graph("./data/networks_data/valencia_drive.graphml", format = "graphml")

V(grafo)$x <- as.numeric(V(grafo)$x)
V(grafo)$y <- as.numeric(V(grafo)$y)
E(grafo)$length <- as.numeric(E(grafo)$length)

gtfs <- read_gtfs("data/gtfs_data/transit_metrovalencia.zip")

# Paradas de metro
paradas_metro <- gtfs$stops
head(paradas_metro)

# para las 5 primeras paradas de metro:
paradas <- paradas_metro$stop_name[1:5]

for (parada in paradas) {
  print(parada)
  parada <- paradas_metro[paradas_metro$stop_name == parada, ]
  iso_graph <- isochron_graph(grafo, parada$stop_lon, parada$stop_lat, "length", 500)
  m <- plot_isochron(iso_graph)
  m
}

benimaclet <- paradas_metro[paradas_metro$stop_name == "La Carrasca", ]
benimaclet$stop_lat
benimaclet$stop_lon

grafo

iso_graph <- isochron_graph(grafo, benimaclet$stop_lon, benimaclet$stop_lat, "length", 300)

m <- plot_isochron(iso_graph)
m
