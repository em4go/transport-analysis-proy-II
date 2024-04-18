.lib<- c("igraph","osmdata", "dplyr", "leaflet", "ggplot2")


.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst])
lapply(.lib, require, character.only=TRUE)


grafo <- read_graph("./data/networks_data/valencia_drive.graphml", format = "graphml")

degrees <- degree(grafo)
V(grafo)$degree <- degrees
ggplot(data.frame(degrees), aes(x=degrees)) + geom_histogram(binwidth = 1, fill="blue", color="black") + theme_minimal() + labs(title="Distribución de grados", x="Grado", y="Frecuencia")

betweenness <- betweenness(grafo)
betweenness <- betweenness / max(betweenness)

ggplot(data.frame(betweenness), aes(x=betweenness)) + geom_histogram(binwidth = 0.01, fill="blue", color="black") + theme_minimal() + labs(title="Distribución de intermediación", x="Intermediación", y="Frecuencia")

closeness <- closeness(grafo)
closeness

degree_centrality <- degree(grafo, mode="in")
degree_centrality

ggplot(data.frame(degree_centrality), aes(x=degree_centrality)) + geom_histogram(binwidth = 0.01, fill="blue", color="black") + theme_minimal() + labs(title="Distribución de centralidad de grado", x="Centralidad de grado", y="Frecuencia")

autovector_centralities <- eigen_centrality(grafo)$vector
autovector_centralities[autovector_centralities > 0.2]

g_page_rank <- page_rank(grafo)

vec <- g_page_rank$vector[g_page_rank$vector > 0.0002]

ggplot(data.frame(vec), aes(x=vec)) + geom_histogram(binwidth = 0.0001, fill="blue", color="black") + theme_minimal() + labs(title="Distribución de PageRank", x="PageRank", y="Frecuencia")


