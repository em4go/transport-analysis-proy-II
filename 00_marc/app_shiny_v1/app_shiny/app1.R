# Load R packages
library(shiny)
library(shinythemes)

#library(dplyr)
library(leaflet)
#library(leaflet.providers)
#library(ggplot2)
#library(tidytransit)
library(sf)
#library(lubridate)
#library(igraph)
#library(osmdata)
library(stringi)


options(shiny.host = "0.0.0.0")
options(shiny.port = 3838)

ui <- fluidPage(theme = shinytheme("yeti"),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
                ), 
                navbarPage(
                 #theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Proyecto2",
                  tabPanel("Transporte",
                           
                           mainPanel(
                             h1("MAPA"),
                             
                             leafletOutput("mapa", height = 700),
                             h5(HTML("El color <span style='color:yellow;font-weight: bold;'>AMARILLO</span> indica el inicio")),
                             h5(HTML("El color <span style='color:red; font-weight: bold;'>ROJO</span> indica que es una parada de metro-tranvía")),
                             h5(HTML("El color <span style='color:white;font-weight: bold;'>NEGRO</span> indica que es una parada de bus")),
                             
                           ), # mainPanel
      
                           
                           sidebarPanel(
                             tags$h3(HTML("<span style='color:black;'>SELECCIONAR LAS VARIABLES</span>")),
                             
                             selectInput("lugar", "Indique donde se encuentra",
                                         choices = c("Plaza de Toros de Valencia", "Estación del Norte Valencia",
                                                                        "Universidad Politecnica de Valencia", "Estadio Mestalla", "Playa de la Malvarrosa", "Bioparc Valencia", 
                                                                        "Estación Valencia-Cabanyal", "Ciudad de las Artes y las Ciencias", "Catedral de Valencia", 
                                                                        "Mercado Central Valencia")),
                             
                             selectInput("metros", "Número de metros de radio", 
                                         choices = c(250, 500, 750, 1000)
                                         ),
                             
                             checkboxGroupInput("transporte",
                                                label = "Selecciona el medio de transporte:",
                                                choices = list("Metro" = "METRO", "Bus" = "BUS")
                                               ),
                             fluidRow(verbatimTextOutput("value")),
                             
                             fluidRow(
                               div(p("Recuerda que una persona normal, tiende a recorrer 100 metros en 1 o 2 minutos"), class = "text-center")
                             )
                           )# sidebarPanel
                           
                  ),
                  tabPanel("Metro",
                           mainPanel(
                             h1("GRAFO"),
                             
                             leafletOutput("grafoMETRO", height = 700),
          
                                      ), # mainPanel
                           
                           sidebarPanel(
                             tags$h3(HTML("<span style='color:black;'>SELECCIONAR LAS MEDIDAS</span>")),
                             
                             selectInput("color", "Indique que medida usará para el color",
                                         choices = c("PageRank", "Intermediación", "Cercanía", "VectorPropio")
                                         ),
                             
                             selectInput("tamanyo", "Indique que medida usará para el tamaño", 
                                         choices = c("PageRank", "Intermediación", "Cercanía", "VectorPropio")
                                         )
                             
                            )# sidebarPanel
                      
                           
                            ),
                  
                  tabPanel("Ratio",
                           fluidRow(
                             column(width = 6,
                                    tags$h3(HTML("Mapa Metro"), align = "left"),
                                    leafletOutput("mapaMetro", height = 350)
                             ), 
                             column(width = 6,
                                    tags$h3("Mapa Bus", align = "left"),
                                    leafletOutput("mapaBus", height = 350)
                             )
                           ), # fluidRow
                           fluidRow(
                             column(width = 6,
                                    tags$h3("Mapa Valenbici", align = "left"),
                                    leafletOutput("mapaValenbici", height = 350)
                             ),
                             column(width = 6,
                                    tags$h3("Mapa Eco", align = "left"),
                                    leafletOutput("mapaEco", height = 350)
                             )
                           ) # fluidRow
                  ), # Navbar , tabPanel
                  tabPanel("Clustering de Barrios",
                           
                           mainPanel(
                             h1("MAPA DE CLUSTERS"),
                             
                             leafletOutput("mapaCLuster", height = 700)
                             ),
                           
                           
                           sidebarPanel(
                             tags$h3(HTML("<span style='color:black;'>SELECCIONAR EL MAPA</span>")),
                             
                             checkboxGroupInput("cluster",
                                                 label = "Selecciona el cluster para mostrar:",
                                                 choices = list("LineasMetro" = "LineasMetro", "Caracterización" = "Caracterización")
                             ),
                             
                             leafletOutput("mapaDistritos", height = 400)),
                             
                             
                             ),
                  
                  tabPanel("Mapas de Calor",
                           fluidRow(
                             column(width = 6,
                                    tags$h3(HTML("Mapa Metro"), align = "left"),
                                    leafletOutput("hexmetro", height = 350)
                             ), 
                             column(width = 6,
                                    tags$h3("Mapa Valenbisi", align = "left"),
                                    leafletOutput("hexbici", height = 350)
                             )
                           ), # fluidRow
                           fluidRow(
                             column(width = 6,
                                    tags$h3("Mapa Bus", align = "left"),
                                    leafletOutput("hexBus", height = 350)
                             )
                           ) # fluidRow
                  ), # Navbar , tabPanel
                           
                           #fluidRow(
                          #   column(width = 6,
                           #         tags$h3("Mapa Clustering", align = "left"),
                           #         leafletOutput("mapaClusters", height = 450)
                           #  ),
                            # column(width = 6,
                           #         tags$h3("Mapa Distritos", align = "left"),
                           #         leafletOutput("mapaDistritos", height = 450)
                           #  )
                          # ),
                          #
                          # tags$h4("El análisis del mapa muestra una relación entre la ubicación de los barrios y sus características. 
                          #         Los clusters 4 (azul) representan áreas céntricas con alto nivel de vida y servicios. 
                            #       El cluster 3 (verde) abarca barrios periféricos con menor nivel de vida. 
                            #       El cluster 5 (morado) destaca por su alta densidad de transporte público, 
                           #        especialmente en el centro. Otros barrios muestran similitudes, excepto por los 
                           #        del cluster 2 (amarillo), que tienen más zonas verdes.")
                           
                  ),# NavbarPage
            )  # ui fluidPage

server1 <- function(input, output){
  observe({
    
    if (length(input$transporte) == 0) {
      output$value <- renderPrint({ "No has seleccionado ningún transporte" })
      mapa <- readRDS("./mapas/valencia.rds")
      output$mapa <- renderLeaflet(mapa)
    }
    
    else {
      sitio <- paste(input$lugar, input$metros)
      sin_acentos <- stri_trans_general(sitio, "Latin-ASCII")
      sitio_sin_espacios <- gsub(" ", "", sin_acentos)
      if (length(input$transporte) == 1) {
      if (input$transporte == "METRO") {
        output$value <- renderPrint({ "Has seleccionado el metro" })
        ruta <- paste0("./mapas/", sitio_sin_espacios, ".rds")
      }
      else {
        output$value <- renderPrint({ "Has seleccionado el bus" })
        ruta <- paste0("./mapas/", sitio_sin_espacios, "BUS.rds")
      }
    }
      else {      
        output$value <- renderPrint({ "Has seleccionado el metro y el bus" })
        ruta <- paste0("./mapas/", sitio_sin_espacios, "TODOS.rds")
       }
      print(ruta)
      mapa <- readRDS(ruta)        
      output$mapa <- renderLeaflet(mapa)
    }
    
    #--------------------------------
    #---RATIO
    mapaMetro <- readRDS("./mapas/ratio_metro.rds")
    mapaBus <- readRDS("./mapas/ratio_emt.rds")
    mapaValenbici <- readRDS("./mapas/ratio_valenbisi.rds")
    mapaEco <- readRDS("./mapas/ratio_eco.rds")
    
    output$mapaMetro <- renderLeaflet(mapaMetro)
    output$mapaBus <- renderLeaflet(mapaBus)
    output$mapaValenbici <- renderLeaflet(mapaValenbici)
    output$mapaEco <- renderLeaflet(mapaEco)
    
    #--------------------------------
    
    #---HEX
    
    hexmetromapa <- readRDS("./mapas/hex_metro.RDS")
    hexbicimapa <- readRDS("./mapas/hex_valenbisi.RDS")
    hexBusmapa <- readRDS("./mapas/hex_bus.RDS")
    
    output$hexmetro <- renderLeaflet(hexmetromapa)
    output$hexbici <- renderLeaflet(hexbicimapa)
    output$hexBus <- renderLeaflet(hexBusmapa)
    
    
    #---CLUSTERS
    if (length(input$cluster) == 1){
      
      if (input$cluster == "LineasMetro") {
        
        mapaclus <- readRDS("./mapas/cluster_metros.rds")
        output$mapaCLuster <- renderLeaflet(mapaclus)
        
      }
      
      else
      
      mapaclus <- readRDS("./mapas/cluster_barrios.rds")
      output$mapaCLuster <- renderLeaflet(mapaclus)
      
    }
    
    else if (length(input$cluster) == 2) {
      
      mapaclus <- readRDS("./mapas/metros_barrios_cluster.rds")
      output$mapaCLuster <- renderLeaflet(mapaclus)
    }
    
    else {
      
      mapaclus <- readRDS("./mapas/valencia.rds")
      output$mapaCLuster <- renderLeaflet(mapaclus)
    }
    
    
    #---DISTRITOS
    mapadist <- readRDS("./mapas/distrito_barrios.rds")
    output$mapaDistritos <- renderLeaflet(mapadist)
    
    #---METRO
    # c("PageRank", "Intermediación", "Cercanía", "VectorPropio")
    # pagerank, betweenness, closeness, eigenvector

    if (input$color == "PageRank"){
      color <- "pagerank"
    } else if (input$color == "Intermediación"){
      color <- "betweenness"
    } else if (input$color == "Cercanía"){
      color <- "closeness"
    } else if (input$color == "VectorPropio"){
      color <- "eigenvector"
    }
    
    if (input$tamanyo == "PageRank"){
      tamanyo <- "pagerank"
    } else if (input$tamanyo == "Intermediación"){
      tamanyo <- "betweenness"
    } else if (input$tamanyo == "Cercanía"){
      tamanyo <- "closeness"
    } else if (input$tamanyo == "VectorPropio"){
      tamanyo <- "eigenvector"
    }
    ruta0 <- "./mapas/grafo/"
    ruta1 <- paste0(ruta0, color, tamanyo, ".rds") 
    
    grafMETRO <- readRDS(ruta1)
    output$grafoMETRO <- renderLeaflet(grafMETRO)
    
    
  })
  
} # server

# Create Shiny object
shinyApp(ui = ui, server = server1)

