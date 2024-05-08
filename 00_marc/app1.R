# Load R packages
library(shiny)
library(shinythemes)

library(dplyr)
library(leaflet)
library(ggplot2)
library(tidytransit)
library(sf)
library(lubridate)
library(igraph)
library(osmdata)

ui <- fluidPage(theme = shinytheme("yeti"),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "fondo.css")
                ),
                
                navbarPage(
                 #theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Proyecto2",
                  tabPanel("Metro",
                           
                           mainPanel(
                             h1("MAPA Resultante"),
                             
                             leafletOutput("mapa", height = 500)
                           ), # mainPanel
      
                           
                           sidebarPanel(
                             tags$h3("SELECCIONAR LAS VARIABLES"),
                             
                             selectInput("lugar", "Indique donde se encuentra",
                                         choices = c("Plaza de Toros de Valencia", "Estación del Norte Valencia",
                                                     "Universidad Politecnica de Valencia", "Estadio Mestalla", "Playa de la Malvarrosa", "Bioparc Valencia", 
                                                     "Estación Valencia-Cabanyal", "Ciudad de las Artes y las Ciencias", "Catedral de Valencia")
                                         ),
                             
                             selectInput("metros", "Número de metros de radio", 
                                         choices = c(100, 500, 750, 1000, 2000)
                                         ),
                             fluidRow(
                               div(p("Recuerda que una persona normal, tiende a recorrer 100 metros en 1 o 2 minutos"), class = "text-center")
                             )
                           ) # sidebarPanel
                           
                  ),
                  tabPanel("Bus",
                           
                           mainPanel(
                             h1("MAPA Resultante"),
                             
                             leafletOutput("mapaBUS", height = 500)
                           ), # mainPanel
                           
                           
                           sidebarPanel(
                             tags$h3("SELECCIONAR LAS VARIABLES"),
                             
                             selectInput("lugarbus", "Indique donde se encuentra",
                                         choices = c("Plaza de Toros de Valencia", "Estación del Norte Valencia")
                             ),
                             
                             selectInput("metrosbus", "Número de metros de radio", 
                                         choices = c(100, 200)
                             )
                           )
                             
                           
                           ),# Navbar 1, tabPanel
                  tabPanel("Ratio",
                           fluidRow(
                             column(width = 6,
                                    tags$h3("Mapa Metro", align = "left"),
                                    leafletOutput("mapaMetro", height = 300)
                             ), 
                             column(width = 6,
                                    tags$h3("Mapa Bus", align = "left"),
                                    leafletOutput("mapaBus", height = 300)
                             )
                           ), # fluidRow
                           fluidRow(
                             column(width = 6,
                                    tags$h3("Mapa Valenbici", align = "left"),
                                    leafletOutput("mapaValenbici", height = 300)
                             ),
                             column(width = 6,
                                    tags$h3("Mapa Eco", align = "left"),
                                    leafletOutput("mapaEco", height = 300)
                             )
                           ) # fluidRow
                  ), # Navbar , tabPanel
                  tabPanel("Clustering de Barrios",
                           
                           mainPanel(
                             h1("MAPA clusters"),
                             
                             leafletOutput("mapaClusters", height = 700)
                           ), # mainPanel
                           
                  ),
            )) # NavbarPage # ui fluidPage

server1 <- function(input, output){
  observe({
    #---METRO
    sitio <- paste(input$lugar, input$metros)
    sitio_sin_espacios <- gsub(" ", "", sitio)  
    ruta <- paste0("./mapas/", sitio_sin_espacios, ".rds")
    mapa <- readRDS(ruta)
    
    output$mapa <- renderLeaflet(mapa)
    
    #---BUS
    sitiobus <- paste(input$lugarbus, input$metrosbus)
    sitio_sin_espaciosbus <- gsub(" ", "", sitiobus)  
    rutabus <- paste0("./mapas/", sitio_sin_espaciosbus, "BUS.rds")
    mapabus <- readRDS(rutabus)
    
    output$mapaBUS <- renderLeaflet(mapabus)

    #---RATIO
    mapaMetro <- readRDS("./mapas/ratio_metro.rds")
    mapaBus <- readRDS("./mapas/ratio_emt.rds")
    mapaValenbici <- readRDS("./mapas/ratio_valenbisi.rds")
    mapaEco <- readRDS("./mapas/ratio_eco.rds")
    
    output$mapaMetro <- renderLeaflet(mapaMetro)
    output$mapaBus <- renderLeaflet(mapaBus)
    output$mapaValenbici <- renderLeaflet(mapaValenbici)
    output$mapaEco <- renderLeaflet(mapaEco)
    
    #---CLUSTERS
    mapaclus <- readRDS("./mapas/cluster_barrios.rds")
    output$mapaClusters <- renderLeaflet(mapaclus)
    
  })
  
} # server

# Create Shiny object
shinyApp(ui = ui, server = server1)

