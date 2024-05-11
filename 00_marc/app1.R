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
                  tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
                ), 
                navbarPage(
                 #theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Proyecto2",
                  tabPanel("Transport",
                           
                           mainPanel(
                             h1("MAPA"),
                             
                             leafletOutput("mapa", height = 500),
                             h5(HTML("El color <span style='color:yellow;font-weight: bold;'>AMARILLO</span> indica el inicio")),
                             h5(HTML("El color <span style='color:red; font-weight: bold;'>ROJO</span> indica que es una parada de metro-tranvía")),
                             h5(HTML("El color <span style='color:white;font-weight: bold;'>NEGRO</span> indica que es una parada de bus")),
                             
                           ), # mainPanel
      
                           
                           sidebarPanel(
                             tags$h3(HTML("<span style='color:black;'>SELECCIONAR LAS VARIABLES</span>")),
                             
                             selectInput("lugar", "Indique donde se encuentra",
                                         choices = c("Plaza de Toros de Valencia", "Estación del Norte Valencia")),
                             
                             selectInput("metros", "Número de metros de radio", 
                                         choices = c(300, 500)
                                         ),
                             
                             checkboxGroupInput("transporte",
                                                label = "Selecciona el medio de transporte:",
                                                choices = list("Metro" = "METRO", "Bus" = "BUS")
                                               ),
                             fluidRow(verbatimTextOutput("value")),
                             
                             fluidRow(
                               div(p("Recuerda que una persona normal, tiende a recorrer 100 metros en 1 o 2 minutos"), class = "text-center")
                             )
                           ) # sidebarPanel
                           
                  ),
                  tabPanel("Ratio",
                           fluidRow(
                             column(width = 6,
                                    tags$h3(HTML("Mapa Metro"), align = "left"),
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
                             
                             leafletOutput("mapaClusters", height = 700))
                           ), # mainPanel
                           
                  ),# NavbarPage
            )  # ui fluidPage

server1 <- function(input, output){
  observe({
    
    if (length(input$transporte) == 0) {
      output$value <- renderPrint({ "No has seleccionado ningún transporte" })
      
      mapa <- readRDS("./mapas/valencia.rds")
      output$mapa <- renderLeaflet(mapa)
      
    }
    
    else if (length(input$transporte) == 1) {
      if (input$transporte == "METRO") {
        output$value <- renderPrint({ "Has seleccionado el metro" })
        
        sitio <- paste(input$lugar, input$metros)
        sitio_sin_espacios <- gsub(" ", "", sitio)  
        ruta <- paste0("./mapas/", sitio_sin_espacios, ".rds")
        mapa <- readRDS(ruta)
        
        output$mapa <- renderLeaflet(mapa)
      }
      else {
        output$value <- renderPrint({ "Has seleccionado el bus" })
        
        sitio <- paste(input$lugar, input$metros)
        sitio_sin_espacios <- gsub(" ", "", sitio)  
        ruta <- paste0("./mapas/", sitio_sin_espacios, "BUS.rds")
        mapa <- readRDS(ruta)
        
        output$mapa <- renderLeaflet(mapa)
      }
      
    }
    
    else {
      
      output$value <- renderPrint({ "Has seleccionado el bus y el metro" })
      
      sitio <- paste(input$lugar, input$metros)
      sitio_sin_espacios <- gsub(" ", "", sitio)  
      ruta <- paste0("./mapas/", sitio_sin_espacios, "TODOS.rds")
      mapa <- readRDS(ruta)
      
      output$mapa <- renderLeaflet(mapa)
      
    }
    
    #---METRO
    #sitio <- paste(input$lugar, input$metros)
    #sitio_sin_espacios <- gsub(" ", "", sitio)  
    #ruta <- paste0("./mapas/", sitio_sin_espacios, ".rds")
    #mapa <- readRDS(ruta)
    
    #output$mapa <- renderLeaflet(mapa)
    
    #---BUS
    #sitiobus <- paste(input$lugarbus, input$metrosbus)
    #sitio_sin_espaciosbus <- gsub(" ", "", sitiobus)  
    #rutabus <- paste0("./mapas/", sitio_sin_espaciosbus, "BUS.rds")
    #mapabus <- readRDS(rutabus)
    
    #output$mapaBUS <- renderLeaflet(mapabus)
    
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
    
    #---CLUSTERS
    mapaclus <- readRDS("./mapas/cluster_barrios.rds")
    output$mapaClusters <- renderLeaflet(mapaclus)
    
  })
  
} # server

# Create Shiny object
shinyApp(ui = ui, server = server1)

