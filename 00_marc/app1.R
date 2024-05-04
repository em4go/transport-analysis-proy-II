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
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
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
                                         )
                           ) # sidebarPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Bus", "JAJA"), # Navbar 2, tabPanel
                  tabPanel("Ferri", "LAI CABRÓN")
                  ) # NavbarPage
                ) # ui fluidPage


server1 <- function(input, output){
  observe({
    
    sitio <- paste(input$lugar, input$metros)
    sitio_sin_espacios <- gsub(" ", "", sitio)  
    ruta <- paste0("./mapas/", sitio_sin_espacios, ".rds")
    mapa <- readRDS(ruta)
    
    output$mapa <- renderLeaflet(mapa)
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server1)

