library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(leaflet)
library(tidyverse)


       tabPanel("mymap1",
        bootstrapPage(
          tags$style(type = "text/css", "#mymap1 {height: calc(100vh) !important;}"),
          
   
        leafletOutput('mymap1', width = "100%", height = "100%"),
        
        absolutePanel(
          top = 90,
          right = 30,
          draggable = T,
          width = "13%",
          
          selectizeGroupUI(
            id = "my_filters",
            params = list(
              genero = list(inputId = "genero", title = "Genero:"),
              especie = list(inputId = "especie", title = "Especie:")
          #    Estado = list(inputId = "Estado", title = "Estado:")
            ),
            inline = FALSE,
       #     btn_label = "Reiniciar la visualización"
          ),
        ),
        
      )
)
