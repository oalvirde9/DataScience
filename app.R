#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(quantmod)
library(dplyr)
library(shiny)
library(readr)

Mortalidad_shiny <- read_csv("https://raw.githubusercontent.com/oalvirde9/DataScience/main/Mortalidad_shiny.csv")
View(Mortalidad_shiny)

plot_epicurve <- function(data, causa = "All") {
  
  if (!("All" %in% causa)) {
    data <- data %>%
      filter(Causa_muerte %in% causa)
    
    plot_title_causa <- stringr::str_glue("{paste0(causa, collapse = ', ')} causas")
    
  } else {
    
    plot_title_causa <- "Todas las causas"
    
  }
  
  # si no quedan datos, devuelve NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  
  ggplot(data, aes(x = Nivel_marginacion, y = Prevalencia)) +
    geom_col(width = 1, fill = "gray9") +
    theme_minimal() +
    labs(
      x = "Nivel de Marginación",
      y = "Prevalencia %",
      title = stringr::str_glue("Causa de muerte - {plot_title_causa}"),
    )
}
plot_epicurve(Mortalidad_shiny, causa =  "Suicidios")

ui <- fluidPage(
  
  titlePanel("Visualizador de prevalencia por causas de muerte y nivel de marginación"),
  
  sidebarLayout(
    
    sidebarPanel(
      # selector para el distrito
      selectInput(
        inputId = "select_causa",
        label = "Select causa",
        choices = c(
          "Homicidio",
          "Suicidios",
          "Desnutricion",
          "Virus",
          "Diabetes mellitus",
          "Accidentes de transporte",
          "Enfermedades cardiacas",
          "Enfermedades digestivas",
          "Infecciones respiratorias",
          "Tuberculosis"
          
        ),
        selected = "Accidentes de transporte",
        multiple = FALSE
      ),
      
    ),
    
    
    mainPanel(
      # La curva epidemiológica va aquí
      plotOutput("mortalidad_epicurve")
    )
    
  )
)


server <- function(input, output, session) {
  
  output$mortalidad_epicurve <- renderPlot(
    plot_epicurve(Mortalidad_shiny, causa =  input$select_causa)
  )
  
}

shinyApp(ui = ui, server = server)

