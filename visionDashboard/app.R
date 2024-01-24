# Scott Schumacker
# Vision Dashboard

# Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Vision Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Vision Data", tabName = "data")
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "about",
        
      ),
      
      tabItem(tabName = "data",
          selectInput(inputId = "state", "Select a location",
                      choices = eyeHealth$LocationDesc)    
              
      )
    ),
    
  )
)

server <- function(input, output) {

}

shinyApp(ui, server)