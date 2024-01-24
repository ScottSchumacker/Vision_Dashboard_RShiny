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
      
      # Location drop down
      tabItem(tabName = "data",
          selectInput(inputId = "location", "Select a location",
                      choices = eyeHealth$LocationDesc),
          
          # Time Series Plot
          plotOutput("timePlot"),
      )
    ),
    
  )
)

server <- function(input, output) {
  
  # Data Subset
  DF <- reactive({
    eyeHealth %>% filter(LocationDesc == input$location, Age == "All ages",
                         Gender == "All genders", 
                         RaceEthnicity == "All races",
                         RiskFactor == "All participants",
                         Data_Value_Type == "Crude Prevalence")
  })
  
  # Time series plot
  output$timePlot <- renderPlot({
    ggplot(DF(), aes(x = YearStart, y = Data_Value)) +
      geom_line() +
      geom_point(size = 4) + xlab("Crude Prevalence") + ylab("Year")
  })

}

shinyApp(ui, server)