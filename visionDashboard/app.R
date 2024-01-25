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
      menuItem("Vision & Eye Health Data", tabName = "data")
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "about",
        h1("About"),
        br(),
        p("This dashboard was created with data from the Centers for Disease 
        Control and Prevention Data Catalogue."),
        p("Data set: Behavioral Risk Factors - Vision and Eye Health
          Surveillance."),
        p("Data link: https://data.cdc.gov/Vision-Eye-Health/Behavioral-Risk-Factors-Vision-and-Eye-Health-Surv/vkwg-yswv/about_data"),
        p("Dashboard Creator and Maintainer: Scott Schumacker")
      ),
      
      # Location drop down
      tabItem(tabName = "data",
          selectInput(inputId = "location", "Select a location",
                      choices = unique(eyeHealth$LocationDesc),
                      selected = "National"),
          
          # Time Series Plot
          plotlyOutput("timePlot"),
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
  output$timePlot <- renderPlotly({
    p <- ggplot(DF(), aes(x = YearStart, y = Data_Value)) +
      geom_line() +
      geom_point(size = 4) + xlab("Year") + ylab("Crude Prevalence")
    ggplotly(p)
  })

}

shinyApp(ui, server)