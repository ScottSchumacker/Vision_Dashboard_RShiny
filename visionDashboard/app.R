# Scott Schumacker
# Vision Dashboard

# Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(renv)

ui <- dashboardPage(
  dashboardHeader(title = "Vision & Eye Health"),
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
              
              fluidRow(
                
                column(4,
                       h4("Line 1"),
                       selectInput(inputId = "location", "Select a location",
                                   choices = unique(eyeHealth$LocationDesc),
                                   selected = "National"),
                       selectInput(inputId = "gender", "Select a gender",
                                   choices = unique(eyeHealth$Gender),
                                   selected = "All genders"),
                       selectInput(inputId = "age", "Select an age group",
                                   choices = unique(eyeHealth$Age),
                                   selected = "All ages"),
                       selectInput(inputId = "ethnicity", "Select an ethnicity",
                                   choices = unique(eyeHealth$RaceEthnicity),
                                   selected = "All races"), 
                       ),
                
                column(4,
                       h4('Line 2'),
                       selectInput(inputId = "location2", "Select a location",
                                   choices = unique(eyeHealth$LocationDesc),
                                   selected = "National"),
                       selectInput(inputId = "gender2", "Select a gender",
                                   choices = unique(eyeHealth$Gender),
                                   selected = "All genders"),
                       selectInput(inputId = "age2", "Select an age group",
                                   choices = unique(eyeHealth$Age),
                                   selected = "All ages"),
                       selectInput(inputId = "ethnicity2", "Select an ethnicity",
                                   choices = unique(eyeHealth$RaceEthnicity),
                                   selected = "All races"), 
                       )
                
              ),
              
              fluidRow(
                # Time Series Plot
                plotlyOutput("timePlot") 
              )
      )
    ),
    
  )
)

server <- function(input, output) {
  
  # Data Subset
  DF <- reactive({
    eyeHealth %>% filter(LocationDesc == input$location, Age == input$age,
                         Gender == input$gender, 
                         RaceEthnicity == input$ethnicity,
                         RiskFactor == "All participants",
                         Data_Value_Type == "Crude Prevalence")
  })
  
  DF2 <- reactive({
    eyeHealth %>% filter(LocationDesc == input$location2, Age == input$age2,
                         Gender == input$gender2, 
                         RaceEthnicity == input$ethnicity2,
                         RiskFactor == "All participants",
                         Data_Value_Type == "Crude Prevalence")
  })
  
  # Time series plot
  output$timePlot <- renderPlotly({
    p <- ggplot(DF(), aes(x = YearStart, y = Data_Value)) +
      geom_line(color = "black") +
      geom_point(size = 4, color = "black") + xlab("Year") + ylab("Crude Prevalence (%)") +
      geom_line(data = DF2(), aes(x = YearStart, y = Data_Value), color = "red") +
      geom_point(data = DF2(), aes(x = YearStart, y = Data_Value), color = "red", size = 4) +
      ggtitle("Prevalence of Severe Vision Disability")
    ggplotly(p)
  })

}

shinyApp(ui, server)