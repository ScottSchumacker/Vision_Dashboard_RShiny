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
      #menuItem("About", tabName = "about"),
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
                valueBoxOutput("avgValueOut", width = 6),
              ),
              
              fluidRow(
                box(
                  title = "Plot Inputs", status = "primary", solidHeader = TRUE,
                  collapsible = FALSE,
                  column(6,
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
                         h4("Black")
                  ),
                  
                  column(6,
                         selectInput(inputId = "location2", "Select a location",
                                     choices = unique(eyeHealth$LocationDesc),
                                     selected = "New Jersey"),
                         selectInput(inputId = "gender2", "Select a gender",
                                     choices = unique(eyeHealth$Gender),
                                     selected = "All genders"),
                         selectInput(inputId = "age2", "Select an age group",
                                     choices = unique(eyeHealth$Age),
                                     selected = "All ages"),
                         selectInput(inputId = "ethnicity2", "Select an ethnicity",
                                     choices = unique(eyeHealth$RaceEthnicity),
                                     selected = "All races"),
                         h4("Red")
                  )
                ),
                
                # Time Series Plot
                column(6,plotlyOutput("timePlot"))
              ),
              
              # Row for Bar plot
              fluidRow(
                column(6, plotlyOutput("riskPlot")),
                column(6, plotlyOutput("agePlot"))
              ),
              
              # Row for Location Bar Plot
              fluidRow(12, plotlyOutput("locationPlot"))
      )
    ),
    
  )
)

server <- function(input, output) {
  
  # Calculating mean prevalence
  avgDF <- eyeHealth %>% 
    filter(Age == "All ages", Gender == "All genders", 
           RaceEthnicity == "All races", RiskFactor == "All participants",
           RiskFactorResponse == "All participants", LocationDesc == "National")
  
  avgPrevValue <- round(mean(avgDF$Data_Value), 2)
  avgSampleSize <- round(mean(avgDF$Sample_Size),0)
  
  output$avgValueOut <- renderValueBox({
    valueBox(
      paste0(avgPrevValue, "%"), "Mean Vision Disability Prevalence", icon = icon("list"),
      color = "blue"
    )
  })
  
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
      geom_point(size = 4, color = "black") + xlab("Year") + ylab("Prevalence (%)") +
      geom_line(data = DF2(), aes(x = YearStart, y = Data_Value), color = "red") +
      geom_point(data = DF2(), aes(x = YearStart, y = Data_Value), color = "red", size = 4) +
      ggtitle("Prevalence of Severe Vision Disability Over Time")
    ggplotly(p)
  })
  
  riskFactorDF <- eyeHealth %>% 
    filter(Age == "All ages", Gender == "All genders", 
           RaceEthnicity == "All races", Data_Value_Type == "Crude Prevalence",
           RiskFactorResponse == "Yes", LocationDesc == "National") %>% 
    group_by(RiskFactor) %>% 
    summarise(mean_prevalence = mean(Data_Value))
  
  output$riskPlot <- renderPlotly({
    p2 <- ggplot(riskFactorDF, aes(x = RiskFactor, y = mean_prevalence)) +
      geom_bar(stat = "identity") + xlab("Risk Factor") +
      ylab("Mean Prevalence (%)") + ggtitle("Risk Factor Comparison")
    ggplotly(p2)
  })
  
  ageDF <- eyeHealth %>% 
    filter(Gender == "All genders", 
           RaceEthnicity == "All races", Data_Value_Type == "Crude Prevalence",
           RiskFactorResponse == "All participants", RiskFactor == "All participants",
           LocationDesc == "National") %>% 
    filter(Age != "All ages") %>% 
    group_by(Age) %>% 
    summarise(mean_prevalence = mean(Data_Value))
  
  output$agePlot <- renderPlotly({
    p3 <- ggplot(ageDF, aes(x = Age, y = mean_prevalence)) +
      geom_bar(stat = "identity") + xlab("Age Group") + ylab("Mean Prevalence (%)") +
      ggtitle("Age Group Comparison")
    ggplotly(p3)
  })
  
  # Location Bar Plot
  locationDF <- eyeHealth %>% 
    filter(Gender == "All genders", 
           RaceEthnicity == "All races", Age == "All ages", 
           Data_Value_Type == "Crude Prevalence",
           RiskFactorResponse == "All participants", 
           RiskFactor == "All participants") %>% 
    filter(LocationDesc != "National") %>% 
    group_by(LocationDesc) %>% 
    summarise(mean_prevalence = mean(Data_Value)) %>% 
    arrange(desc(mean_prevalence)) %>% 
    head(10)
  
  output$locationPlot <- renderPlotly({
    p4 <- ggplot(locationDF, aes(x = LocationDesc, y = mean_prevalence)) +
      geom_bar(stat = "identity") + xlab("State") + ylab("Mean Prevalence (%)") +
      ggtitle("States With the Highest Mean Prevalence of Vision Disability")
    ggplotly(p4)
  })

}

shinyApp(ui, server)