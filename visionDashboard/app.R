# Scott Schumacker
# Vision Dashboard

# Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(renv)
library(lintr)
library(emoji)

ui <- dashboardPage(
  dashboardHeader(title = tags$u("Vision & Eye Health")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Explore", tabName = "data"),
      menuItem("About", tabName = "about")
    )
  ),
  
  dashboardBody(
    
    # Linking CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      tabItem(tabName = "about",
        tags$div(id = "aboutText",
          h1(tags$u("About")),
          br(),
          p(icon("database"), "Data Set: ", tags$a("Behavioral Risk Factors - Vision and Eye Health Surveillance (CDC)", href = "https://data.cdc.gov/Vision-Eye-Health/Behavioral-Risk-Factors-Vision-and-Eye-Health-Surv/vkwg-yswv/about_data")),
          p(icon("user"), "Dashboard Creator and Maintainer: Scott Schumacker"),
          p(emoji("smile"))
        )
      ),
      
      # Location drop down
      tabItem(tabName = "data",
              
        # Row for main metric
        fluidRow(
          valueBoxOutput("avgValueOut", width = 6),
          valueBoxOutput("avgValueOut2", width = 6)
        ),
              
        # Row for time series plot and inputs
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
              selectInput(inputId = "ethnicity", 
                          "Select an ethnicity",
                          choices = unique(eyeHealth$RaceEthnicity),
                          selected = "All races"),
              selectInput(inputId = "color", "Select a color",
                          choices = c("Black"),
                          selected = "All races")
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
              selectInput(inputId = "ethnicity2", 
                          "Select an ethnicity",
                          choices = unique(eyeHealth$RaceEthnicity),
                          selected = "All races"),
              selectInput(inputId = "color2", "Select a color",
                          choices = c("Blue"),
                          selected = "All races"),
            )
          ),
                
          # Time Series Plot
          column(6, plotlyOutput("timePlot"))
        ),
              
        # Row for risk bar plot and age bar plot
        fluidRow(
          column(6, plotlyOutput("riskPlot")),
          column(6, plotlyOutput("agePlot"))
        ),
              
        # Row for Location Bar Plot
        fluidRow(12, plotlyOutput("locationPlot"))
      )
    ),
    br(),
    br(),
    div(id = "versionNumber",
      "v1.0.0" 
    )
  )
)

server <- function(input, output) {
  
  # Load data
  
  # Show Modal
  showModal(modalDialog(
    title = "Welcome!",
    paste0("Welcome to the Vision & Eye Health Dashboard v1.0.0 - Scott ", emoji("smile")),
    easyClose = TRUE,
    footer = NULL
  ))
  
  
  # Creating Main Metric DF to Calculate Overall Mean Prevalence
  avgDF <- eyeHealth %>% 
    filter(Age == "All ages", Gender == "All genders", 
           RaceEthnicity == "All races", RiskFactor == "All participants",
           RiskFactorResponse == "All participants", LocationDesc == "National")
  
  avgPrevValue <- round(mean(avgDF$Data_Value), 2)
  
  # Creating Location Data Frame
  locationDF <- eyeHealth %>% 
    filter(Gender == "All genders", 
           RaceEthnicity == "All races", Age == "All ages", 
           Data_Value_Type == "Crude Prevalence",
           RiskFactorResponse == "All participants", 
           RiskFactor == "All participants") %>% 
    filter(LocationDesc != "National") %>% 
    group_by(LocationDesc) %>% 
    summarise(mean_prevalence = round(mean(Data_Value), 2)) %>% 
    arrange(desc(mean_prevalence)) %>% 
    head(10)
  
  state1 <- locationDF[1, 1]
  
  # Output for Main Metric
  output$avgValueOut <- renderValueBox({
    valueBox(
      paste0(avgPrevValue, "%"), 
      "Mean National Vision Disability Prevalence", 
      icon = icon("eye"), color = "blue"
    )
  })
  
  # Output for Main Metric
  output$avgValueOut2 <- renderValueBox({
    valueBox(
      state1, 
      "State / Territory with the Highest Mean Prevalence of Vision Loss", 
      icon = icon("plane"), color = "blue"
    )
  })
  
  # Creating Reactive Time Series Data Frames
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
  
  # Reactive Output for Time Series Plot
  output$timePlot <- renderPlotly({
    p <- ggplot(DF(), aes(x = YearStart, y = Data_Value)) +
      geom_line(color = "black") +
      geom_point(size = 4, color = "black") + 
      xlab("Year") + ylab("Prevalence (%)") +
      geom_line(data = DF2(), aes(x = YearStart, y = Data_Value), 
                color = "#00B4D8") +
      geom_point(data = DF2(), aes(x = YearStart, y = Data_Value), 
                 color = "#00B4D8", size = 4) +
      ggtitle("Prevalence of Severe Vision Disability Over Time")
    ggplotly(p)
  })
  
  # Creating Risk Factor Data Frame
  riskFactorDF <- eyeHealth %>% 
    filter(Age == "All ages", Gender == "All genders", 
           RaceEthnicity == "All races", Data_Value_Type == "Crude Prevalence",
           RiskFactorResponse == "Yes", LocationDesc == "National") %>% 
    group_by(RiskFactor) %>% 
    summarise(mean_prevalence = round(mean(Data_Value), 2))
  
  # Output for Risk Factor Bar Plot
  output$riskPlot <- renderPlotly({
    p2 <- ggplot(riskFactorDF, aes(x = reorder(RiskFactor, +mean_prevalence), 
                                   y = mean_prevalence, label = RiskFactor, label2 = mean_prevalence)) +
      geom_bar(stat = "identity", fill = "#0077B6", color = "black", 
               alpha = 0.9) + xlab("Risk Factor") +
      ylab("Mean Prevalence (%)") + ggtitle("Risk Factor Comparison")
    ggplotly(p2, tooltip = c("label", "label2"))
  })
  
  # Creating Age Data Frame
  ageDF <- eyeHealth %>% 
    filter(Gender == "All genders", 
           RaceEthnicity == "All races", Data_Value_Type == "Crude Prevalence",
           RiskFactorResponse == "All participants", 
           RiskFactor == "All participants",
           LocationDesc == "National") %>% 
    filter(Age != "All ages") %>% 
    group_by(Age) %>% 
    summarise(mean_prevalence = round(mean(Data_Value), 2))
  
  # Output for Age Bar Plot
  output$agePlot <- renderPlotly({
    p3 <- ggplot(ageDF, aes(x = reorder(Age, +mean_prevalence), 
                            y = mean_prevalence, label = Age, label2 = mean_prevalence)) +
      geom_bar(stat = "identity", fill = "#0077B6", color = "black", 
               alpha = 0.9) + xlab("Age Group") + 
      ylab("Mean Prevalence (%)") +
      ggtitle("Age Group Comparison")
    ggplotly(p3, tooltip = c("label", "label2"))
  })
  
  # Output for Location Bar Plot
  output$locationPlot <- renderPlotly({
    p4 <- ggplot(locationDF, aes(x = reorder(LocationDesc, -mean_prevalence), 
                                 y = mean_prevalence, label = LocationDesc, label2 = mean_prevalence)) +
      geom_bar(stat = "identity", fill = "#0077B6", 
               color = "black", alpha = 0.9) + xlab("State") + 
      ylab("Mean Prevalence (%)") +
      ggtitle("States / Territories With the Highest Mean Prevalence of Vision Disability")
    ggplotly(p4, tooltip = c("label", "label2"))
  })

}

shinyApp(ui, server)
