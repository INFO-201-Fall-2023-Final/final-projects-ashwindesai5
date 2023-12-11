library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)

source("finalproject.R")
disease_df <- read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv") 
wa_df <- read.csv("wa_df.csv")


introduction_view <- fluidPage(
  h1("About this project"),
  p("Welcome to our detailed analysis, where we examine the complex relationship 
    between chronic diseases, focusing primarily on diabetes, and dietary habits. 
    Utilizing data from two reputable sources, CDC's Division of Population Health and FAO's 
    Food Balance Sheet, our aim is to provide insights into public health 
    practices and contribute to efforts aimed at reducing diabetes rates.")
)

analysis_view<-fluidPage(
  titlePanel("Diabetes Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "state_name",
        label = "Select a state",
        choices = diseases_df$LocationAbbr #filtered dataframe of choosen state
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("tab1", h3("plot1 title"),plotOutput(outputId = "plot1")),
        tabPanel("tab2", h3("plot2 title"),plotOutput(outputId = "plot2")),
        tabPanel("tab3", h3("plot3 title"),plotOutput(outputId = "plot3"))
      )
    )
  ),
  p("paragraph outside")
)

summary_view <- fluidPage(
  h1("Summary"),
  p("From the backgroud study, we obtian that individuals' daily sugar intake 
    values are siginificantly higher than the FDA’s recommendation(less than 10%). 
    This is a significant issue for people to draw attentions on. Our study compares 
    people with different races, gender and regions focused in westcost on the 
    number of diagnosed diabetes cases. We found out that people in Washington 
    state have higher diagnosed rate than people who lives in California. 
    Hispanics have higher rate than overall. Based on the anaylsis, we hypothises 
    that the reason for these result could be culturally dietary pattern difference 
    caused by factors of races, genders and regions."),
  p("Data sources:"),
  p("CDC's Division of Population Health (U.S. Chronic Disease Indicators):"),
  p("124 indicators designed for uniform collection and reporting of chronic disease data.
  Comprehensive coverage across states, territories, and large metropolitan areas.
  Focus on cancer and diabetes indicators crucial for public health practice."),
  p("FAO's Food Balance Sheet:"),
  p("Holistic view of the country's food supply, detailing sources and utilization for various food items.
  Insights into Americans' daily average calorie intake per nutrient group.
  Facilitating the analysis of dietary patterns to understand their correlation with diabetes rates.")
)

ui<-navbarPage(
  "Final Project",
  tabPanel("Introduction", introduction_view),
  tabPanel("Analysis", analysis_view),
  tabPanel("Summary", summary_view)
)

server<-function(input, output){
  
  make_table <- function(state){
    #filtered table goes here
  }
 
  output$table <-renderTable({
    return(make_table(input$state_name))
  })

  output$barchart <- renderPlot({
    #plot goes here
    #return ()
  })
}

shinyApp(ui=ui,server=server)
