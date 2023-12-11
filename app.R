library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)

source("finalproject.R")
ui <- fluidPage(
  titlePanel("Diabetes Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("race", "Select a race:", choices = unique(wa_df$Stratification1))
    ),
    mainPanel(
      plotOutput("diabetes_plot"),
      tags$div(
        style = "color: #666; font-size: 14px; margin-top: 20px;",
        HTML(paste("The bar graph shows the number of people with diabetes who are: <b>
                   </b> Who are from that race for each year. The x-axis represents the year, and the y-axis represents the number of people with diabetes. 
                   The data only shows the number of people from this race that mortality due to diabetes reported as any listed cause of death. 
                   The graph is interactive, allowing the user to select the race from a dropdown menu. 
                   The graph will then update to show the number of people with diabetes from the selected race for each year. 
                   This allows the user to easily compare the number of people with diabetes across different races and years."))
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  # Filter the data
  filtered_data <- reactive({
    race_df <- wa_df[wa_df$DataValueType == "Number" & !is.na(wa_df$YearStart), ]
    race_df$DataValue <- as.numeric(as.character(race_df$DataValue))
    race_df <- race_df[race_df$DataValueType == "Number" & race_df$StratificationCategory1 == "Race/Ethnicity" & race_df$Stratification1 == input$race, ]
    
  })
  
  
  
  # Create the plot
  output$diabetes_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = YearStart, y = DataValue)) +
      geom_col() +
      labs(title = paste("Number of people with diabetes who are:", input$race, "for each year"),
           x = "Year",
           y = "Number of people with diabetes") 
    
  })
}

shinyApp(ui=ui,server=server)