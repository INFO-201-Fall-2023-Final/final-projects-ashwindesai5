library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Assuming that these functions are defined in "finalproject.R"
source("finalproject.R")

diabetes_df <- read.csv("diabetes.csv") 
wa_df <- read.csv("wa_df.csv")

race_choices <- c("Hispanic", "Black, non-Hispanic", "American Indian or Alaska Native", "White, non-Hispanic", "Asian or Pacific Islander")

ui <- fluidPage(
  # Summary
  h1("Diabetes Analysis"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        #condition = "input.tabselected == 'VS WA'",
        selectInput(
          inputId = "state_name",
          label = "Select a state",
          choices = diabetes_df$LocationDesc 
        )
      ),
      conditionalPanel (
        condition = "input.tabselected == 'Gender'",
        selectInput(
          inputId = "state_name", 
          label = "Select a state:",
          choices = unique(diabetes_df$LocationDesc)
        )
      ),
      selectInput(
        inputId = "race",
        label = "Select a race",
        choices = race_choices
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("VS WA", h3("Selected State Compared to Washington"),plotlyOutput(outputId = "plot1")),
        tabPanel("Gender", 
                 h3("Comparison of Selected States Diabetic Outcomes in terms of Gender"), 
                 plotlyOutput(outputId = "plot2")
        ),
        tabPanel("Race", 
                 h3("Comparison of Selected States Diabetic Outcomes in terms of Race"), 
                 plotlyOutput(outputId = "plot3")
        )
      )
    )
  )
)

server <- function(input, output){
  output$plot1 <- renderPlotly({
    selected_state <- filter(diabetes_df, LocationDesc == input$state_name, Stratification1 == "Overall", 
                             Question == "Hospitalization with diabetes as a listed diagnosis")
    selected_state <- filter(selected_state, !is.na(DataValue))
    cleaned_wa <- filter(wa_df, Stratification1 == "Overall", 
                         Question == "Hospitalization with diabetes as a listed diagnosis")
    cleaned_wa <- filter(cleaned_wa, !is.na(DataValue))
    
    merged <- full_join(selected_state, cleaned_wa)
    a <- ggplot(merged, aes(YearStart, DataValue, fill = LocationDesc)) +
      geom_col() +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"),
                         breaks = scales::pretty_breaks(5)) +
      labs(x = "Year",
           y = "Total Population with Diabetes (in thousands)",
           fill = "Legend") +
      facet_wrap(~LocationDesc)
    ggplotly(a)
  })
  
  output$plot2 <- renderPlotly({
      filtered_data <- filter(diabetes_df, LocationDesc == input$state_name, StratificationCategory1 == "Gender")
      b <- ggplot(filtered_data, aes(x = YearStart, y = DataValue, fill = Stratification1)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Number of People with Diabetes by Gender in", input$state_name),
             x = "Year", y = "Number of People with Diabetes",
             fill = "Gender") +
        theme_minimal() +
        theme(legend.position = "top")
      
      ggplotly(b)
    })
  
  output$plot3 <- renderPlotly({
    filtered_data <- reactive({
      race_df <- wa_df[wa_df$DataValueType == "Number" & !is.na(wa_df$YearStart), ]
      race_df$DataValue <- as.numeric(as.character(race_df$DataValue))
      race_df <- race_df[race_df$DataValueType == "Number" & race_df$StratificationCategory1 == "Race/Ethnicity" & race_df$Stratification1 == input$race, ]
      race_df
    })
    
    c <- ggplot(filtered_data(), aes(x = YearStart, y = DataValue)) +
      geom_col() +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3),
                         breaks = scales::pretty_breaks(5)) +
      labs(x = "Year", y = "Number of People With Diabetes (Thousands)")
    ggplotly(c)
  })
}

shinyApp(ui = ui, server = server)

