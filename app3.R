library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Assuming that these functions are defined in "finalproject.R"
source("finalproject.R")

# Assuming that these datasets are available
diabetes_df <- read.csv("diabetes.csv") 
wa_df <- read.csv("wa_df.csv")

# Specify the unique values for the race dropdown
race_choices <- c("Hispanic", "Black, non-Hispanic", "American Indian or Alaska Native", "White, non-Hispanic", "Asian or Pacific Islander")

ui <- fluidPage(
  # Summary
  h1("Summary"),
  p("From the background study, we obtain that individuals' daily sugar intake 
    values are significantly higher than the FDA’s recommendation (less than 10%). 
    This is a significant issue for people to draw attention to. Our study compares 
    people with different races, gender, and regions focused on the west coast on the 
    number of diagnosed diabetes cases. We found out that people in Washington 
    state have a higher diagnosed rate than people who live in California. 
    Hispanics have a higher rate than overall. Based on the analysis, we hypothesize 
    that the reason for these results could be culturally dietary pattern differences 
    caused by factors of races, genders, and regions."),
  p("Data sources:"),
  p("CDC's Division of Population Health (U.S. Chronic Disease Indicators):"),
  p("124 indicators designed for uniform collection and reporting of chronic disease data.
  Comprehensive coverage across states, territories, and large metropolitan areas.
  Focus on cancer and diabetes indicators crucial for public health practice."),
  p("FAO's Food Balance Sheet:"),
  p("Holistic view of the country's food supply, detailing sources and utilization for various food items.
  Insights into Americans' daily average calorie intake per nutrient group.
  Facilitating the analysis of dietary patterns to understand their correlation with diabetes rates."),
  
  # Diabetes Analysis
  h1("Diabetes Analysis"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabselected == 'VS WA'",
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
          choices = unique(gender_df$LocationDesc)
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
      tabPanel("VS WA", 
                 h3("Selected State Compared to Washington"), 
                 plotlyOutput(outputId = "plot1"),
                 conditionalPanel(
                   condition = "input.tabselected == 'VS WA'",
                   selectInput("state", "Select a state:", choices = LocationDesc)
                 )
        ),
        tabPanel("Gender", 
                 h3("Comparison of Selected States Diabetic Outcomes in terms of Gender"), 
                 plotlyOutput(outputId = "plot2"),
                 conditionalPanel(
                   condition = "input.tabselected == 'Gender'",
                   selectInput("state", "Select a state:", choices = LocationDesc)
                 )
        ),
      tabPanel("Race", 
               h3("Comparison of Selected States Diabetic Outcomes in terms of Race"), 
               plotlyOutput(outputId = "plot3"),
               conditionalPanel(
                 condition = "input.tabselected == 'Race'",
                 HTML("<!-- The bar graph shows the number of people with diabetes who are from that race each year. 
          The x-axis represents the year, and the y-axis represents the number of people who were hospitalized 
          with diabetes as a listed cause. The graph is interactive, allowing the user to select the race from 
          a dropdown menu. The graph will then be updated to show the number of people with diabetes from the 
          selected race for each year. This allows the user to easily compare the number of people with diabetes 
          across different races and years. -->")
               )
      )
  )
)

server <- function(input, output) {
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
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"),  # Display values in increments of a thousand
                         breaks = scales::pretty_breaks(5)) +  # Adjust the number of breaks as needed
      labs(x = "Year",
           y = "Total Population with Diabetes (in thousands)",  # Indicate the units in the axis label
           fill = "Legend") +
      facet_wrap(~LocationDesc)
    ggplotly(a)
  })
  
  output$plot2 <- renderPlotly({
      gender_df <- filter(gender_df, LocationDesc == input$state_name)
      b <- ggplot(gender_df, aes(YearStart, DataValue, fill = Stratification1)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~ LocationDesc, ncol = 2) +
        labs(title = "Number of People with Diabetes by Gender and State",
             x = "Year", y = "Number of People with Diabetes",
             fill = "Gender")
      
      return (b)
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
           x = "Year"
           y = "Number of People With Diabetes (Thousands)") 
    
    ggplotly(c)
  })

shinyApp(ui = ui, server = server)

