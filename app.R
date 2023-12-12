library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

source("finalproject.R")

diabetes_df <- read.csv("diabetes.csv") 
wa_df <- read.csv("wa_df.csv")

state_chioce <- unique(diabetes_df$LocationDesc)

race_choice <- c("Hispanic", "Black, non-Hispanic", "American Indian or Alaska Native", 
                  "White, non-Hispanic", "Asian or Pacific Islander")

#gender_choice <- unique(diabetes_df$Stratification1)
gender_choice <- c("Male", "Female")


introduction_view <- fluidPage(
  h1("About this project"),
  p("Welcome to our detailed analysis, where we examine the complex relationship 
    between chronic diseases, focusing primarily on diabetes, and dietary habits. 
    Utilizing data from two reputable sources, CDC's Division of Population Health and FAO's 
    Food Balance Sheet, our aim is to provide insights into public health 
    practices and contribute to efforts aimed at reducing diabetes rates.")
)

summary_view <- fluidPage(
  h1("Summary"),
  p("From the backgroud study, we obtian that individuals' daily sugar intake 
    values are siginificantly higher than the FDA’s recommendation(less than 10%). 
    This is a significant issue for people to draw attentions on. Our study compares 
    people with different races, gender and regions focused in westcost on the 
    number of diagnosed diabetes cases. People living in southeastern part of the 
    US tends to get higher rate of diabete diagnosis. Asisna and pacific lslander
    have higher rate than overall. Based on the anaylsis, we hypothises 
    that the reason for these result could be culturally dietary pattern difference 
    caused by factors of races, genders and regions."),
  br(),
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

analysis_view<-fluidPage(
  tabsetPanel(
    tabPanel("VS WA", 
             
             sidebarLayout(
               sidebarPanel(
                 h2("Control panel"),
                 selectInput(
                   inputId = "state_name",
                   label = "Select a state",
                   choices = state_chioce
                 )
               ),
               mainPanel(
                 h3("Selected State Compared to Washington"), 
                 plotlyOutput(outputId = "plot1")
               )
             ),
             p("The bar graph offers a comparative analysis between the number of 
               individuals with diabetes in Washington and those from a user-specified state. 
               The x-axis delineates the years under consideration, while the y-axis reflects 
               the respective counts of people hospitalized with diabetes as a listed cause. 
               It's important to note that there may be variations in data availability between 
               the states, and there is a possibility of missing data for certain years. 
               The graph facilitates a dynamic comparison, allowing users to select a 
               specific state for juxtaposition with Washington. This interactive feature 
               empowers users to discern patterns and disparities in diabetes prevalence 
               between the two chosen states across different years, contributing to a 
               comprehensive understanding of regional health trends.")

    ),
    tabPanel("Gender", 
             h3("Comparison of Selected States Diabetic Outcomes in terms of Gender"), 
             plotlyOutput(outputId = "plot2"),
             p("The bar graph provides a comparative analysis of the annual count 
               of individuals with diabetes, categorized by gender, specifically 
               focusing on the selected state as specified by the user from provies
               plot. The x-axis represents the chronological progression of years, 
               while the y-axis depicts the corresponding number of people hospitalized 
               with diabetes as a listed cause. It is essential to acknowledge the 
               possibility of missing data for certain years or genders within the chosen state. 
               In some instances, certain years may not have available data for a 
               particular gender. The graph's interactive functionality allows users 
               to tailor their exploration by selecting a specific state, enabling a 
               more nuanced examination of diabetes prevalence across different 
               genders and years within the chosen state.")
             
    ),
    tabPanel("Race", 
             sidebarLayout(
               sidebarPanel(
                 h2("Control panel"),
                 selectInput(
                   inputId = "race",
                   label = "Select a race",
                   choices = race_choice
                 )
               ),
               mainPanel(
                 h3("Comparison of Selected States Diabetic Outcomes in terms of Race"), 
                 plotlyOutput(outputId = "plot3")
               )
             ),
             p("The bar graph illustrates the yearly count of individuals with diabetes, 
               specifically those identifying as [input race from dropdown, who 
               were hospitalized with diabetes as a listed cause. The x-axis 
               corresponds to the year, while the y-axis denotes the number of 
               people affected. It is important to note that there might be instances 
               of missing data for certain states, and it is possible that some 
               states may not have any available data. The graph features interactivity, 
               enabling users to choose a specific race from a dropdown menu. 
               Upon selection, the graph dynamically updates to display the corresponding 
               data for people with diabetes from the chosen race for each year. 
               This interactive feature facilitates easy comparison of diabetes 
               prevalence across various races and years.")

    )
  )
  
)

ui<-navbarPage(
  "Final Project",
  tabPanel("Introduction", introduction_view),
  tabPanel("Analysis", analysis_view),
  tabPanel("Summary", summary_view)
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


shinyApp(ui=ui,server=server)
