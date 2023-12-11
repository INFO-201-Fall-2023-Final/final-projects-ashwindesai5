#INFO 201 Final Project

library(dplyr)
library(stringr)
library(ggplot2)

# Project Code -----------------------------------------------------------------

disease_df <- read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv")

diabetes_df <-filter(disease_df, Topic == "Diabetes")

write.csv(diabetes_df, file = "U.S._Diabetes_.csv")

diabetes_df <- select(diabetes_df, YearStart, LocationAbbr, LocationDesc, 
                      Topic, Question, DataValueType, DataValue, 
                      StratificationCategory1, Stratification1)
wa_df <- filter(diabetes_df, LocationAbbr == "WA" & YearStart > 2013)


food_df <- read.csv("FAOSTAT_data_en_11-27-2023.csv")

food_df <- select(food_df, Area, Element, Year, Unit, Grand.Total, Sugar...Sweeteners)
food_df$sugar_pct <- food_df$Sugar...Sweeteners/food_df$Grand.Total * 100

df <- left_join(wa_df, food_df, by = c("YearStart" = "Year"))
filtered_data <- wa_df[wa_df$DataValueType == "Number" & !is.na(wa_df$YearStart), ]
filtered_data$DataValue <- as.numeric(as.character(filtered_data$DataValue))
g <- ggplot(filtered_data, aes(x = YearStart, y = DataValue, group = 1)) +
  geom_col(aes(fill = YearStart )) +
  labs(title = "Number of People with Diabetes (2014-2017)",
       x = "Year",
       y = "Number of People") +
  theme_minimal()


ca_df <- filter(diabetes_df, LocationAbbr == "CA" & YearStart > 2013)

wa_ca_df <- full_join(wa_df, ca_df)

filtered_data <- wa_ca_df[wa_ca_df$DataValueType == "Number" & !is.na(wa_ca_df$YearStart), ]
filtered_data$DataValue <- as.numeric(as.character(filtered_data$DataValue))
e <- ggplot(filtered_data, aes(x = YearStart, y = DataValue, group = LocationDesc )) +
  geom_col(aes(fill = LocationDesc )) +
  facet_wrap(~LocationDesc) +
  labs(title = "Number of People with Diabetes (2014-2017)",
       x = "Year",
       y = "Number of People") +
  theme_minimal()




plot_diabetes <- function(race) {
  # Filter the data
  filtered_data <- wa_df[wa_df$DataValueType == "number" & wa_df$StratificationCategory1 == "Race/Ethnicity" & wa_df$Stratification1 == race, ]
  
  
  ggplot(filtered_data, aes(x = YearStart, y = DataValue)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Number of people with diabetes from the", race, "race in each year"),
         x = "Year",
         y = "Number of people with diabetes") +
    coord_flip()
}

#race_df <- filter(wa_df, YearStart, DataValueType == "Number", DataValue, StratificationCategory1 == "	
#Race/Ethnicity", Stratification1)

race_df <- filtered_data[filtered_data$DataValueType == "Number" & filtered_data$StratificationCategory1 == "Race/Ethnicity" & filtered_data$Stratification1 == "Hispanic",]
plot(ggplot(race_df, aes(x = YearStart, y = DataValue)) +
       geom_col())
