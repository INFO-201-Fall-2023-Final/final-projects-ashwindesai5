#INFO 201 Final Project

library(dplyr)
library(stringr)
library(ggplot2)

# Project Code -----------------------------------------------------------------

diabetes_df <- read.csv("diabetes.csv")


diabetes_df <- select(diabetes_df, YearStart, LocationAbbr, LocationDesc, 
                      Topic, Question, DataValueType, DataValue, 
                      StratificationCategory1, Stratification1)
wa_df <- filter(diabetes_df, LocationAbbr == "WA" & YearStart > 2013)


food_df <- read.csv("FAOSTAT_data_en_11-27-2023.csv")

food_df <- select(food_df, Area, Element, Year, Unit, Grand.Total, Sugar...Sweeteners)
food_df$sugar_pct <- food_df$Sugar...Sweeteners/food_df$Grand.Total * 100

df <- left_join(wa_df, food_df, by = c("YearStart" = "Year"))
#This Code is for testing selecte_state and to look at the df for errors
selected_state <-  filter(diabetes_df, LocationDesc == "Alabama", Stratification1 == "Overall", 
                          Question == "Hospitalization with diabetes as a listed diagnosis")
selected_state <- filter(selected_state, !is.na(DataValue))

race_df <- wa_df[wa_df$DataValueType == "Number" & !is.na(wa_df$YearStart), ]
race_df$DataValue <- as.numeric(as.character(race_df$DataValue))
race_df <- race_df[race_df$StratificationCategory1 == "Race/Ethnicity" & race_df$Question == "Hospitalization with diabetes as a listed diagnosis", ]

gender_df <- filter(diabetes_df, StratificationCategory1 == "Gender" & YearStart > 2013)
gender_df <- gender_df[gender_df$Question == "Hospitalization with diabetes as a listed diagnosis", ]
gender_df$DataValue <- as.numeric(as.character(gender_df$DataValue))
gender_df <- filter(gender_df, !is.na(DataValue))
gender_df <- select(gender_df, YearStart, LocationDesc, 
                      Question, DataValueType, DataValue, 
                      StratificationCategory1, Stratification1)

