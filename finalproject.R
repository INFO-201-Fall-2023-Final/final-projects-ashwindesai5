#INFO 201 Final Project

library(dplyr)
library(stringr)
library(ggplot2)

# Project Code -----------------------------------------------------------------

disease_df <- read.csv("diabetes.csv")


diabetes_df <- select(diabetes_df, YearStart, LocationAbbr, LocationDesc, 
                      Topic, Question, DataValueType, DataValue, 
                      StratificationCategory1, Stratification1)
wa_df <- filter(diabetes_df, LocationAbbr == "WA" & YearStart > 2013)


food_df <- read.csv("FAOSTAT_data_en_11-27-2023.csv")

food_df <- select(food_df, Area, Element, Year, Unit, Grand.Total, Sugar...Sweeteners)
food_df$sugar_pct <- food_df$Sugar...Sweeteners/food_df$Grand.Total * 100

df <- left_join(wa_df, food_df, by = c("YearStart" = "Year"))
