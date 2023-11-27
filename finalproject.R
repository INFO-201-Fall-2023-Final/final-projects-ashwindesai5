#INFO 201 Final Project

library(dplyr)
library(stringr)
library(ggplot2)

# Project Code -----------------------------------------------------------------

disease_df <- read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv")

diabetes_df <-filter(us_disease_df, Topic == "Diabetes")