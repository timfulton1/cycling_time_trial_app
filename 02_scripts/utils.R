# Load Packages
library(shiny)
library(readxl)
library(dplyr)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(scales)



load_and_process_data <- function(file_path) {
  
  data_raw <- read_excel("01_data/test_data.xlsx", col_names = c("time", "power", "rpm", "distance", "unused_2"))
  
  data_cleaned <- data_raw %>% 
    filter(!is.na(distance)) %>% 
    select(time, power, distance)
  
  # Return the processed data frame
  return(data_cleaned)
}