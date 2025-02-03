# Load Packages
library(shiny)
library(readxl)
library(dplyr)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(scales)
library(gt)
library(openxlsx)
library(glue)



load_and_process_data <- function(file_path) {
  
  data_raw <- read_excel(file_path, col_names = c("time", "power", "rpm", "distance", "unused_2"))
  
  data_cleaned <- data_raw %>% 
    filter(!is.na(distance)) %>% 
    select(time, power, distance)
  
  # Return the processed data frame
  return(data_cleaned)
}