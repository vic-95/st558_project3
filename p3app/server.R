#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(fastDummies)

# https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset

theT <- read_csv("../online_shoppers_intention.csv") # the data set as-is
theModelT <- dummy_cols(
  theT,
  select_columns = c("Month", "VisitorType"),
  remove_first_dummy = FALSE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = TRUE,
  remove_selected_columns = TRUE
) # the data set with dummy cols for factor vars

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
  observeEvent(input$tabset, {
    # TODO: update sidebar widgets depending on what tab is open
  })
    # TODO: graphical plot based on user input with dynamic to allow for stack vs grouped bars if bar
    # TODO: summary based on user input
    # TODO: split data into training and test based on user ratio
    # TODO: define models & show fit stats & tests based on user input when button is clicked
    # TODO: execute model predict when user changes input and clicks button
    # TODO: render data table with user subsets
 
})
