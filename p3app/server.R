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
library(corrplot)

# https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset

theT <- read_csv("../online_shoppers_intention.csv") # the data set as-is
theModelT <- dummy_cols(
  theT,
  select_columns = c("Month", "OS", "Region", "TrafficType", "VisitorType"),
  remove_first_dummy = FALSE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = TRUE,
  remove_selected_columns = TRUE
) # the data set with dummy cols for factor/categorical vars

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
  observeEvent(input$tabset, {
    # TODO: update sidebar widgets depending on what tab is open
  })
  observe({
    updateSelectInput(session, "xaxis", choices = if(input$graphType == 'scatter') {
      list("Administrative Pages Viewed"           = "Administrative", 
           "Administrative Duration"               = "Administrative_Duration",
           "Informational Pages Viewed"            = "Informational",
           "Informational Duration"                = "Informational_Duration",
           "Product-Related Pages Viewed"          = "ProductRelated",
           "Product-Related Duration"              = "ProductRelated_Duration",
           "Bounce Rates"                          = "BounceRates",
           "Exit Rates"                            = "ExitRates",
           "Page Values"                           = "PageValues",
           "Proximity to Special Day"              = "SpecialDay"
      )} else {
        list("Month"                                 = "Month",
             "OS"                                    = "OperatingSystems",
             "Browser"                               = "Browser",
             "Region"                                = "Region",
             "Traffic Type"                          = "TrafficType",
             "Visitor Type"                          = "VisitorType",
             "Weekend (T/F)"                         = "Weekend",
             "Revenue (T/F)"                         = "Revenue"
        )
       }
      )
  }) # this bit puts numeric vars in x axis choices for scatter plot, otherwise categorical
  
  output$expVis <- renderPlot({
    if(input$graphType == "bar" & input$split == "None") {
        
      ggplot(theT, aes(x = theT[[input$xaxis]])) +
        geom_bar() +
          labs(x = input$xaxis)
        
    } else if(input$graphType == "bar" & input$split != "None") {
        
      ggplot(theT, aes(x = as_factor(theT[[input$xaxis]]))) +
        geom_bar(aes(fill = theT[[input$split]])) +
          labs(x = input$xaxis, fill = input$split)
        
    } else if(input$graphType == "box") {
        
      theT$x <- factor(theT[[input$xaxis]])
        ggplot(theT, aes(fill = theT$x, y = theT[[input$yaxis]])) +
          geom_boxplot() +
            labs(x = input$xaxis, y = input$yaxis)
        
    } else if(input$graphType == "scatter" & input$split == "None") {
      
      ggplot(theT, aes(x = theT[[input$xaxis]], y = theT[[input$yaxis]])) +
        geom_point() +
          labs(x = input$xaxis, y = input$yaxis)
        
    } else if(input$graphType == "scatter" & input$split != "None") {
        
      ggplot(theT, aes(x = .data[[input$xaxis]], y = theT[[input$yaxis]], color = theT[[input$split]])) +
        geom_point() +
          labs(x = input$xaxis, y = input$yaxis, color = input$split)
        
    }
  }) # some really ugle if/else logic to get the right graph based on user selections
  
  
  observe({
    updateCheckboxGroupInput(session, "filterList", choices = unique(theT[[input$byVar]]), selected = unique(theT[[input$byVar]]))
  }) # change what options the filter box displays based on the by-variable. Select them all by default.

  output$expTab <- renderDataTable({
    df <- theT %>%
      filter(.data[[input$byVar]] %in% input$filterList) %>%
        group_by(.data[[input$byVar]]) %>%
          summarize(mean = round(mean(.data[[input$summVar]]), digits = 2), 
                    median = round(median(.data[[input$summVar]]), digits = 2),
                    iqr = round(IQR(.data[[input$summVar]]), digits = 2),
                    sd = round(sd(.data[[input$summVar]]), digits = 2)
                    )
  })
    # TODO: split data into training and test based on user ratio
    # TODO: define models & show fit stats & tests based on user input when button is clicked
    # TODO: execute model predict when user changes input and clicks button
    # TODO: render data table with user subsets
 
})
