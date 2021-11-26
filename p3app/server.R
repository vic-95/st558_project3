#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(caret)
library(tree)
library(randomForest)

source("setup.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
  observe({
    updateSelectInput(session, "xaxis", choices = if(input$graphType == 'scatter') {numVars} 
      else {catVars}
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
  }) # some really ugly if/else logic to get the right graph based on user selections
  
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
  }) # data table that allows for filtering values of categorical by-vars
  
    # TODO: split data into training and test based on user ratio
  
  
  
  index <- eventReactive(input$modelGo, {createDataPartition(y = theT$Revenue , p = input$dataSplit, list = FALSE)})
  trdata <- eventReactive(input$modelGo, {data.frame(theT[index(),])})
  tsdata <- eventReactive(input$modelGo, {data.frame(theT[-index(),])})
    
  lrTrain <- eventReactive(input$modelGo, {
    withProgress({
    train(
      as.formula(paste("Revenue ~ ", paste(input$linregVars, collapse = "+"))),
      data = trdata(),
      method = "glm",
      family = "binomial",
      preProcess = c("center","scale"),
      trControl = trainControl(method = "cv", number = 10)
    )}, message = "training glm")
  })
    
  ctTrain <- eventReactive(input$modelGo, {
    withProgress({
    train(
      as.formula(paste("Revenue ~ ", paste(input$clTreeVars, collapse = "+"))),
      data = trdata(),
      method = "rpart",
      preProcess = c("center","scale"),
      trControl = trainControl(method = "cv", number = 10)
    )}, message = "training tree")
  })
    
  rfTrain <- eventReactive(input$modelGo, {
    withProgress({
    train(
      as.formula(paste("Revenue ~ ", paste(input$rForestVars, collapse = "+"))),
      data = trdata(),
      method = "rf",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
      tuneGrid = data.frame(mtry = seq(round(length(input$rForestVars)/2),length(input$rForestVars),1))
    )}, message = "training random forest")
  })
  
  output$lrStats <- renderPrint({
    lrTrain()
  })
  
  output$ctStats <- renderPrint({
    ctTrain()
  })
  
  output$rfStats <- renderPrint({
    rfTrain()
  })

    
#output$fitStats <- renderDataTable({
#  data.frame(Models = c("Linear Regression","Classification Tree","Random Forest"),
#             RMSE = round(c(lrTest[1], ctTest[1],rfTest[1]),1))
#})
    

  
    # TODO: execute model predict when user changes input and clicks button
    # TODO: render data table with user subsets
 
})
