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
  
  index <- eventReactive(input$modelGo, {createDataPartition(y = finalData$Revenue , p = input$dataSplit, list = FALSE)})
  trdata <- eventReactive(input$modelGo, {data.frame(finalData[index(),])})
  tsdata <- eventReactive(input$modelGo, {data.frame(finalData[-index(),])})
  # splitting data into training/test on action
    
  lrTrain <- eventReactive(input$modelGo, {
    withProgress({
      train(
        as.formula(paste("Revenue ~ ", paste(input$linregVars, collapse = "+"))),
        data = trdata(),
        method = "glm",
        family = "binomial",
        preProcess = c("center","scale"),
        trControl = trainControl(method = "cv", number = 10)
    )}, message = "GLM: Training Model", detail = "this part should be quick")
  }) # training glm model on action (with a progress bar)
  
  lrTest <- eventReactive(input$modelGo, {
    withProgress({
      pred <- predict(lrTrain(), newData = tsdata())
      round(postResample(pred, obs = tsdata()$Revenue),4)
    }, message = "GLM: Testing Model", detail = "this part should be quick")
  })
  
    
  ctTrain <- eventReactive(input$modelGo, {
    withProgress({
    train(
      as.formula(paste("Revenue ~ ", paste(input$clTreeVars, collapse = "+"))),
      data = trdata(),
      method = "rpart",
      preProcess = c("center","scale"),
      trControl = trainControl(method = "cv", number = 10)
    )}, message = "Classification Tree: Training Model", detail = "this part should be quick")
  }) # training classification tree on action (with a progress bar)
  
  ctTest <- eventReactive(input$modelGo, {
    withProgress({
      pred <- predict(ctTrain(), newData = tsdata())
      round(postResample(pred, obs = tsdata()$Revenue),4)
    }, message = "Classification Tree: Testing Model", detail = "this part should be quick")
  })
    
  rfTrain <- eventReactive(input$modelGo, {
    withProgress({
    train(
      as.formula(paste("Revenue ~ ", paste(input$rForestVars, collapse = "+"))),
      data = trdata(),
      method = "rf",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
      tuneGrid = data.frame(mtry = seq(1,length(input$rForestVars),1))
    )}, message = "Random Forest: Training Model", detail = "this part might take a while")
  }) # training random forest on action (with a progress bar)

  rfTest <- eventReactive(input$modelGo, {
    withProgress({
      pred <- predict(rfTrain(), newData = tsdata())
      round(postResample(pred, obs = tsdata()$Revenue),4)
    }, message = "Random Forest: Testing Model", detail = "this part should be quick")
  })  
    
  output$lrStats <- renderPrint({
    lrTrain()
  }) # print the output of the training
  
  output$lrPred <- renderPrint({
    lrTest()
  })
  
  output$ctStats <- renderPrint({
    ctTrain()
  }) # print the output of the training
  
  output$ctPred <- renderPrint({
    ctTest()
  })
  
  output$rfStats <- renderPrint({
    rfTrain()
  }) # print the output of the training
  
  output$rfPred <- renderPrint({
    rfTest()
  })
    # TODO: execute model predict when user changes input and clicks button
    # TODO: render data table with user subsets
 
})
