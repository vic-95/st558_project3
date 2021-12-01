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
library(rpart.plot)
library(randomForest)
library(effects)
library(tree)

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
  
  indexLR <- eventReactive(input$modelGo, {createDataPartition(y = finalData$Revenue , p = input$lrDataSplit, list = FALSE)})
  trdataLR <- eventReactive(input$modelGo, {data.frame(finalData[indexLR(),])})
  tsdataLR <- eventReactive(input$modelGo, {data.frame(finalData[-indexLR(),])})
  # splitting data into training/test on action
    
  lrTrain <- eventReactive(input$modelGo, {
    withProgress({
      train(
        as.formula(paste("Revenue ~ ", paste(input$linregVars, collapse = "+"))),
        data = trdataLR(),
        method = "glm",
        family = "binomial",
        preProcess = c("center","scale"),
        trControl = trainControl(method = "cv", number = 10)
    )}, message = "GLM: Training", detail = "this part should be quick")
  }) # training glm model on action (with a progress bar)
  
  lrTest <- eventReactive(input$modelGo, {
    withProgress({
      pred <- predict(lrTrain(), newData = tsdataLR())
      round(postResample(pred, obs = tsdataLR()$Revenue),4)
    }, message = "GLM: Testing", detail = "this part should be quick")
  })
  
  output$lrResult <- renderDataTable({
    lrTrain()$results %>%
      mutate_at(c("Accuracy", "Kappa", "AccuracySD", "KappaSD"), round, digits = 4)
  })
  
  output$lrSumm <- renderPrint({
    summary(lrTrain())
  }) # print the output of the training
  
  indexCT <- eventReactive(input$modelGo, {createDataPartition(y = finalData$Revenue , p = input$ctDataSplit, list = FALSE)})
  trdataCT <- eventReactive(input$modelGo, {data.frame(finalData[indexCT(),])})
  tsdataCT <- eventReactive(input$modelGo, {data.frame(finalData[-indexCT(),])})
  # splitting data into training/test on action
    
  ctTrain <- eventReactive(input$modelGo, {
    withProgress({
    train(
      as.formula(paste("Revenue ~ ", paste(input$clTreeVars, collapse = "+"))),
      data = trdataCT(),
      method = "rpart",
      preProcess = c("center","scale"),
      trControl = trainControl(method = "cv", number = 10)
    )}, message = "Classification Tree: Training", detail = "this part should be quick")
  }) # training classification tree on action (with a progress bar)
  
  ctTest <- eventReactive(input$modelGo, {
    withProgress({
      pred <- predict(ctTrain(), newData = tsdataCT())
      round(postResample(pred, obs = tsdataCT()$Revenue),4)
    }, message = "Classification Tree: Testing", detail = "this part should be quick")
  })
  
  output$ctResult <- renderDataTable({
    ctTrain()$results %>%
      slice_max(Accuracy, n = 1) %>%
        mutate_all(round, digits = 4)
  })
  
  output$ctPlot <- renderPlot({
    plot(tree(ctTrain())); text(tree(ctTrain()))
  }) # print the output of the training
  
  indexRF <- eventReactive(input$modelGo, {createDataPartition(y = finalData$Revenue , p = input$rfDataSplit, list = FALSE)})
  trdataRF <- eventReactive(input$modelGo, {data.frame(finalData[indexRF(),])})
  tsdataRF <- eventReactive(input$modelGo, {data.frame(finalData[-indexRF(),])})
  # splitting data into training/test on action
    
  rfTrain <- eventReactive(input$modelGo, {
    if(input$para) {
      withProgress({
        library(parallel) # just bringing this in for the ability to detect cores. Couldn't find that in doParallel
        library(doParallel)
        cores <- detectCores()
        cluster <- makePSOCKcluster(cores - 1)
        
        registerDoParallel(cluster)
        
        tr <- train(
          as.formula(paste("Revenue ~ ", paste(input$rForestVars, collapse = "+"))),
          data = trdataRF(),
          method = "rf",
          preProcess = c("center", "scale"),
          trControl = trainControl(method = "cv", number = 5),
          tuneGrid = data.frame(mtry = seq(1,length(input$rForestVars),1))
        )
        
        stopCluster(cluster)
        
        tr
      }, message = "Random Forest: Training", detail = "This part might take a while")
    } else {
      withProgress({
        train(
          as.formula(paste("Revenue ~ ", paste(input$rForestVars, collapse = "+"))),
          data = trdataRF(),
          method = "rf",
          preProcess = c("center", "scale"),
          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
          tuneGrid = data.frame(mtry = seq(1,length(input$rForestVars),1))
        )
      }, message = "Random Forest: Training", detail = "this part will take a long time")
    }
  }) # training random forest on action (with a progress bar)

  rfTest <- eventReactive(input$modelGo, {
    withProgress({
      pred <- predict(rfTrain(), newData = tsdataRF())
      round(postResample(pred, obs = tsdataRF()$Revenue),4)
    }, message = "Random Forest: Testing", detail = "this part should be quick")
  })  
    
  output$rfResult <- renderDataTable({
    rfTrain()$results %>%
      slice_max(Accuracy, n = 1) %>%
        mutate_at(c("Accuracy", "Kappa", "AccuracySD", "KappaSD"), round, digits = 4)
    
  }) # print the output of the training\
  
  output$rfVarImp <- renderPlot({
    imp <- varImp(rfTrain(), scale = FALSE)
    plot(imp)
  }) # print the output of the training
  
  output$predComp <- renderDataTable({
    data.frame(
      Model = c("GLM","Classification Tree","Random Forest"),
      Accuracy = round(c(lrTest()[1], ctTest()[1], rfTest()[1]),4),
      Kappa = round(c(lrTest()[2], ctTest()[2], rfTest()[2]),4)
    )
  })
    # TODO: execute model predict when user changes input and clicks button
  output$varOps <- renderUI({
    purrr::map(input$linregVars, ~ {
      if(.x %in% numVars) {
        sliderInput(.x, paste0("Variable ", .x), min = round(min(finalData[[.x]])), max = round(max(finalData[[.x]])), value = 0)
      } else {
        checkboxInput(.x, paste0("Variable ", .x), value = FALSE)
      }
    })
  })
  
  predValues <- eventReactive(input$predGo, {
    df <- data.frame(unlist(purrr::map(input$linregVars, ~ {get(paste0("input$", .x))})))
    predict(lrTrain(), newData = df, type = "prob")
  })
  
  output$userPred <- renderPrint({
    predValues()
  })
  
    # TODO: render data table with user subsets
  output$theT <- renderDataTable(theT)
})
