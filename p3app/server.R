
source("setup.R") # setup variables & library imports and all that jazz

shinyServer(function(input, output, session) {
  
  ######################### Data Exploration tab #########################
  
  observe({
    updateSelectInput(session, "xaxis", choices = if(input$graphType == 'scatter') {numVars} 
      else {catVars}
      )
  }) # this bit puts numeric vars in x axis choices for scatter plot, otherwise categorical
  
  output$visTitle <- renderUI({
    x <- paste0(str_to_title(input$graphType), " Plot of ", input$xaxis)
    y <- if(input$graphType == "bar"){""} else(paste0(" and ", input$yaxis))
    by <- if(input$split == "None" | input$graphType == "box"){""} else{paste0(" by ", input$split)}
    h4(paste0(x, y, by))
  }) # Some dynamic UI for the title of the data exploration visual
  
  output$expVis <- renderPlot({
    req(input$graphType, input$split, input$xaxis, input$yaxis)
    if(input$graphType == "bar" & input$split == "None") {
        
      ggplot(theT, aes_string(x = input$xaxis)) +
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
            labs(fill = input$xaxis, y = input$yaxis)
        
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
  
  output$summTitle <- renderUI({
    title <- paste0("Summary of ", input$summVar, " by ", input$byVar)
    h4(title)
  }) # Some dynamic UI for the title of the data exploration visual
  
  output$expTab <- renderDataTable({
    if(input$byVar == 'None'){
      df <- theT %>%
        summarize(mean = round(mean(.data[[input$summVar]]), digits = 2), 
                  median = round(median(.data[[input$summVar]]), digits = 2),
                  iqr = round(IQR(.data[[input$summVar]]), digits = 2),
                  sd = round(sd(.data[[input$summVar]]), digits = 2)
        )
    } else {
      df <- theT %>%
        group_by(.data[[input$byVar]]) %>%
        summarize(mean = round(mean(.data[[input$summVar]]), digits = 2), 
                  median = round(median(.data[[input$summVar]]), digits = 2),
                  iqr = round(IQR(.data[[input$summVar]]), digits = 2),
                  sd = round(sd(.data[[input$summVar]]), digits = 2)
        )
    }
  }, filter = "top") # data table that allows for filtering values of categorical by-vars
  
  ######################### Modeling.Model Training tab #########################
  
  indexLR <- eventReactive(input$modelGo, {createDataPartition(y = finalData$Revenue , p = input$lrDataSplit, list = FALSE)})
  trdataLR <- eventReactive(input$modelGo, {data.frame(finalData[indexLR(),])})
  tsdataLR <- eventReactive(input$modelGo, {data.frame(finalData[-indexLR(),])})
  # splitting data into training/test on action for the glm
    
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
      pred <- predict(lrTrain(), newdata = tsdataLR())
      round(postResample(pred, obs = tsdataLR()$Revenue),4)
    }, message = "GLM: Testing", detail = "this part should be quick")
  }) # testing glm model on action (with a progress bar)
  
  output$lrResult <- renderDataTable({
    lrTrain()$results %>%
      mutate_at(c("Accuracy", "Kappa", "AccuracySD", "KappaSD"), round, digits = 4)
  }) # the results of the glm model on the training data
  
  output$lrSumm <- renderPrint({
    summary(lrTrain())
  }) # the summary of the glm fit on the training data
  
  indexCT <- eventReactive(input$modelGo, {createDataPartition(y = finalData$Revenue , p = input$ctDataSplit, list = FALSE)})
  trdataCT <- eventReactive(input$modelGo, {data.frame(finalData[indexCT(),])})
  tsdataCT <- eventReactive(input$modelGo, {data.frame(finalData[-indexCT(),])})
  # splitting data into training/test on action for the classification tree
    
  ctTrain <- eventReactive(input$modelGo, {
    withProgress({
    train(
      as.formula(paste("Revenue ~ ", paste(input$clTreeVars, collapse = "+"))),
      data = trdataCT(),
      method = "rpart",
      trControl = trainControl(method = "cv", number = 10, splitRule = "gini", classProbs = TRUE)
    )}, message = "Classification Tree: Training", detail = "this part should be quick")
  }) # training classification tree on action (with a progress bar)
  
  ctTest <- eventReactive(input$modelGo, {
    withProgress({
      pred <- predict(ctTrain(), newdata = tsdataCT())
      round(postResample(pred, obs = tsdataCT()$Revenue),4)
    }, message = "Classification Tree: Testing", detail = "this part should be quick")
  }) # testing classification tree on action (with a progress bar)
  
  output$ctResult <- renderDataTable({
    ctTrain()$results %>%
      slice_max(Accuracy, n = 1) %>%
        mutate_all(round, digits = 4)
  }) # results of the classification tree model on the training data
  
  output$ctPlot <- renderPlot({
    rpart.plot(ctTrain()$finalModel, uniform = TRUE)
  }) # a tree plot with text of the classification tree model
  
  indexRF <- eventReactive(input$modelGo, {createDataPartition(y = finalData$Revenue , p = input$rfDataSplit, list = FALSE)})
  trdataRF <- eventReactive(input$modelGo, {data.frame(finalData[indexRF(),])})
  tsdataRF <- eventReactive(input$modelGo, {data.frame(finalData[-indexRF(),])})
  # splitting data into training/test on action for the random forest model
    
  rfTrain <- eventReactive(input$modelGo, {
    withProgress({
      train(
        as.formula(paste("Revenue ~ ", paste(input$rForestVars, collapse = "+"))),
        data = trdataRF(),
        importance = "impurity",
        method = "ranger",
        preProcess = c("center", "scale"),
        trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, classProbs = TRUE),
        tuneGrid = expand.grid(mtry = seq(1,length(input$rForestVars),1),
                               splitrule = "gini",
                               min.node.size = 1
                              )
      )
    }, message = "Random Forest: Training", detail = "this part will take longer")
  }) # training random forest on action (with a progress bar). using mtry values 1 thru the number of variables entered for consideration

  rfTest <- eventReactive(input$modelGo, {
    withProgress({
      pred <- predict(rfTrain(), newdata = tsdataRF())
      round(postResample(pred, obs = tsdataRF()$Revenue),4)
    }, message = "Random Forest: Testing", detail = "this part should be quick")
  }) # testing random forest on the test data
    
  output$rfResult <- renderDataTable({
    rfTrain()$results %>%
      slice_max(Accuracy, n = 1) %>%
        mutate_at(c("Accuracy", "Kappa", "AccuracySD", "KappaSD"), round, digits = 4)
    
  }) # results of random forest on the training data
  
  output$rfVarImp <- renderPlot({
    imp <- varImp(rfTrain())
    plot(imp)
  }) # variable importance plot for random forest
  
  output$predComp <- renderDataTable({
    data.frame(
      Model = c("GLM","Classification Tree","Random Forest"),
      Accuracy = round(c(lrTest()[1], ctTest()[1], rfTest()[1]),4),
      Kappa = round(c(lrTest()[2], ctTest()[2], rfTest()[2]),4)
    )
  }) # combination of results on test data for all 3 models
  
  ######################### Modeling.Prediction tab #########################
  
  output$varOps <- renderUI({
    purrr::map(input$rForestVars, ~ {
      if(.x %in% numVars) {
        sliderInput(.x, paste0("Variable ", .x), min = floor(min(finalData[[.x]])), max = ceiling(max(finalData[[.x]])), value = 0)
      } else {
        checkboxInput(.x, paste0("Variable ", .x), value = FALSE)
      }
    })
  }) # use the variables selected for use in the random forest model to make widgets so the user can make a custom observation
  
  predValues <- eventReactive(input$predGo, {
    df <- data.frame(matrix(data = vector(), nrow = 0, ncol = length(input$rForestVars)))
    vals <- unlist(lapply(input$rForestVars, function(x){return(input[[x]])}))
    
    df <- rbind(df, vals)
    names(df) <- input$rForestVars
    predict(rfTrain(), newdata = df, type = "prob")
    
  }) # when the user hits the button, make a teeny tiny data frame with the observation and var names and run a predict on it with probabilities
  
  pred <- eventReactive(input$predGo, {
    df <- data.frame(matrix(data = vector(), nrow = 0, ncol = length(input$rForestVars)))
    vals <- unlist(lapply(input$rForestVars, function(x){return(input[[x]])}))
    
    df <- rbind(df, vals)
    names(df) <- input$rForestVars
    predict(rfTrain(), newdata = df)
  }) # when the user hits the button, make a teeny tiny data frame with the observation and var names and run a predict on it
  
  output$thePred <- renderUI({
    if(pred() == FALSE) {h3("This observation is not predicted to produce revenue")} else {h3("This observation is predicted to produce revenue")}
  })
  output$userPred <- renderDataTable({
    predValues() %>% mutate_all(round, digits = 4)
  })
  
  ######################### Data tab #########################
  
  observe({
    updateSelectInput(session, "dataCols", label = NULL, choices = NULL,
                      selected = if(input$filterCol){NULL} else{names(theT)})
  })
  
  uEdits <- reactive({
    theT %>%
      select(all_of(input$dataCols))
  })
  
  output$theT <- renderDataTable({uEdits()}, filter = "top")
  
  output$dlData <- downloadHandler(
    filename = function() {
      paste0("customer_intent_",Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(uEdits(), file, row.names = FALSE)
    }
  )
  
})
