## working with data here before putting into shiny
library(shiny)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(fastDummies)
library(caret)
library(ranger)
library(tree)
library(DT)
library(stringr)

set.seed(108)

rawData <- read_csv("../online_shoppers_intention.csv") # the data set as-is

catVars <- c("Month", 
             "OperatingSystems", 
             "Browser",
             "Region",
             "TrafficType",
             "VisitorType",
             "Weekend",
             "Revenue"
             )

numVars <- c("Administrative", 
             "Administrative_Duration", 
             "Informational", 
             "Informational_Duration",
             "ProductRelated",
             "ProductRelated_Duration",
             "BounceRates",
             "ExitRates",
             "PageValues",
             "SpecialDay"
             )

theT <- rawData %<>%
  mutate_at(catVars, factor) # mass converting categorical variables to factors with infix function from magrittr

levels(theT$Revenue) <- c("revenue", "no_revenue")
make.names(levels(theT$Revenue))

theModelT <- dummy_cols(
  theT,
  select_columns = catVars[ ! catVars %in% c("Revenue")],
  remove_first_dummy = FALSE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = TRUE,
  remove_selected_columns = TRUE
)
# turning categorical variables into numeric dummy columns. 
# Caret can do this for me I noticed, but I have more control this way (was seeing issues I had to step in and fix)

nzv <- nearZeroVar(theModelT, saveMetrics = TRUE)
nzvNames <- nzv %>%
  filter(nzv == TRUE) %>%
    rownames()

sansNZV <- theModelT %>%
  select(-one_of(nzvNames)) 
# I had some issues with warnings being thrown about zero-variance columns during cross validation. 
# Choosing to deal with this by preemptively removing near-zero variance columns from the data.

c <- cor(sansNZV %>% select(-Revenue))

cTib <- as_tibble(c) %>%
  mutate(v1 = names(as_tibble(c))) %>%
  pivot_longer(cols = !v1, names_to = "v2", values_to = "corr") %>%
  filter(abs(corr) == 1 & v1 != v2)
# I was seeing rank deficiency issues in my models and figured out it was because of 100% correlated predictors.
# This allows me to root out and eliminate to superfluous predictor. I mean it was obviously the boolean value weekend, but...

finalData <- sansNZV %>% select(-Weekend_FALSE) # I could have probably done this programatically but...

modNames <- names(theT)
predVec <- modNames[ ! modNames %in% c("Revenue")]
# these are variable names to be used in the UI for user selection in the data exploration part

finNames <- names(finalData)
finPred <- finNames[! finNames %in% c("Revenue")]
# these are variable names to be used in the UI for user selection in the modeling part

#########################
# 
# index <- createDataPartition(y = finalData$Revenue , p = 0.8, list = FALSE)
# trdata <- data.frame(finalData[index,])
# tsdata <- data.frame(finalData[-index,])
# 
# vec <- c("Administrative", "Administrative_Duration")
# 
# ctTrain <-
#     train(
#       Revenue ~ Administrative + Administrative_Duration,
#       data = trdata,
#       method = "glm",
#       family = "binomial",
#       preProcess = c("center","scale"),
#       trControl = trainControl(method = "cv", number = 10)
#     )
# 
# # pred <- predict(ctTrain, newData = tsdata)
# # p2 <- round(postResample(pred, obs = tsdata$Revenue),4)
# 
# df <- data.frame(matrix(data = vector(), nrow = 0, ncol = length(vec)))
# vals <- unlist(lapply(vec, function(.x) {return(runif(1,0,1))}))
# 
# df <- rbind(df, vals)
# names(df) <- vec
# 
# df
#
# 
# x <- predict(ctTrain, newdata = df, type = "prob")
#
# 
# tr <- train(
#   Revenue ~ .,
#   data = trdata,
#   method = "ranger",
#   importance = "impurity",
#   preProcess = c("center", "scale"),
#   trControl = trainControl(method = "cv", number = 5),
#   tuneGrid = expand.grid(mtry = seq(1:10),
#                          splitrule = "gini",
#                          min.node.size = 1
#   )
# )