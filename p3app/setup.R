## working with data here before putting into shiny
library(tidyverse)
library(magrittr)
library(fastDummies)
#library(mathJax)

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



theModelT <- dummy_cols(
  theT,
  select_columns = catVars[ ! catVars %in% c("Revenue")],
  remove_first_dummy = FALSE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = TRUE,
  remove_selected_columns = TRUE
) # the data set with dummy cols for factor/categorical vars

modNames <- names(theModelT)

predVec <- modNames[ ! modNames %in% c("Revenue")]

