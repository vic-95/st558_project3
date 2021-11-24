#
library(tidyverse)
library(shiny)
library(fastDummies)
#library(mathJax)

theT <- read_csv("../online_shoppers_intention.csv") # the data set as-is
theModelT <- dummy_cols(
  theT,
  select_columns = c("Month", "OperatingSystems", "Region", "TrafficType", "VisitorType"),
  remove_first_dummy = FALSE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = TRUE,
  remove_selected_columns = TRUE
) # the data set with dummy cols for factor/categorical vars

modNames <- names(theModelT)

predVec <- modNames[ ! modNames %in% c("Revenue")]


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    navbarPage(
      "Ecommerce Purchase Intent", # Application title
      tabPanel(
        "About",
        sidebarLayout(
          sidebarPanel(
            
          ),
          mainPanel(
            h2(" About the App"),
            HTML("<div>
                    <img src = 'noun_online shopping_1605024.svg' width = '100px' height = 'auto' align = 'left' alt = 'online shopping icon'>
                    <p> This app provides an interface for the user to explore, analyze, and model ecommerce session behavior and its impact on purchase.</p>
                    <p>The data set from the UCI Machine Learning repository contains information about the purchase intent behavior of website visitors. It details all of a users' visits in a 1 yr period: length, duration, types of content viewed, and whether they ultimately made an ecommerce purchase.</p>
                  </div>"
            ),
            br(),
            p("The user can use the following tabs to interact with and gain isight from the data:"),
            HTML("<ul>
                    <li>On the <b>Data Exploration</b> page, the user is able to interact with the data through visualizations and tables.</li> 
                    <li>On the <b>Modeling</b> page, the user is able to customize & train three types of predictive model and use them to predict purchase outcome for new user-entered data.</li> 
                    <li>On the <b>Data</b> page, the user can view, filter, and interact with the source data.</li>
                  </ul>"
            ),
            br(),
            HTML("<p><em>Data Credit: <a href = 'https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset'> Online shoppers intention from the UCI Machine Learning Repository</a></em></p>"),
            HTML("<p><em>Image credit: online shopping by <a href = 'https://thenounproject.com/grega.cresnar'>Gregor Cresnar</a> from the Noun Project</em></p>")
          )
        )
      ),
      tabPanel(
        "Data Exploration",
        sidebarLayout(
          sidebarPanel(
            h3("Visual Controls"),
            radioButtons(
              "graphType", 
              strong("Select a graph type"), 
              choices = list("Bar Chart" = "bar", "Box Plot" = "box", "Scatter Plot" = "scatter")
            ),
            selectInput(
              "xaxis", 
              strong("Select x axis variable"),
              choices = NULL
            ),
            conditionalPanel(
              condition = "input.graphType == 'box' | input.graphType == 'scatter'",
              selectInput(
                "yaxis", 
                strong("Select y axis variable"),
                choices = list("Administrative Pages Viewed"           = "Administrative", 
                               "Administrative Duration"               = "Administrative_Duration",
                               "Informational Pages Viewed"            = "Informational",
                               "Informational Duration"                = "Informational_Duration",
                               "Product-Related Pages Viewed"          = "ProductRelated",
                               "Product-Related Duration"              = "ProductRelated_Duration",
                               "Bounce Rates"                          = "BounceRates",
                               "Exit Rates"                            = "ExitRates",
                               "Page Values"                           = "PageValues",
                               "Proximity to Special Day"              = "SpecialDay"
                )
              )
            ),
            conditionalPanel(
              condition = "input.graphType == 'bar' | input.graphType == 'scatter'",
              selectInput(
                "split", 
                strong("Select a split varible"),
                choices = list("None"                                  = "None",
                               "Weekend (T/F)"                         = "Weekend",
                               "Revenue (T/F)"                         = "Revenue"
                )
              )
              
            ),
            h3("Summary Controls"),
            selectInput(
              "summVar",
              strong("Select the variable to be summarized"),
              choices = list("Administrative Pages Viewed"           = "Administrative", 
                             "Administrative Duration"               = "Administrative_Duration",
                             "Informational Pages Viewed"            = "Informational",
                             "Informational Duration"                = "Informational_Duration",
                             "Product-Related Pages Viewed"          = "ProductRelated",
                             "Product-Related Duration"              = "ProductRelated_Duration",
                             "Bounce Rates"                          = "BounceRates",
                             "Exit Rates"                            = "ExitRates",
                             "Page Values"                           = "PageValues",
                             "Proximity to Special Day"              = "SpecialDay"
              )
            ),
            selectInput(
              "byVar",
              strong("Select the variable for summary grouping"),
              choices = list("Month"                                 = "Month",
                             "OS"                                    = "OperatingSystems",
                             "Browser"                               = "Browser",
                             "Region"                                = "Region",
                             "Traffic Type"                          = "TrafficType",
                             "Visitor Type"                          = "VisitorType",
                             "Weekend (T/F)"                         = "Weekend",
                             "Revenue (T/F)"                         = "Revenue"
              )
            ),
            checkboxGroupInput(
              "filterList",
              strong("Select summary filters"),
              choices = NULL
            )
          ),
          mainPanel(
            plotOutput("expVis"),
            dataTableOutput("expTab")
          )
        )
      ),
      navbarMenu(
        "Modeling",
        tabPanel(
          "Modeling Info",
          sidebarLayout(
            sidebarPanel(
            
            ),
            mainPanel(
                
            )
          )
        ),
        tabPanel(
          "Model Fitting",
          sidebarLayout(
            sidebarPanel(
              h3("Fitting the Models"),
              sliderInput(
                "dataSplit", label = strong("Select the proportion of data to use for model training"),
                min = 0, max = 1, value = 0.8
              ),
              checkboxGroupInput(
                "linregVars", label = strong("Variables for the linear regression model"),
                choices = predVec
              ),
              checkboxGroupInput(
                "regTreeVars", label = strong("Variables for the linear regression model"),
                choices = predVec
              ),
              checkboxGroupInput(
                "rForestVars", label = strong("Variables for the linear regression model"),
                choices = predVec
              ),
              actionButton("modelGo", "Train Models")
            ),
            mainPanel(
                
            )
          )
        ),
        tabPanel(
          "Prediction",
          sidebarLayout(
            sidebarPanel(
                
            ),
            mainPanel(
                
            )
          )
        )
      ),
      tabPanel(
        "Data",
        sidebarLayout(
          sidebarPanel(
            
          ),
          mainPanel(
          
          )
        )
      )
    )
  )
)

