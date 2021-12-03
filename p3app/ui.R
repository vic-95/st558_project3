
library(shiny)

source("setup.R")

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(theme = shinytheme("simplex"),
    navbarPage(
      "Ecommerce Purchase Intent", # Application title
      tabPanel(
        "About",
        fluidRow(
          column(12,
            h2(" About the App"),
            HTML("<p style='text-align:center;'><img src = 'ecommerce_stock_photo_unsplash.jpg' width = '1000px' height = 'auto' align = 'center' alt = 'image of a person holding a credit card at a laptop'>"),     
            h4("This app provides an interface for the user to explore, analyze, and model ecommerce session behavior and its impact on purchase."),
            p("The data set from the UCI Machine Learning repository contains information about the purchase intent behavior of website visitors. It details all of a users' visits in a 1 yr period: length, duration, types of content viewed, and whether they ultimately made an ecommerce purchase."),
            br(),
            strong("The user can use the following tabs to interact with and gain insight from the data:"),
          )
        ),
        fluidRow(
          column(4,
            h3("Data Exploration"),
            HTML(
              "<div>
                 <ul>
                   <li>Select variables to use in bar, scatter, or box plots</li>
                   <li>Add a variable to graph legends for any plot type</li>
                   <li>View data summaries on any numeric variable</li>
                   <li>Subset summary data by any categorical variable</li>
                   <li>filter which values of the subsetting variable to show in the summary</li>
                 </ul>
               </div>"
            )
          ),
          column(4,
            h3("Modeling"),
            HTML(
              "<div>
                 <ul>
                   <li>Get information about the different classification methods used in this app</li>
                   <li>Design and train three different kinds of classification models</li>
                   <li>Control training data proportions and variable inclusion for all models</li>
                   <li>Enter a custom observation and predict its outcome with the random forest model</li>
                 </ul>
               </div>"
            )
          ),
          column(4,
            h3("Data"),
            HTML(
              "<div>
                 <ul>
                   <li>View the source data from within the app</li>
                   <li>Customize which columns to include or exclude from the view</li>
                   <li>Filter any column to narrow results</li>
                   <li>Export your custom view to a csv file</li>
                 </ul>
               </div>"
            )
          )  
        ),
        fluidRow(
          column(12,
            br(),
            HTML("<p><em>Data Credit: <a href = 'https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset'> Online shoppers intention from the UCI Machine Learning Repository</a></em></p>")
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
                choices = numVars
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
              choices = numVars
            ),
            selectInput(
              "byVar",
              strong("Select the variable for summary grouping"),
              choices = catVars
            ),
            checkboxGroupInput(
              "filterList",
              strong("Select summary filters"),
              choices = NULL
            )
          ),
          mainPanel(
            uiOutput("visTitle"),
            plotOutput("expVis"),
            br(),
            uiOutput("summTitle"),
            dataTableOutput("expTab")
          )
        )
      ),
      navbarMenu(
        "Modeling",
        tabPanel(
          "Modeling Info",
          fluidRow(
            column(12,
              h3("Modeling Info"),
              p("something about what classification models do as opposed to regression models")
            ),
            column(4,
              h4("Generalized Linear Model: Binomial"),
              p("grab the formula and the logit function")
            ),
            column(4,
              h4("Classification Tree Model"),
              p("TBD")
            ),
            column(4,
              h4("Random Forest Model"),
              ("TBD")
            )
          )
        ),
        tabPanel(
          "Model Fitting",
          fluidRow(
            column(4,
              titlePanel("Fitting the Models")
            ),
            column(8,
              conditionalPanel(condition = "! input.modelGo", p("Model fit statistics and summaries will display here once the models are trained. Click the 'Train Models' button when you're ready"))
            )
          ),
          fluidRow(
            column(4,
              h4("GLM Binomial"),
              sliderInput(
                "lrDataSplit", label = strong("Select the proportion of data to use for model training"),
                min = 0, max = 1, value = 0.8
              ),
              selectInput(
                "linregVars", label = strong("Variables for the binomial GLM"),
                choices = finPred,
                selected = finPred,
                multiple = TRUE
              )
            ),
            column(8,
                conditionalPanel(condition = "input.modelGo", h4("Results")),
                dataTableOutput("lrResult"),
                br(),
                conditionalPanel(condition = "input.modelGo", h4("Summary")),
                verbatimTextOutput("lrSumm"),
            )
          ),
          fluidRow(
            column(4,
              h4("Classification Tree"),
              sliderInput(
                "ctDataSplit", label = strong("Select the proportion of data to use for model training"),
                min = 0, max = 1, value = 0.8
              ),
              selectInput(
                "clTreeVars", label = strong("Variables for the classification tree"),
                choices = finPred,
                selected = finPred,
                multiple = TRUE
              )
            ),
            column(8,
              conditionalPanel(condition = "input.modelGo", h4("Results")),
              dataTableOutput("ctResult"),
              br(),
              conditionalPanel(condition = "input.modelGo", h4("Tree Visualization")),
              plotOutput("ctPlot")
            )
          ),
          fluidRow(
            column(4,
              h4("Random Forest"),
              sliderInput(
                "rfDataSplit", label = strong("Select the proportion of data to use for model training"),
                min = 0, max = 1, value = 0.8
              ),
              selectInput(
                "rForestVars", label = strong("Variables for the random forest model"),
                choices = finPred,
                selected = finPred,
                multiple = TRUE
              )
            ),
            column(8,
              conditionalPanel(condition = "input.modelGo", h4("Results")),
              dataTableOutput("rfResult"),
              br(),
              conditionalPanel(condition = "input.modelGo", h4("Variable Importance")),
              plotOutput("rfVarImp")
            )
          ),
          fluidRow(
            column(4,
              actionButton("modelGo", "Train Models"),
              br(),
              conditionalPanel(condition = "! input.modelGo", p("Please note that the models may take a few minutes to train & test"))
            ),
            column(8,
              conditionalPanel(condition = "input.modelGo", h4("Comparing All Models on the Test Data")),
              dataTableOutput("predComp")
            )
          )
        ),
        tabPanel(
          "Prediction",
          sidebarLayout(
            sidebarPanel(
              h3("Predict"),
              conditionalPanel(
                condition = "input.modelGo == 0",
                p("Please train the models before using them to predict.")
              ),
              conditionalPanel(
                condition = "input.modelGo != 0",
                p("Please select values for each predictor used in the model:"),
                uiOutput("varOps")
              ),
              actionButton("predGo", "Predict Outcome")
            ),
            mainPanel(
              uiOutput("thePred"),
              dataTableOutput("userPred")
            )
          )
        )
      ),
      tabPanel(
        "Data",
        sidebarLayout(
          sidebarPanel(
            h3("Data"),
            p("Select columns to include or exclude here in the sidebar. Data filters can be found at the top of each column in the table."),
            br(),
            checkboxInput("filterCol", "Edit visible columns?"),
            conditionalPanel(
              condition = "input.filterCol",
              selectInput("dataCols", "Select columns to show", choices = names(theT), selected = names(theT), multiple = TRUE)
            ),
            downloadButton("dlData", "Export as .csv")
          ),
          mainPanel(
            dataTableOutput("theT")
          )
        )
      )
    )
  )
)

