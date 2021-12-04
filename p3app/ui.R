
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
              choices = append("None", catVars), selected = "None"
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
              p("This app gives you the ability to train three types of classification models: a generalized linear model in the binomial family, a classification tree, and a random forest model."),
              p("Because our response varible, Revenue, is categorical rather than continuous, we use classification to predict which one of two outcomes a user on a website will fall into: generating revenue or not generating revenue."),
              p("If our response variable was continuous, say, a dollar amount of revenue generated, we would instead use regression methods to predict a dollar amount of revenue for each person."),
              br()
            ),
            column(4,
              h4("Generalized Linear Model: Binomial"),
              p("logit function links the mean to the linear form of the model"),
              withMathJax(),
              p("logit for binomial/bernoulli is:"),
              helpText("$$X\\beta = \\ln(\\frac{\\mu}1-\\mu)$$"),
              p("where"),
              helpText("$$\\mu = \\frac{exp(X\\beta)}1+exp(X\\beta) = \\frac{1}1+exp(X\\beta)$$"),
              p("Yi is the outcome of the ith observation. Yi ~ Bin(mui)")
            ),
            column(4,
              h4("Classification Tree Model"),
              p("Classification trees are a supervised learning method in which predictor variables are split via nodes and probability of group membership differs by node."),
              br(),
              p("This model type is easy to understand visually and has a few ease-of-implementation benefits. Predictors do not need to be scaled and statistical assumptions of the data are unecessary."),
              br(),
              p("However, classification trees are highly sensitive to small changes in data, making them less able to handle new data. They also usually need to be pruned."),
              p("To predict group membership, we use the gini index."),
              helpText("For a binary (T/F) response like ours, where $$p=P(correct classification)$$ in a tree node:"),
              helpText("Gini: $$2p(1-p)$$"),
              helpText("Deviance: $$-2p\\log(p)-2(1-p)\\log(1-p)$$"),
              p("smaller gini and deviance values mean that a node classifies well.")
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
              br(),
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

