#
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(

    # Application title
    titlePanel("Relay Deal Intelligence"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        # TODO: conditional sidebar widgets for each tab
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabPanel(
              "About",
              # TODO: purpose of app, explain data & data source & tab purposes & image
            ),
            tabPanel(
              "Data Exploration",
              # TODO: Graphical summary, user input changes graph type, vars and summary type, filters
            ),
            tabPanel(
              "Modeling",
              tabPanel(
                "Modeling Info",
                # TODO: explain 3 models, benefits & drawbacks. use mathJax for math explainers
              ),
              tabPanel(
                "Model Fitting",
                # TODO: split into training & test (user selects ratio) 
                # TODO: model settings in sidebar (settings/vars/go button)
                # TODO: display models, report rmse on training, show summaries, show fit on test
              ),
              tabPanel(
                "Prediction",
                # TODO: allow users to select predictor vals and use model to show resp value
              )
            ),
            tabPanel(
              "Data",
              # TODO: show data set with user subsets and export functionality
            )
        )
    )
  )
)
