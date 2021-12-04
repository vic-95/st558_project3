README
================
Victoria Seng
11/17/2021

## What is this?

This app allows the user to explore, model, and predict with online
shopping data. Besides making visualizations, summaries, and exploring &
exporting the data itself, the user can design and train three different
kinds of predictive model for whether or not a shopper will generate
revenue. They can then create a fictitious observation that will be
evaluated via the model and its outcome predicted.

## Required Packages

The following packages are required for the app to function:

-   `Shiny`
-   `shinythemes`
-   `tidyverse`
-   `magrittr`
-   `fastDummies`
-   `caret`
-   `ranger`
-   `rpart`
-   `rpart.plot`
-   `DT`
-   `stringr`

### Install packages

Use the code below to install the required packages

``` r
install.packages(c("shiny",
                   "shinythemes",
                   "tidyverse",
                   "magrittr",
                   "fastDummies",
                   "caret",
                   "ranger",
                   "rpart",
                   "rpart.plot",
                   "DT",
                   "stringr"
                   )
                 )
```

### Run App

Use the code below to run this app yourself:

``` r
shiny::runGitHub(repo = "st558_project3", 
                 username = "vic-95", 
                 ref = "main", 
                 subdir = "p3app/"
                 )
```
