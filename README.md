# NCSU-ST558-Project-R-Shiny

This is NCSU st558 final project, creating dynamic R shiny app.  

This R shiny App explores anime from Kitsu website. Kitsu website is a very useful source to look for anime that you may like (based on your preference)  

Kitsu website have averagely over 610,000 visits every month and The most visitors come from the United States  

You can access the Kitsu website [here](https://kitsu.io/explore/anime)  

The data set used in this app is obtained from Kitsu API. You can see more details about this API [here](https://kitsu.docs.apiary.io/#)  

I choose to get top 300 rating anime from year 2010 to 2022 (2022 has few upcoming animes), the rating scores are rated by users in this website.  

## Purpose of this app 
Anime lovers sometimes are looking for anime with high ratings. This app allows you to explore anime(basically all Japanese anime) by subsetting the data set plots, summary statistics and some predictive model based on the rating and some other attributes of anime(year, month released popularity rank, age rating guide, episodes numbers, etc...)  

The functionality of this app provides you a comprehensive exploration of how some attributes could potentially affect rating scores. A simple example here: anime that fall in different age rating guide category may have different rating because the audiences may vary.  

The predictive models introduced in this app may not be good and there are definitely some correlations between some attributes(like popularity rank and user counts variables). But it is not the main purpose of this app. This app is developed to let users better understand how anime are favored by users from Kitsu website and how the attributes of anime could potentially explain the rating scores.  

## R Packages Used 

`shiny` [shiny package:](https://www.rdocumentation.org/packages/shiny/versions/1.7.1) Build the interactive web app in R  
`tidyverse` [tidyverse package:](https://www.tidyverse.org/) Data analysis package in R  
`caret` [caret package:](https://cran.r-project.org/web/packages/caret/vignettes/caret.html) Contains functions to streamline the model training process for complex regression and classification problems  
`randomForest` [randomForest package:](https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest) Classification and Regression with Random Forest  
`shinyWidgets` [shinyWidgets package:](https://cran.r-project.org/web/packages/shinyWidgets/index.html) Custom Inputs Widgets for Shiny  
`factoextra` [factoextra package:](https://cran.r-project.org/web/packages/factoextra/index.html) Extract and Visualize the Results of Multivariate Data Analyses  
`shinydashboard` [shinydashboard package:](https://cran.r-project.org/web/packages/shinydashboard/index.html)  Create Dashboards with 'Shiny'  
`elasticnet` [elasticnet package:](https://cran.r-project.org/web/packages/elasticnet/index.html) Elastic-Net for Sparse Estimation and Sparse PCA  

### Install the packages:

Use the code in R:  
install.packages(c("shiny","tidyverse","shinyWidgets","factoextra","shinydashboard","randomForest","caret","elasticnet"))  
if you want to install the individual package, just use install.package("any package you don't have in your R")  

After the installations, you can load those packages using:  

library(caret)  
library(shiny)  
library(tidyverse)  
library(factoextra)  
library(elasticnet)  
library(randomForest)  
library(shinyWidgets)  
library(shinydashboard)  





## How to Run this app:

*_please have all packages installed and loaded before running this app_*  
Use codes here to run this app in R:  

shiny::runGitHub(repo = "NCSU-Final-Project-R-Shiny",
                 username = "CKATONY",
                 ref = "main",
                 subdir = "R-Shiny-Universal")  






















