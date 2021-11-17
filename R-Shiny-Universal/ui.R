#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinyWidgets)

data <- read_delim("anime_FinalInfo_from_Kitsu_API.csv",delim = " ")
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "style.css",
                  div(style = "padding: 1px 0px; width: '100%'",
                      titlePanel(
                          title = "Exploring Animes and predicting Anime Rating from Kitsu WebSite",
                          windowTitle = "Exploring Animes from Kitsu"
                      )
                  ),
                  navbarPage("Exploring Animes from Kitsu",
                             tabPanel(
                                 "About",
                                 fluidRow(
                                     column(
                                         h2("Purpose of the App"),
                                         br(),
                                         p("......."),width = 5),
                                     column(
                                         h2("Data and Source"),width = 2
                                     ),
                                     column(
                                         h2("Purpose of the tabs"),
                                         br(),
                                         width = 3
                                     ),
                                     column(
                                         tags$img(src = "",width="200px",height="130px"),width = 2)
                                     )
                                
                                 
                                 
                             ),
                             tabPanel(
                                 "Data",
                                 tabsetPanel(
                                     type = "tabs",
                                     
                                     tabPanel(
                                         "Select Animes You Like",
                                           sidebarPanel(
                                             h3("Select the anime based on Year:"),
                                             pickerInput(inputId = "year",
                                                         label = "Years",
                                                         choices = c(2010:2022),
                                                         selected = 2010
                                             ),
                                             checkboxInput("month", 
                                                           label = tags$span(style="color: red;","select by month within year"), 
                                                           value = FALSE, 
                                                           width = NULL),
                                             conditionalPanel(condition = "input.month",
                                                              pickerInput("year_month",
                                                                          "Months", 
                                                                          choices = c(1:12),
                                                                          selected = 1)),
                                             checkboxInput("age", 
                                                           label = tags$span(style="color: red;","select by age rating guide"), 
                                                           value = FALSE, 
                                                           width = NULL),
                                             conditionalPanel(condition = "input.age",
                                                              radioButtons(inputId = "age_rating",
                                                         label = "Age Rating",
                                                         choices = c("G", "PG", "R"),
                                                         selected = "G"
                                             ))
                                             
                                     ),
                                     mainPanel(
                                       tableOutput("Datatable")
                                     )),
                                     tabPanel(
                                         "Based on criterion"
                                     ),
                                     tabPanel(
                                         "save the data set"
                                     )
                                 )
                             ),
                             tabPanel(
                                 "Data Exploration",
                                 tabsetPanel(
                                     type = "tabs",
                                     
                                 tabPanel(
                                     "Table Summaries"
                                 ),
                                 tabPanel(
                                     "Visualizations"
                                 )
                                 )
                             ),
                             tabPanel(
                                 "Modeling",
                                 tabsetPanel(
                                     type = "tabs",
                                     tabPanel(
                                         "Modeling Info",
                                     ),
                                     tabPanel(
                                         "Model Fitting"
                                     ),
                                     tabPanel(
                                         "Prediction"
                                     )
                                 )
                             )
                             
                             
                             
                             
                             
                             
                             )
                  
                                       
                      
                      
 
    # Application title
   
))


