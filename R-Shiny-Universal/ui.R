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

data <- read_delim("anime_FinalInfo_from_Kitsu_API.csv",delim =" ")
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
                                             )),
                                             downloadButton("downloadData", "Download")
                                     ),
                                     mainPanel(
                                       dataTableOutput("Datatable1")
                                     )),
                                     tabPanel(
                                         "Based on rank and genre",
                                         sidebarPanel(
                                           h3("Select the anime based on the rating rank"),
                                           pickerInput(inputId = "rank",
                                                       label = "Ranking",
                                                       choices = c(1,10,20,30,40,50,60,70,80,90,100),
                                                       selected = 1
                                           ),
                                            
                                          
                                         h3("Based on Genre"),
                                           radioButtons(inputId = "genre",
                                                        label = "Categories",
                                                        choices = c("Fantasy", "Magic","Action", "Comedy", "School Life","Science Fiction","Romance","Horror",
                                                                    "Adventure","Music","Drama","Slice of Life","Sports",'Historical',"Mystery"),
                                                        selected = "Fantasy"
                                         ),
                                         downloadButton("downloadData2", "Download")
                                         ),
                                         mainPanel(
                                             dataTableOutput("Datatable2")

                                         )
                                       
                                     )
                                           
                                     )
                                 )
                             ,
                             tabPanel(
                                 "Data Exploration",
                                 tabsetPanel(
                                     type = "tabs",
                                     
                                 tabPanel(
                                     "Table Summaries",
                                     h3("Get summary statistics based on year"),
                                     sidebarPanel(
                                       radioButtons(inputId = "year2",
                                                   label = "Years",
                                                   choices = c(2010:2022),
                                                   selected = 2010),
                                       checkboxInput("percent1", 
                                                     label = tags$span(style="color: red;","see percentage of year with summary"), 
                                                     value = FALSE, 
                                                     width = NULL),
                                       br(),
                                       br(),
                                       h3("Get summary statistics based on age rating"),
                                       radioButtons(inputId = "age_rating2",
                                                    label = "Age Rating",
                                                    choices = c("G", "PG", "R"),
                                                    selected = "G"),
                                       checkboxInput("percent2", 
                                                     label = tags$span(style="color: red;","see percentage of age_rating with summary"), 
                                                     value = FALSE, 
                                                     width = NULL)
                                     ),
                                     mainPanel(
                                       dataTableOutput("Datatable3"),
                                       br(),
                                       br(),
                                       
                                       dataTableOutput("Datatable4")
                                     )
                                 ),
                                 tabPanel(
                                     "Visualizations",
                                     sidebarPanel(
                                       h3("Plot based on year"),
                                       sliderInput("year3",
                                                   "Years",
                                                   min = 2010,
                                                   max = 2022,
                                                   step = 1,
                                                   value = 2010),
                                       sliderInput("month2",
                                                   "Months",
                                                   min = 1,
                                                   max = 12,
                                                   step =1,
                                                   value = 1)
                                         
                                       ),
                                     mainPanel(
                                       plotOutput("plotyear"),
                                       plotOutput("plotmonth")
                                     )
                                     )
                                 )
                                 )
                            ,
                             tabPanel(
                                 "Modeling",
                                 tabsetPanel(
                                     type = "tabs",
                                     tabPanel(
                                         "Modeling Info"
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


