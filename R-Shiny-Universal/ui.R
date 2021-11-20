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
library(factoextra)
library(shinydashboard)


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
                                  dashboardPage(
                                    dashboardHeader(title = "Table Summaries"),
                                    dashboardSidebar(
                                      "This is the table summaries"
                                    ),
                                    dashboardBody(
                                     fluidRow(
                                       box(
                                       title = "Get summary statistics based on year", 
                                       status = "warning",
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       radioButtons(inputId = "year2",
                                                   label = "Years",
                                                   choices = c(2010:2022),
                                                   selected = 2010),
                                       checkboxInput("percent1", 
                                                     label = tags$span(style="color: red;","see percentage of year with summary"), 
                                                     value = FALSE, 
                                                     width = NULL)),
                                       box(
                                         title = "Summary Stats based on year",
                                         status = "warning",
                                         solidHeader = TRUE,
                                         dataTableOutput("Datatable3")
                                       )
                                     ),
                                     
                                     fluidRow(
                                       box(
                                         title = "Get summary statistics based on age rating", 
                                         status = "primary", 
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         radioButtons(inputId = "age_rating2",
                                                      label = "Age Rating",
                                                      choices = c("G", "PG", "R"),
                                                      selected = "G"),
                                         checkboxInput("percent2", 
                                                       label = tags$span(style="color: red;","see percentage of age_rating with summary"), 
                                                       value = FALSE, 
                                                       width = NULL)),
                                       box(
                                         title = "Summary Stats based on age rating",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         dataTableOutput("Datatable4")
                                       )
                                     ),
                                     
                                     
                                     fluidRow(
                                       box(
                                         title = "Get summary statistics based on number of episodes", 
                                         status = "success", 
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         radioButtons(inputId = "episodes",
                                                      label = "Episodes Category",
                                                      choices = c("Episodes >= 15", "15>Episodes >=8", "8 > Episodes","missing values"),
                                                      selected = "15>Episodes >=8"),
                                         "note that missing episode number is denoted as value 0 for better interpretation",
                                         checkboxInput("percent3", 
                                                       label = tags$span(style="color: red;","see percentage of Episode Category with summary"), 
                                                       value = FALSE, 
                                                       width = NULL)),
                                       box(
                                         title = "Summary Stats based on number of episodes",
                                         status = "success",
                                         solidHeader = TRUE,
                                         dataTableOutput("Datatable5")
                                       )
                                     )))),

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
                                       h4("by hitting the check box below, you could see the k-mean clustering of two variables: user count and rating,
                                           and the original values are scaled"),
                                       checkboxInput("kmeans", 
                                                     label = tags$span(style="color: blue;","try k-means clustering based on user count and rating"), 
                                                     value = FALSE, 
                                                     width = NULL),
                                      
                                       conditionalPanel(condition = "input.kmeans",
                                                        sliderInput("kmean",
                                                                    "Select K",
                                                                    min = 2,
                                                                    max = 6,
                                                                    step = 1,
                                                                    value = 2)),
                                       h4("by hitting the check box below, you could see the counts for the top number of popularity rank you selected,
                                          the plot is exploring the how episode numbers could possibly affect the popularity rank within each cluster 
                                          generated by the k mean clustering for each year"),
                                       tags$span(style="color: red;","note that missing episode number is denoted as value 0 for better interpretation"),
                                       checkboxInput("kmeans_explore", 
                                                     label = tags$span(style="color: blue;","explore more based on clustering?"), 
                                                     value = FALSE, 
                                                     width = NULL),
                                       conditionalPanel(condition = "input.kmeans_explore",
                                                        pickerInput("popularity",
                                                                    "Top Popularity rank based on the cluster",
                                                                    choices = c(101,201,501,999,4999,9999),
                                                                    selected = 101)),
                                       
                                       
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       h4("Plot based on month for each year"),
                                       sliderInput("month2",
                                                   "Months",
                                                   min = 1,
                                                   max = 12,
                                                   step =1,
                                                   value = 1)
                                         
                                       ),
                                     mainPanel(
                                       textOutput("text"),
                                       plotOutput("plotyear"),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
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
                                         "Modeling Info",
                                        dashboardPage(
                                           dashboardHeader(title = "Modeling Information"),
                                           dashboardSidebar(
                                             "This is the Modeling Information"
                                           ),
                                           dashboardBody(
                                             fluidRow(
                                               column(
                                                 box(
                                               title = "Model 1", 
                                               status = "warning",
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               "probably multiple regression method",
                                               "Use the multiple linear regression method to fit the response varaible rating",
                                               "we fit the response using the subset of predictors",
                                               withMathJax(),
                                               helpText("The modeling equation for the multiple linear regression is
                                                        $$Y = \\beta0+\\beta1*X1+\\beta2*X2+\\beta3*X3+\\beta4*X4+\\beta5*X5...$$",
                                               "notice that here the multiple linear regression model have simple interpretation of 
                                               relationship between X's and Y, the order of X's is 1",
                                               "however, this method have some drawbacks:
                                               We are not including the synergy effect(interaction effect) and higher order terms of X's 
                                               in the model, we lose the interpretation of additive effects, and linear regression have linearity
                                               assumptions which will require us to transfrom Y or X if applicable"),
                                               
                                              
                                               width = 12),width = 4
                                               ),
                                               column(
                                             box(
                                               title = "Model 2", 
                                               status = "primary",
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               withMathJax(),
                                               "random forest regression method",
                                               "random forest regression method uses resampling method, like bootstrapping methods to 
                                               generate the data sets based on the original dataset(allow the repeatness of the observations)
                                               And then we fit the model using predictors by random forest regression. Random forest regression
                                               is a de-correlate methods, because under the setting of tree method, if we use same predicotrs all the time,
                                               the trees will be highly correlated. Under the setting of random forest method, we randomly select m predictors 
                                               to fit the model, here the m will be chose by k-fold cross validation so that the averaged mean square error will be 
                                               minimized",
                                               helpText("Algorithms that can explain the random forest regression:
                                               Step 1: Random sample with replacement, $$b = 1,...,B$$
                                               step 2: train regression tree $$f_b$$ on the sample data
                                               step 3: use the k-fold cross validation to determine the value of m to use, notice that m is usually $$\\sqrt(p)$$ or $$\\frac{p}{3}$$ p is the total predictors"),
                                               helpText("step 4: the predictions for sampled $$x's$$ will be averaged to reduce the test error, $$estimated f = \\frac{1}{B}\\cdot\\sum_{b=1}^B f_b$$"),
                                               width = 12),width = 4
                                               ),
                                              column(
                                                box(
                                               title = "Model 3", 
                                               status = "success",
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               "dfhiousdhauigfhiua",width = 12),width = 4
                                              )
                                             
                                             )
                                             
                                        
                                     ))),
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


