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
library(caret)


data <- read_delim("anime_FinalInfo_from_Kitsu_API.csv",delim =" ")

shinyUI(fluidPage(theme = "style.css",
                  div(style = "padding: 1px 0px; width: '100%'",
                      titlePanel(
                          title = "Exploring Animes from Kitsu WebSite",
                          windowTitle = "Exploring Animes from Kitsu"
                      )
                  ),
                  navbarPage("Exploring Animes from Kitsu",
                             tabPanel(
                                 "About",
                                 dashboardPage(
                                   dashboardHeader(title = "About this App"),
                                   dashboardSidebar(
                                     "Description of this app"),
                                   dashboardBody(
                                     fluidRow(
                                       column(
                                         box(
                                           title = "Purpose of the App", 
                                           status = "primary",
                                           solidHeader = TRUE,
                                           collapsible = FALSE,
                                           h4("This app explores the animes in Kitsu website. Users could change the functionality(preference) to obtain the information of the 
                                           subseted data, see some exploratory data analysis based on the entire data set, fit some predictive models(supervised learning methods), and predict the rating of the anime 
                                           based on the explanatory(predictors) values that are specified by users. 
                                            
                                      "),
                                           
                                         width = 15),width = 2),
                                       column(
                                         box(
                                           title = "Data and Source", 
                                           status = "info",
                                           solidHeader = TRUE,
                                           collapsible = FALSE,
                                           h4("Data is obtained by accessing the Kitsu API. The more information of this API can be found",
                                              tags$a(href= "https://kitsu.docs.apiary.io/#",
                                                  "Here.")),
                                           
                                           h4("This API is built upon the Kitsu Website, Kitsu is a anime website that allows users to explore anime and manga.
                                              You can access the Kitsu Website",tags$a(href= "https://kitsu.io/explore/anime",
                                                  "Here.")),
                                           h4("Animes used in this app is selected by top 300 ratings for each year from 2010 to 2022, "),
                                           h4("Data used in this app is filtered with necessary attributes of anime. Variables include:"),
                                           h4("1.Anime Name in English"),
                                           h4("2.Anime Name in Japanese"),
                                           h4("3.Rating of Anime"),
                                           h4("4.Favorite Count of each Anime(if users like the anime, they hit the favorite tab)"),
                                           h4("5.User Count(the users who have watched the animes)"),
                                           h4("6.Airing Start Date and Airing End Date"),
                                           h4("7.Popularity Rank"),
                                           h4("8.Rating Rank"),
                                           h4("9.Age Rating(category that provides suitable range of age for animes"),
                                           h4("10.Age Rating Guide(provide age range)"),
                                           h4("11.Toal Number of Episode of the Anime"),
                                           h4("12.Total Length of Anime in Minutes"),
                                           h4("13.Genre and Categories(genres of each anime"),
                                           
                                           
                                           
                                           
                                           width = 12),width = 3),
                                       column(
                                         box(
                                           title = "Purpose of the tabs", 
                                           status = "danger",
                                           solidHeader = TRUE,
                                           collapsible = FALSE,
                                           h4("You have 4 pages:"),
                                              
                                           h4(tags$span(style="color: blue;","About Page:"), "Discussion of the purpose of the app, data and its source and purpose of each tab"),
                                              
                                           h4(tags$span(style="color: blue;","Data Page:")," Subset the dataset based on your interest and save the dataset"),
                                              
                                           h4(tags$span(style="color: blue;","Data Exploration Page:"))  ,
                                              
                                           h4("1. Table Summary tab : Change the numerical summaries based on Year, Age Rating and Number of Episodes"),
                                              
                                           h4("2. Visualization tab : See the scatter plots (kmean clustering included) and bar plots based on interest"),
                                           h4(tags$span(style="color: blue;","Modeling Page:")),
                                           h4("1. Modleing info tab : See the information of models that will be used to fit the data"),
                                           h4("2. Model Fitting tab : Fit the model based on interest"),
                                           h4("3. Prediction tab : predict the response: rating based on the input values from you"),
                                              
                                              
                                           
                                           width = 12),width = 3),
                                       column(
                                         tags$img(src = "onepiece.jpg",width="500px",height="1200px"),width = 4)
                                       
                                
                                    
                                     
                                 
                                 
                             )
                             )
                             )
                             )
                             ,
                             tabPanel(
                                 "Data",
                                 tabsetPanel(
                                     type = "tabs",
                                     
                                     tabPanel(
                                         "Select Animes You Like",
                                         dashboardPage(
                                           dashboardHeader(title = "Subset the Data"),
                                           dashboardSidebar(
                                             "You can subset the data based on your interest"),
                                           dashboardBody(
                                             fluidRow(
                                               column(
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
                                                 ,width = 2),
                                               
                                               column(
                                                 DT::dataTableOutput("Datatable1")
                                                 ,width = 10)
                                             )
                                           )
                                         )
                                     ),

                                     tabPanel(
                                         "Based on rank and genre",
                                         dashboardPage(
                                           dashboardHeader(title = "Subset the Data"),
                                           dashboardSidebar(
                                             "You can select the anime based on the rating rank"),
                                           dashboardBody(
                                             fluidRow(
                                               column(
                                                 
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
                                                 ,width = 2),
                                               
                                               column(
                                                 
                                                 DT::dataTableOutput("Datatable2"),
                                                 width = 10)
                                             )
                                           )
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
                                      "This is the table summaries, it provides the mean of rating, user counts, favorite counts and episode number, Interquantile range of rating and users counts
                                      based on year, age rating guide, or episode category you choose"
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
                                     dashboardPage(
                                       dashboardHeader(title = "Data Visualization"),
                                       dashboardSidebar(
                                         "This tab uses Scatter Plot and Bar Plot to provide graphical visualizations.",
                                         br(),
                                         "The scatter plot explore the relationship of user counts and rating of anime based on year. ",
                                         br(),
                                         "The k mean cluster explore further about those two variables.",
                                         br(),
                                         "You can hit the check box to see more explorations based on the clusters",
                                         br(),
                                         "The bar plot explore the counts of animes for each month across year(bar stacked by age rating guide)"
                                       ),
                                       dashboardBody(
                                         fluidRow(
                                           box(
                                             title = "Scatter Plot and K means Clustering Functionality", 
                                             status = "success", 
                                             solidHeader = TRUE,
                                             collapsible = FALSE,
                                             h3("Plot based on year"),
                                             sliderInput("year3",
                                                         "Years",
                                                         min = 2010,
                                                         max = 2022,
                                                         step = 1,
                                                         value = 2010), 
                                             h4("By hitting the check box below, you could see the k-mean clustering of two variables: user count and rating.
                                           The original values are scaled"),
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
                                             h4("By hitting the check box below, you could see the counts for the top number of popularity rank you selected,
                                          the plot is exploring how episode numbers(3 categories) could possibly affect the popularity rank within each cluster 
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
                                                                          selected = 101))
                                           ),
                                           box(
                                             title = "Plots", 
                                             status = "success", 
                                             solidHeader = TRUE,
                                             collapsible = FALSE,
                                             textOutput("text"),
                                             plotOutput("plotyear")
                                             
                                           )
                                         ),
                                         
                                         fluidRow(
                                           box(
                                             title = "Bar Plot Functionality", 
                                             status = "info", 
                                             solidHeader = TRUE,
                                             collapsible = FALSE,
                                             h4("Plot based on month for each year"),
                                             
                                             sliderInput("month2",
                                                   "Months",
                                                   min = 1,
                                                   max = 12,
                                                   step =1,
                                                   value = 1)
                                             ),
                                           box(
                                             title = "Plot", 
                                             status = "info", 
                                             solidHeader = TRUE,
                                             collapsible = FALSE,
                                             h4("Plot based on month for each year"),
                                             plotOutput("plotmonth")
                                           )
                                     )
                                     )
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
                                             
                                             "we want to fit the dataset using predictive models. Overall speaking, we want to model to have less MSE(mean square error)
                                             MSE is measured by the bias + variance + irreducible error.",
                                             h3("Note that here we want to model the response variable rating of the anime by other explantory varaibles and rating is a 
                                                quantitative response, so we choose the models under regression setting.")
                                           ),
                                           dashboardBody(
                                             fluidRow(
                                               column(
                                                 box(
                                               title = "Model 1", 
                                               status = "warning",
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               "Multiple Regression Method",
                                               br(),
                                               "Use the multiple linear regression method to fit the response varaible rating",
                                               br(),
                                               "we fit the response using the subset of predictors",
                                               br(),
                                               br(),
                                               withMathJax(),
                                               "The modeling equation for the multiple linear regression is
                                                        $$Y = \\beta_0+\\beta_1*X_1+\\beta_2*X_2+\\beta_3*X_3+\\beta_4*X_4+\\beta_5*X_5...$$",
                                               "notice that here the multiple linear regression model have simple interpretation of 
                                               relationship between X's and Y, the order of X's is 1",
                                               br(),
                                               br(),
                                               "however, this method have some drawbacks:",
                                               br(),
                                               br(),
                                               "We are not including the synergy effect(interaction effect) and higher order terms of X's 
                                               in the model, we lose the interpretation of additive effects, and linear regression have linearity
                                               assumptions which will require us to transfrom Y or X if applicable",
                                               
                                              
                                               width = 12),width = 3
                                               ),
                                               column(
                                                 box(
                                                   title = "Model 2", 
                                                   status = "info",
                                                   solidHeader = TRUE,
                                                   collapsible = FALSE,
                                                   "Regession Tree Method",
                                                   br(),
                                                   "The idea of regression tree method is to stratify the predictors(explanatory variables space into a simple region
                                                   using mean or mode of training observations to make prediction for a given observation. The rules for splitting the
                                                   predictors can be summarized in a treem so it is called the tree methods.",
                                                   br(),
                                                   withMathJax(),
                                                   "Here is a simple algorithm to explain the regression tree method:",
                                                   br(),
                                                   br(),
                                                   "Step 1: We divide $$X_1,...,X_{p}$$ in to J distinct, non-overlapping region, $$R_1,...,R_{j}$$",
                                                   "Step 2: We use the mean of the response values for training observations in Rj as the prediction to stratify the observations that fall into redion Rj",
                                                   br(),
                                                   br(),
                                                   "Step 3: We want to divide the predictor space into high dimension boxes. And the goal is to find boxes $$R_1,...,R_{j}$$ 
                                                   that minimize the residual sum of square, which is given by $$\\sum_{j=1}^J \\sum_{i\\in {R_{j}}} (y_i- \\hat{y}_{R_{j}})^2$$
                                                   Where $$\\hat{y}_{R_{j}}$$ is mean response for the training observations within the $$j_{th}$$ box",
                                                   br(),
                                                   br(),
                                                   "There are ways to improve the tree method, like recursive binary splitting, Tree pruning, and etc...",
                                                   
                                                   br(),
                                                   br(),
                                                   "The decsion tree method is simple and useful for interpretation but it is not competitive for other supervised learning methods
                                                   (like lasso, ridge or dimension reduction method) in terms of prediction accuracy",
                                                   
                                                
                                                   
                                                   
                                                   width = 12),width = 3
                                               ),
                                               column(
                                             box(
                                               title = "Model 3", 
                                               status = "primary",
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               withMathJax(),
                                               "Random Forest Regression Method",
                                               br(),
                                               "random forest regression method is one of the aggregated decision trees methods that uses resampling method, like bootstrapping methods to 
                                               generate the data sets based on the original dataset(allow the repeatness of the observations)
                                               And then we fit the regression tree using predictors under random forest setting. Random forest regression
                                               is a de-correlate methods, because under the setting of tree method, if we use same predicotrs all the time,
                                               the trees will be highly correlated. Under the setting of random forest method, we randomly select m predictors 
                                               to fit the model, here the m will be chose by k-fold cross validation so that the averaged mean square error will be 
                                               minimized",
                                               br(),
                                               br(),
                                               "Algorithms that can explain the random forest regression:",
                                               br(),
                                               br(),
                                               "Step 1: Random sample with replacement, $$b = 1,...,B$$",
                                               "Step 2: train regression tree $$f_b$$ on the sample data, details can be seen from the model 2",
                                               br(),
                                               br(),
                                               "Step 3: use the k-fold cross validation to determine the value of m to use, notice that m is usually $$\\sqrt(p)$$ or $$\\frac{p}{3}$$ p is the total predictors",
                                               br(),
                                               br(),
                                               "Step 4: the predictions for sampled $$x's$$ will be averaged to reduce the test error, $$\\hat{f} = \\frac{1}{B}\\cdot\\sum_{b=1}^B f_b$$",
                                                        "Using random forest could definitely reduce the variance and overfitting problem and therefore improve the accuracy of the model,
                                                        trees method is better to visualize but it runs slowly when the predictors are large. Meanwhile, the categorical variables are not suitable under the 
                                                        random regression setting",
                                               width = 12),width = 3
                                               ),
                                              column(
                                                box(
                                               title = "Model 4", 
                                               status = "success",
                                               solidHeader = TRUE,
                                               collapsible = FALSE,
                                               withMathJax(),
                                               "Shrinkage Method: Ridge Regression",
                                               br(),
                                               "Shrinkage method like Ridge regression could reduce the variance by shrinking the parameters estimates using the tuning paramter added in the model,
                                               Ridge regression shrinks the coefficient estimates towards 0",
                                               br(),
                                               "The usual linear regression method uses least square to estimate the coefficients, it usually has low bias but high variance",
                                               br(),
                                               "The algorithms of ridge regression:",
                                               br(),
                                               br(),
                                                        "Step 1: For the least square method, the residual sum of square is represented as 
                                                        $$RSS = \\sum_{i=1}^n (y_i-(\\beta_0 + \\sum_{j=1}^p \\beta_{j}\\cdot X_{ij}))^2$$",
                                               br(),
                                                        
                                                        "Step 2: Ridge regression adds shrinkage penalty $$\\lambda \\sum_{j=1}^p \\beta^2_{j}$$ here $$\\lambda > 0$$, is a tuning parameter",
                                               br(),
                                               br(),
                                                        "Step 3: Ridge regression coefficient estimates $$\\hat{\\beta}^{R}$$ are the values that minimize
                                                        $$\\sum_{i=1}^n (y_i-(\\beta_0 + \\sum_{j=1}^p \\beta_{j}\\cdot X_{ij}))^2 + \\lambda \\sum_{j=1}^p \\beta^2_{j}$$",
                                               br(),
                                               
                                                        "Step 4: Lambda here controls the relative impact of RSS and shrinkage penalty stated above. Ridge will produce a difference set of coefficient
                                                        estimate $$\\hat{\\beta}^{R}_{\\lambda}$$ for each value of $$\\lambda$$. The lambda here is basically determined by cross validation",
                                               br(),
                                               br(),
                                               "Ridge regression definitely improve the accuracy of the model fitting if there are variables that have less impact on the model, it works better
                                               when the least squares method have high variance, it also has a computational advantage over other variable selection method to reduce the dimension of the data
                                               But unfortunately, it will only set the coeffcient estimates to be close to 0, it did not perform variable selection",
                                               width = 12),width = 3
                                              )
                                             
                                             )
                                             
                                        
                                     ))),
                                     tabPanel(
                                         "Model Fitting",
                                         dashboardPage(
                                           dashboardHeader(title = "Modeling Fitting"),
                                           dashboardSidebar(
                                             "This is the Model Fitting page:",
                                             "The left side box allow you to:",
                                             br(),
                                             "choose the proportion of data used",
                                             "select variable used for each model",
                                             br(),
                                             "And the right side box will show the corresponding results based on your choice"
                                           ),
                                           dashboardBody(
                                             fluidRow(
                                               column(
                                                 box(
                                                   title = "Functionality", 
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   collapsible = FALSE,
                                                   p("note that you will not see the result unless you hit the submit botton", style = "color:red"),
                                                   h3("Splitting the training and test data set:"),
                                                   sliderInput("split",
                                                               "Proportions of training data",
                                                               min = 0.5,
                                                               max = 0.9,
                                                               step = 0.1,
                                                               value = 0.7),
                                                   radioButtons("CV",
                                                                "Select K for the k-fold cross validation",
                                                                choices = c(3,5,10),
                                                                selected = 3
                                                   ),
                                                   p("Note that random forest run slowly if the cv is large",style = "color:red"),
                                                   h4("For this data set, we want to fit the model with response variable: rating, rating ranges from 0 to 100, 100 denotes the perfect rating.
                                                   we will have all appropriate explantory variables:
                                                      favorite count, user count, year, month, popularity rank, age rating, total episodes number, total length of anime in minutes"),
                                                   h4("before fitting the model, I set the categorical variable age_rating(3 categories) to be three numerical dummy variables, with each column have value (0,1) indicating whether 
                                                      the observation falls in this category. Also fixed some negative values in total length column. Lastly, omit some rows that have missing values of total episodes."),
                                                   p("note that in the multiple linear regression, we droped the one of the dummy variable where age_rating = G because this category has collinearity with category PG",style = "color:red"),
                                                   h4("Next Step: I set the base model to have variables: 
                                                      favorite count, user count, month, total episodes number, age_rating(3 dummies),
                                                      you can choose if you want to add other variables:"),
                                                   pickerInput("variable",
                                                               "choose the variable you prefer:",
                                                               choices = c("popularity_rank","year","total_length","all included"),
                                                               selected = "popularity_rank"),
                                                   
                                                   radioButtons("model",
                                                                "Select Model you want to fit:",
                                                                choices = c("multiple linear regression","regression tree","random forest regression","ridge regression","All models"),
                                                                selected = "multiple linear regression"
                                                                ),
                                                   
                                                   actionButton("press",
                                                                "Submit",
                                                                color = "danger",
                                                                no_outline = TRUE),
                                                   
                                                   width = 12),width = 5),
                                               column(
                                                 box(
                                                   title = "Results", 
                                                   status = "danger",
                                                   solidHeader = TRUE,
                                                   collapsible = FALSE,
                                                   "Results from model fitting:",
                                                   
                                                   
                                                   h3("Info and Results for the multiple linear regression:"),
                                                   verbatimTextOutput("Multiple"),
                                                   h3("Info and Results for the Regression Tree:"),
                                                   h4("Here this CART model replicates the same process used by the rpart function where the model complexity is determined using the one-standard error method.
                                                      We do not need to specify the tuning parameter complecity parameter in this case"),
                                                   verbatimTextOutput("Tree"),
                                                   h3("Info and Results for the Random Forest Regression:"),
                                                   h4("it may take a minute to run random forest"),
                                                   verbatimTextOutput("Random"),
                                                   h4("Here the plot shows the importance of variable using the random forest method, if greater the Node Purity, the more important of the popularity"),
                                                   plotOutput("Random2"),
                                                   h3("Info and Results for the Ridge Regression:"),
                                                   verbatimTextOutput("Ridge"),
                                                   h3("All model compared using the test set"),
                                                   h4("Note:only show when you select all models"),
                                                   dataTableOutput("AllStats"),
                                                   
                                                  
                                                   
                                                   width = 12),width = 7)
                                             )
                                           )

                                      )
                                      ),
                                     
                                     
                                     
                                     
                                     
                                     tabPanel(
                                         "Prediction",
                                         dashboardPage(
                                           dashboardHeader(title = "Predicting Rating"),
                                           dashboardSidebar(
                                             "This page is predicting page,
                                             you can select model and specify the values of explanatory variables to predict the response variable rating"),
                                           dashboardBody(
                                             fluidRow(
                                               column(
                                                 box(
                                                   title = "Select models and Specify the values", 
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   collapsible = FALSE,
                                                   p("note that you will not see the result unless you hit the Predict botton", style = "color:red"),
                                                   p("And the age guide = G is set to be 0 all the time becasue of the high correlation", style = "color:red"),
                                                   h3("Model to use:"),
                                                   h4("note that the random forest model is opt out since it runs slowly"),
                                                   radioButtons("M",
                                                                "Select Model you want to predict:",
                                                                choices = c("multiple.linear.regression","regression.tree","ridge.regression"),
                                                                selected = "multiple.linear.regression"
                                                   ),
                                                   numericInput("fcount",
                                                                "favorite count(range from 0 to 1000)",
                                                                min = 0,
                                                                max= 10000,
                                                                value = 0,
                                                                step = 1),
                                                   numericInput("ucount",
                                                                "user count(range from 0 to 350000)",
                                                                min = 0,
                                                                value = 0,
                                                                max= 350000,
                                                                step = 1),
                                                   numericInput("year4",
                                                                "year(range from 2010 to 2022)",
                                                                min = 2010,
                                                                max= 2022,
                                                                value = 2010,
                                                                step = 1),
                                                   numericInput("month4",
                                                                "month(range from 1 to 12)",
                                                                min = 1,
                                                                max= 12,
                                                                value = 1,
                                                                step = 1),
                                                   numericInput("pop",
                                                                "popularity rank (range from 1 to 20000)",
                                                                min = 1,
                                                                max= 20000,
                                                                value = 1,
                                                                step = 1),
                                                   numericInput("Nep",
                                                                "number of episodes  (range from 1 to 260)",
                                                                min = 1,
                                                                max= 260,
                                                                value = 1,
                                                                step = 1),
                                                   numericInput("Lep",
                                                                "Length of Episodes in minute  (range from 1 to 6500)",
                                                                min = 1,
                                                                max= 6500,
                                                                value = 1,
                                                                step = 1),
                                                   radioButtons("age_guide",
                                                                "Select Age guide:",
                                                                choices = c("age_ratingPG","age_ratingR"),
                                                                selected = "age_raingPG"
                                                   ),
                                                   actionButton("pred",
                                                                "Predict"),
                                                   width = 12),width = 2
                                                 ),
                                               column(
                                                 box(
                                                   title = "Predicted Rating(range from 0 to 100)", 
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   collapsible = FALSE,
                                                   h3("Predicted value"),
                                                   dataTableOutput("values"),
                                                   
                                                   width = 12),width = 10
                                               )
                                               )
                                             )
                                           
                                           )
                                     )
                                 )
                             )
                             
                             
                             
                             
                             
                             
                             )
                  
                                       
                      
                      
 
    # Application title
   
))






























