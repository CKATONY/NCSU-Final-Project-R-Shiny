#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
data <- read_delim("anime_FinalInfo_from_Kitsu_API.csv",delim = " ")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

    output$Datatable <- renderTable({
        if(input$month == FALSE & input$age == FALSE){
            data %>% filter(year == input$year)
        }
        else if(input$month == FALSE & input$age == TRUE){
            data %>% filter(year == input$year & age_rating == input$age_rating)
        }
        else if(input$month == TRUE & input$age == FALSE){
            data %>% filter(year == input$year & month == input$year_month)
        }
        else if(input$month == TRUE & input$age == TRUE){
            data %>% filter(year == input$year & month == input$year_month & age_rating == input$age_rating)
        }
        
            
        
        
    })

})
