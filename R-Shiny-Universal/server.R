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
# data page 
    output$Datatable1 <- renderDataTable({
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
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$year, ".csv", sep = "")
        },
        content = function(file){
            write.csv(Datatable1, file, row.names = FALSE)
        }
    )
    output$Datatable2 <- renderDataTable({
        B <-data[order(data$rating,decreasing = TRUE),]
        B %>% filter(Categories %in% input$genre|Genres %in% input$genre)%>% top_n(input$rank, rating)
        
    })
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$year, ".csv", sep = "")
        },
        content = function(file){
            write.csv(Datatable1, file, row.names = FALSE)
            
        }
    )
    # data exploration page 
    output$Datatable3 <- renderDataTable({
        new <- na.omit(data)
        if(input$percent1 == FALSE){
            new %>% filter( year == input$year2) %>% summarize(mean_rating = mean(rating),
                                                 mean_user = mean(user_count),
                                                 mean_favorite = mean(favorite_count),
                                                 mean_episode = mean(total_episode))
        }
        else{
            new %>% group_by(year) %>%
                summarize(percent = 100 * n() / nrow(new),mean_rating = mean(rating),
                          mean_user = mean(user_count),
                          mean_favorite = mean(favorite_count),
                          mean_episode = mean(total_episode))
        }
    })
    
    output$Datatable4 <- renderDataTable({
        new <- na.omit(data)
        if(input$percent2 == FALSE){
            new %>% filter(age_rating == input$age_rating2) %>% summarize(mean_rating = mean(rating),
                                                 mean_user = mean(user_count),
                                                 mean_favorite = mean(favorite_count),
                                                 mean_episode = mean(total_episode))
        }
        else{
            new %>% group_by(age_rating) %>%
                summarize(percent = 100 * n() / nrow(new),mean_rating = mean(rating),
                          mean_user = mean(user_count),
                          mean_favorite = mean(favorite_count),
                          mean_episode = mean(total_episode))
        }
    })
    
    output$plotyear <- renderPlot({
        newdata <- data %>% filter(year == input$year3)
        scatter <- ggplot(newdata, aes(x = user_count, y = rating))
        scatter + geom_point(aes(color = age_rating)) + 
            geom_smooth(method = "lm") + 
            labs(title = paste0("user counts vs rating for year: ",input$year3), x = "user count", y = "anime rating") + 
            scale_color_discrete(name = "age rating")
    })
    
    output$plotmonth <- renderPlot({
        newdata <- data %>% filter(month == input$month2)
        bar <- ggplot(newdata, aes(x = year))
        bar + geom_bar(aes(fill = as.factor(age_rating)), 
                         position = "stack",
                         show.legend = NA) +         labs(x = "Year") + 
            scale_fill_discrete(name = "age rating") + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
            labs(title = "year by age_rating ")
    })
    
    
    
    
    
    
    
})




























