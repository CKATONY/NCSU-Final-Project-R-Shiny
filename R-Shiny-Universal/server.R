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
library(factoextra) 
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
                                                 mean_episode = mean(total_episode),
                                                 IQR_rating = IQR(rating),
                                                 IQR_users = IQR(user_count))
        }
        else{
            new %>% group_by(year) %>%
                summarize(percent = 100 * n() / nrow(new),mean_rating = mean(rating),
                          mean_user = mean(user_count),
                          mean_favorite = mean(favorite_count),
                          mean_episode = mean(total_episode),
                          IQR_rating = IQR(rating),
                          IQR_users = IQR(user_count))
        }
    })
    
    output$Datatable4 <- renderDataTable({
        new <- na.omit(data)
        if(input$percent2 == FALSE){
             new %>% filter(age_rating == input$age_rating2) %>% summarize(mean_rating = mean(rating),
                                                 mean_user = mean(user_count),
                                                 mean_favorite = mean(favorite_count),
                                                 mean_episode = mean(total_episode),
                                                 IQR_rating = IQR(rating),
                                                 IQR_users = IQR(user_count))
        }
        else{
             new %>% group_by(age_rating) %>%
                summarize(percent = 100 * n() / nrow(new),mean_rating = mean(rating),
                          mean_user = mean(user_count),
                          mean_favorite = mean(favorite_count),
                          mean_episode = mean(total_episode),
                          IQR_rating = IQR(rating),
                          IQR_users = IQR(user_count))
        }
    })
    output$Datatable5 <- renderDataTable({
        final <- data %>% mutate(total_episode = if_else(is.na(total_episode), 0, total_episode))
        v <- ifelse(final$total_episode >= 15, "Episodes >= 15",
                    ifelse(final$total_episode >= 8, "15>Episodes >=8",
                           ifelse(final$total_episode > 0,"8 > Episodes","missing values")))
        final$episode_categories <- v
        if(input$percent3 == FALSE){
            final %>% filter(episode_categories == input$episodes) %>% summarize(mean_rating = mean(rating),
                                                                          mean_user = mean(user_count),
                                                                          mean_favorite = mean(favorite_count),
                                                                          mean_episode = mean(total_episode),
                                                                          IQR_rating = IQR(rating),
                                                                          IQR_users = IQR(user_count))
        }
        else{
            final %>% group_by(episode_categories) %>%
                summarize(percent = 100 * n() / nrow(data),mean_rating = mean(rating),
                          mean_user = mean(user_count),
                          mean_favorite = mean(favorite_count),
                          mean_episode = mean(total_episode),
                          IQR_rating = IQR(rating),
                          IQR_users = IQR(user_count))
        }
    })
    
    
    
   
    output$plotyear <- renderPlot({
        if(input$kmeans == FALSE & input$kmeans_explore == FALSE){
            newdata <- data %>% filter(year == input$year3)
            scatter <- ggplot(newdata, aes(x = user_count, y = rating))
            scatter + geom_point(aes(color = age_rating,size = rating)) + 
              geom_smooth(method = "lm") + 
              labs(title = paste0("user counts vs rating for year: ",input$year3), x = "user count", y = "anime rating") + 
              scale_color_discrete(name = "age rating")
        }
        else if(input$kmeans == TRUE & input$kmeans_explore == FALSE){
            observeEvent(input$popularity, {updateSliderInput(session,"kmean", max = 6)})
            observeEvent(input$popularity, {updateSliderInput(session,"year3", max = 2022)})
            newdata <- data %>% filter(year == input$year3)
            last <- newdata %>% select(user_count,rating)
              
             
            k <- kmeans(last, centers = input$kmean, nstart = 25)
            fviz_cluster(k, data = last)
        }
        else if(input$kmeans == TRUE & input$kmeans_explore == TRUE){
            observeEvent(input$popularity, {updateSliderInput(session,"year3", max = 2021)})
            observeEvent(input$popularity, {updateSliderInput(session,"kmean", max = 4)})
            
            final <- data %>% mutate(total_episode = if_else(is.na(total_episode), 0, total_episode))
            v <- ifelse(final$total_episode >= 15, "Episodes >= 15",
                        ifelse(final$total_episode >= 8, "15>Episodes >=8",
                               ifelse(final$total_episode > 0,"8 > Episodes","missing values")))
            final$episode_categories <- v
            newdata <- final %>%  filter(year == input$year3) %>% select(user_count,rating)
            newdata2 <- final %>% filter(year == input$year3)
            k <- kmeans(newdata, centers = input$kmean, nstart = 25)
            cluster_level <- k$cluster
            new <- cbind(newdata2,cluster_level)
            last <- new %>% filter(popularity_rank < input$popularity)
            
            ggplot(last,aes(as.factor(episode_categories), fill = as.factor(age_rating))) + 
                geom_bar(position = "stack")+
                facet_grid(cols = vars(cluster_level),labeller = label_both)+ 
                scale_fill_discrete(name = "age rating") +
                labs (x = "episodes category",
                    title = paste(paste("Counts for top ",input$popularity), paste("popularity paneled by cluster for year",input$year3)))+
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
            
            
        }})
    
    output$text <-renderText({
        if(input$kmeans == FALSE & input$kmeans_explore == TRUE){
            "please check the k mean cluster checkbox to see the plots involving clustering"
        }
    })

    output$plotmonth <- renderPlot({
        newdata <- data %>% filter(month == input$month2)
        bar <- ggplot(newdata, aes(x = year))
        bar + geom_bar(aes(fill = as.factor(age_rating)), 
                         position = "stack",
                         show.legend = NA) +         labs(x = "Year") + 
            scale_fill_discrete(name = "age rating") + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
            labs(title = "year by age_rating within each month")
    })
    
    
    
    
    
    
    
})




























