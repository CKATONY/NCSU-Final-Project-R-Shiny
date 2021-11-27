
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(factoextra)
library(randomForest)
library(elasticnet)
library(shinydashboard)
library(caret)

data <- read_delim("anime_FinalInfo_from_Kitsu_API.csv",delim = " ")


# Define server logic required to draw a histogram
shinyServer(function(input, output,session){
# data page 
    output$FullData <- DT::renderDataTable({
      data},
      options = list(scrollX = TRUE)
    )
    
    output$downloadData3 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(data, file, row.names = FALSE)
      }
    )
    
      
    subsetData <- data
    getData <- reactive({
        subsetData
    })
    
    tableData <- reactive({
      if(input$month == FALSE & input$age == FALSE){
        getData() %>% filter(year == input$year)
        
      }
      else if(input$month == FALSE & input$age == TRUE){
        getData() %>% filter(year == input$year & age_rating == input$age_rating)
        
      }
      else if(input$month == TRUE & input$age == FALSE){
        getData() %>% filter(year == input$year & month == input$year_month)
        
      }
      else if(input$month == TRUE & input$age == TRUE){
        getData() %>% filter(year == input$year & month == input$year_month & age_rating == input$age_rating)
      }
    })
    
    output$Datatable1 <- DT::renderDataTable({
      tableData()},
      options = list(scrollX = TRUE)
    )
    
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Anime-subset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(tableData(), file, row.names = FALSE)
      }
    )

    
    tableData2 <- reactive({
      getData() %>% filter( Categories %in% input$genre | Genres %in% input$genre ) %>% filter(rating >= input$rank)
    })
    
    output$Datatable2 <- DT::renderDataTable({
      tableData2()},
      options = list(scrollX = TRUE)
    )
    
        #B <-data[order(data$rating,decreasing = TRUE),]
    output$downloadData2 <- downloadHandler(
        filename = function() {
            paste("Anime-subset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file){
            write.csv(tableData2(), file, row.names = FALSE)
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
    
    
    
    
    
    
    
    #data cleaning before the model fitting 
    get <- data %>% select(c(rating,favorite_count,popularity_rank,month,year,user_count,age_rating,total_episode,total_length))
    # convert the age rating into numeric columns. 
    dummies <- dummyVars(rating ~ age_rating,data = get)
    pre<-predict(dummies, newdata = get)
    Z <- cbind(get,pre)
    final2 <- Z %>% select(-age_rating)
    last <- abs(final2)
    # data cleaning to get final data set:
    set.seed(123)
    final <- na.omit(last)
    rows <- sample(nrow(final))
    final <- final[rows, ]
    
    
    
    
 # create the event reactive that will only change the response when we select different input after hit the action botton    
    splitinput <- eventReactive(input$press,{ input$split})
    variableinput <- eventReactive(input$press,{input$variable})
    CVinput <- eventReactive(input$press,{input$CV})
    modelinput <- eventReactive(input$press,{input$model})
# deliver the response using observe event 
    observeEvent(input$press,{
      output$Multiple <- renderPrint({
        train.index.sub <- createDataPartition(y = final$rating, p = as.numeric(splitinput()), list = F)
        train.sub <- final[train.index.sub, ] # training set
        test.sub <- final[-train.index.sub, ] # test set
                     if(variableinput() != "all included"){
                       if(modelinput() == "All models" | modelinput() =="multiple linear regression"){
                         predictors1 <- paste(variableinput(),"favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR",sep = "+")
                         model1 <- as.formula(paste0("rating", "~", predictors1))

                         multiple <- train(model1, data = train.sub, 
                                           method = "lm", preProcess =c("center", "scale"), 
                                           trControl = trainControl(method = "cv", number = as.numeric(CVinput())))
                         summary(multiple)
                       }
                       else{print("Multiple Linear Regression not selected")}
                     }
                     else if(variableinput() == "all included"){
                       if(modelinput() == "All models" | modelinput() =="multiple linear regression"){
                         predictors1 <- paste("favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","popularity_rank","year","total_length",sep = "+")
                         model1 <- as.formula(paste0("rating", "~", predictors1))
                         multiple <- train(model1, data = train.sub, 
                                           method = "lm", preProcess =c("center", "scale"), 
                                           trControl = trainControl(method = "cv", number = as.numeric(CVinput())))
                         summary(multiple)
                       }
                       else{print("Multiple Linear Regression not selected")}}
                      
    })
        
      
    })
   
       
      
    observeEvent(input$press,{
        output$Tree <- renderPrint({
          train.index.sub <- createDataPartition(y = final$rating, p = as.numeric(splitinput()), list = F)
          train.sub <- final[train.index.sub, ] # training set
          test.sub <- final[-train.index.sub, ] # test set

          if(variableinput() != "all included"){
            if(modelinput() == "All models" | modelinput() =="regression tree"){
            predictors2 <- paste(variableinput(),"favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG",sep = "+")
            model2 <- as.formula(paste0("rating", "~", predictors2))
            Tree <- train(model2, data = train.sub, 
                          method = 'rpart1SE',
                          preProcess = c("center","scale"),
                          trControl = trainControl(method = "cv",number = as.numeric(CVinput())))
            summary(Tree)
            }
            else{print("Regression Tree not selected")}
          }
         if(variableinput() == "all included"){
              if(modelinput() == "All models" | modelinput() =="regression tree"){
                predictors2 <- paste("favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG","popularity_rank","year","total_length",sep = "+")
                model2 <- as.formula(paste0("rating", "~", predictors2))
                Tree <- train(model2, data = train.sub, 
                              method = 'rpart1SE',
                              preProcess = c("center","scale"),
                              trControl = trainControl(method = "cv",number = as.numeric(CVinput())))
                summary(Tree)}
              else{print("Regression Tree not selected")}
        }
      })
    })
    observeEvent(input$press,{   
      output$Random <- renderPrint({
          train.index.sub <- createDataPartition(y = final$rating, p = as.numeric(splitinput()), list = F)
          train.sub <- final[train.index.sub, ] # training set
          test.sub <- final[-train.index.sub, ] # test set
          if(variableinput() != "all included"){
            if(modelinput() == "All models" | modelinput() =="random forest regression"){
              predictors2 <- paste(input$variable,"favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG",sep = "+")
              model2 <- as.formula(paste0("rating", "~", predictors2))
             
                  RF <-  train(model2, data = train.sub, 
                                      method = "rf",
                                      preProcess = c("center","scale"),
                                      trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
                  random <- randomForest(model2, data =train.sub, mtry = RF$bestTune$mtry )
                  print(RF)
            }
            else{print("Random Forest Regression not selected")}
          }
          if(variableinput() == "all included"){
            if(modelinput() == "All models" | modelinput() =="random forest regression"){
              predictors2 <- paste("favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG","popularity_rank","year","total_length",sep = "+")
              model2 <- as.formula(paste0("rating", "~", predictors2))
              RF <-  train(model2, data = train.sub, 
                           method = "rf",
                           preProcess = c("center","scale"),
                           trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
              random <- randomForest(model2, data =train.sub, mtry = RF$bestTune$mtry )
              print(RF)
              }
            else{print("Random Forest Regression not selected")}
          }
      })
    })
    
    observeEvent(input$press,{
      output$Random2 <- renderPlot({
        train.index.sub <- createDataPartition(y = final$rating, p = as.numeric(splitinput()), list = F)
        train.sub <- final[train.index.sub, ] # training set
        test.sub <- final[-train.index.sub, ] # test set
      
        if(variableinput() != "all included"){
          if(modelinput() == "All models" | modelinput() =="random forest regression"){
            predictors2 <- paste(input$variable,"favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG",sep = "+")
            model2 <- as.formula(paste0("rating", "~", predictors2))
            
            RF <-  train(model2, data = train.sub, 
                         method = "rf",
                         preProcess = c("center","scale"),
                         
                         trControl = trainControl(method = "cv",number =as.numeric(input$CV)))
            random <- randomForest(model2, data =train.sub, mtry = RF$bestTune$mtry )
            varImpPlot(random)
          }
          else{print("Null")}
        }
        if(variableinput()== "all included"){
          if(modelinput() == "All models" | modelinput()=="random forest regression"){
            predictors2 <- paste("favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG","popularity_rank","year","total_length",sep = "+")
            model2 <- as.formula(paste0("rating", "~", predictors2))
            RF <-  train(model2, data = train.sub, 
                         method = "rf",
                         preProcess = c("center","scale"),
                         
                         trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
            random <- randomForest(model2, data =train.sub, mtry = RF$bestTune$mtry )
            varImpPlot(random)
          }
          else{print("Null")}
        }
      })
    })
      
      
      
    observeEvent(input$press,{  
      output$Ridge <- renderPrint({
        train.index.sub <- createDataPartition(y = final$rating, p = as.numeric(splitinput()), list = F)
        train.sub <- final[train.index.sub, ] # training set
        test.sub <- final[-train.index.sub, ] # test set
      
        if(variableinput() != "all included"){
          if(modelinput() == "All models" | modelinput() =="ridge regression"){
            predictors2 <- paste(input$variable,"favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG",sep = "+")
            model2 <- as.formula(paste0("rating", "~", predictors2))
            ridge <-  train(model2, data = train.sub, 
                         method = "ridge",
                         preProcess = c("center","scale"),
                         trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
            print(ridge)
          }
          else{print("Ridge Regression not selected")}
        }
        if(variableinput() == "all included"){
          if(modelinput() == "All models" | modelinput() =="ridge regression"){
            predictors2 <- paste("favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG","popularity_rank","year","total_length",sep = "+")
            model2 <- as.formula(paste0("rating", "~", predictors2))
            ridge <-  train(model2, data = train.sub, 
                            method = "ridge",
                            preProcess = c("center","scale"),
                            trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
            print(ridge)
          }
          else{print("Ridge Regression not selected")}
        }
      })
    }) 
    
    observeEvent(input$press,{  
      output$AllStats <- renderDataTable({
        train.index.sub <- createDataPartition(y = final$rating, p = as.numeric(splitinput()), list = F)
        train.sub <- final[train.index.sub, ] # training set
        test.sub <- final[-train.index.sub, ] # test set
      
        if(variableinput() != "all included"){
          if(modelinput() == "All models"){
            predictors2 <- paste(input$variable,"favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG",sep = "+")
            model2 <- as.formula(paste0("rating", "~", predictors2))
            
            multiple <- train(model2, data = train.sub, 
                              method = "lm", preProcess =c("center", "scale"), 
                              trControl = trainControl(method = "cv", number = as.numeric(CVinput())))
            
            Tree <- train(model2, data = train.sub, 
                          method = 'rpart1SE',
                          preProcess = c("center","scale"),
                          trControl = trainControl(method = "cv",number = as.numeric(CVinput())))
            RF <-  train(model2, data = train.sub, 
                         method = "rf",
                         preProcess = c("center","scale"),
                         trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
            ridge <-  train(model2, data = train.sub, 
                            method = "ridge",
                            
                            preProcess = c("center","scale"),
                            trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
            pred.lm <- predict(multiple  , newdata = test.sub)
            test.RMSE.lm <- RMSE(pred.lm, test.sub$rating)
            pred.tree <- predict(Tree  , newdata = test.sub)
            test.RMSE.tree <- RMSE(pred.tree, test.sub$rating)
            pred.random <- predict(RF  , newdata = test.sub)
            test.RMSE.RF <- RMSE(pred.random, test.sub$rating)
            pred.ridge <- predict(ridge  , newdata = test.sub)
            test.RMSE.ridge <- RMSE(pred.ridge, test.sub$rating)
            
            data.frame(models=c("Multiple Linear Regression","Regression Tree","Random Forest Regression","Ridge Regression"),
                       test_RMSE = c(test.RMSE.lm,test.RMSE.tree,test.RMSE.RF,test.RMSE.ridge))
            
          }
        
        }
       if(variableinput() == "all included"){
          if(modelinput() == "All models"){
            predictors2 <- paste("favorite_count","month","user_count","total_episode","age_ratingPG","age_ratingR","age_ratingG","popularity_rank","year","total_length",sep = "+")
            model2 <- as.formula(paste0("rating", "~", predictors2))
            
            multiple <- train(model2, data = train.sub, 
                              method = "lm", preProcess =c("center", "scale"), 
                              trControl = trainControl(method = "cv", number = as.numeric(CVinput())))
            
            Tree <- train(model2, data = train.sub, 
                          method = 'rpart1SE',
                          preProcess = c("center","scale"),
                          trControl = trainControl(method = "cv",number = as.numeric(CVinput())))
            RF <-  train(model2, data = train.sub, 
                         method = "rf",
                         preProcess = c("center","scale"),
                         
                         trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
            ridge <-  train(model2, data = train.sub, 
                            method = "ridge",
                            preProcess = c("center","scale"),
                            trControl = trainControl(method = "cv",number =as.numeric(CVinput())))
            pred.lm <- predict(multiple  , newdata = test.sub)
            test.RMSE.lm <- RMSE(pred.lm, test.sub$rating)
            pred.tree <- predict(Tree  , newdata = test.sub)
            test.RMSE.tree <- RMSE(pred.tree, test.sub$rating)
            pred.random <- predict(RF  , newdata = test.sub)
            test.RMSE.RF <- RMSE(pred.random, test.sub$rating)
            pred.ridge <- predict(ridge  , newdata = test.sub)
            test.RMSE.ridge <- RMSE(pred.ridge, test.sub$rating)
            
            data.frame(models=c("Multiple Linear Regression","Regression Tree","Random Forest Regression","Ridge Regression"),
                       test_RMSE = c(test.RMSE.lm,test.RMSE.tree,test.RMSE.RF,test.RMSE.ridge))
          }
          
        }
      })
    })
    
    
    
    fav <- eventReactive(input$pred,{input$fcount})
    user <- eventReactive(input$pred,{input$ucount})
    year <- eventReactive(input$pred,{input$year4})
    month <- eventReactive(input$pred,{input$month4})
    pop <- eventReactive(input$pred,{input$pop})
    Ep <- eventReactive(input$pred,{input$Lep})
    NEp <- eventReactive(input$pred,{input$Nep})
    age <- eventReactive(input$pred,{input$age_guide})
    Model <- eventReactive(input$pred,{input$M})
    
                  
      
  observeEvent(input$pred,{  
    output$values <- renderDataTable({
      train.index.sub <- createDataPartition(y = final$rating, p = 0.7, list = F)
      train.sub <- final[train.index.sub, ] # training set
      test.sub <- final[-train.index.sub, ] # test set
      
      multiple.linear.regression <- train(rating~., data = train.sub, 
                        method = "lm", preProcess =c("center", "scale"), 
                        trControl = trainControl(method = "cv", number = 5))
      
      regression.tree <- train(rating~., data = train.sub, 
                    method = 'rpart1SE',
                    preProcess = c("center","scale"),
                    trControl = trainControl(method = "cv",number = 5))
      ridge.regression <-  train(rating~., data = train.sub, 
                      method = "ridge",
                      preProcess = c("center","scale"),
                      trControl = trainControl(method = "cv",number =5))
      
      p <- data.frame(favorite_count = fav(),user_count = user(),year = year(),
                 month = month(),popularity_rank = pop(),total_episode = NEp(),
                 total_length = Ep(), age_ratingPG = ifelse(age()=="age_ratingPG",1,0), age_ratingR = ifelse(age()=="age_ratingR",1,0),age_ratingG = 0)
      if(Model()=="multiple.linear.regression"){
        data.frame(p,predict(multiple.linear.regression,newdata = p))
      }
      else if(Model()=="regression.tree"){
        data.frame(p,predict(regression.tree,newdata = p))
      }
      else if(Model()=="ridge.regression"){
        data.frame(p,predict(ridge.regression ,newdata = p))
      }
      
      
      
    })
  })
    

  
  
  
  
  
      
})




























