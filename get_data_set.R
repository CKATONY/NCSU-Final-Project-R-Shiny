


library(httr)
library(jsonlite)
library(tidyverse)
url <- "https://kitsu.io/api/edge"
response <- VERB("GET", url, add_headers(Accept = "application/vnd.api+json", content_type = "application/vnd.api+json"))

Z <- function(offset){
  all <- paste0("https://kitsu.io/api/edge/anime?page[limit]=20&page[offset]=",offset)
  K = GET(all)
  Y2 <- content(K,"text")
  getdata_json2<- fromJSON(Y2)
  berry_lookup2 <- as.data.frame(getdata_json2$data)
  return(berry_lookup2)
}

df1 <- Z(0)  
for(i in seq.int(20,17000,20)){
  df1<- bind_rows(df1,Z(i))
}



K = GET("https://kitsu.io/api/edge/anime/10")
Y3 <- content(K,"text")
B<- fromJSON(Y3)
berry_lookup2 <- as.data.frame(B$data$relationships)

# get the genres from the website 
L <-GET(berry_lookup2$genres.links.related)
H <- content(L,"text")
getdata <- fromJSON(H)
str_c(getdata$data$attributes$name,collapse = "/")
# get the categories from API 



Q <-berry_lookup2$categories.links.related %>% GET() %>% content("text",encoding = NULL) %>% fromJSON()
# berry_lookup2$categories.links.self %>% GET() %>% content("text",encoding = NULL) %>% fromJSON()
Q$data$attributes$title
paste0(Q$data$attributes$title,collapse = "/")




#df1$attributes$
  
  #C <- df1 %>% filter(attributes$startDate > "2010-01-01")
  
  #Z <- as.Date(df1$attributes$startDate)
  
  #df1$attributes$startDate <-Z
  F <- df1[df1$attributes$startDate > "2010-01-01",]
  V <- separate(F,attributes.startDate,c("year","month","day"),sep = "-",convert = TRUE,remove = FALSE)

Z <- tibble(number = F$id ,
            anime_name_english = F$attributes$slug ,
            anime_name_japanese = F$attributes$titles$ja_jp,
            rating = F$attributes$averageRating ,
            favorite_count = F$attributes$favoritesCount ,
            user_count = F$attributes$userCount,
            airing_start = F$attributes$startDate,
            airing_end = F$attributes$endDate,
            popularity_rank = F$attributes$popularityRank,
            rating_rank = F$attributes$ratingRank,
            age_rating = F$attributes$ageRating,
            age_rating_Guide = F$attributes$ageRatingGuide,
            categories = F$relationships$categories$links$related,
            genres = F$relationships$genres$links$related,
            total_episode = F$attributes$episodeCount,
            total_length = F$attributes$totalLength
)
anyNA(Z)

write_excel_csv(Z,"C:\\Users\\CKA\\Desktop\\guitar\\final_anime_from_Kitsu_API.csv",delim = " ")
S <- read_delim("C:/Users/CKA/Desktop/guitar/final_anime_from_Kitsu_API.csv",delim = " ")

#P <- S %>% arrange(-desc(airing_start))

C <- S[order(S$airing_start,decreasing = FALSE),]
V <- separate(C,airing_start,c("year","month","day"),sep = "-",convert = TRUE,remove = FALSE)

#Final <- V  %>% group_by(year)%>% arrange(desc(rating))




B <-V[order(V$year,V$rating,decreasing = TRUE),]
#dataset of final top 50 anime for each year 


G<- B %>% group_by(year) %>% top_n(300,rating)





# get genres and categories 
get_genres_and_categories <- function(i){
  R <- G[i,]
  getdata <-  R$genres %>%
    GET() %>% content("text",encoding = "UTF-8") %>% fromJSON() 
  genres <- paste0(getdata$data$attributes$name,collapse = "/")
  
  getdata2 <- R$categories %>%
    GET() %>% content("text",encoding = "UTF-8") %>% fromJSON() 
  categories <- paste0(getdata2$data$attributes$title,collapse = "/")
  
  final <- tibble(
    Genres = genres,
    Categories = categories
  )
  return(final)
}

get_genres_and_categories(1)

#lapply(F$id,get_genres_and_categories(),10)




df2 <- get_genres_and_categories(1)
for(j in 2:nrow(G)){
  df2<- bind_rows(df2,get_genres_and_categories(j))
}

#or any method that is faster 

binded <- cbind(G,df2)
final_data <- binded %>% select(-c(categories,genres))

write_excel_csv(final_data,"C:\\Users\\CKA\\Desktop\\guitar\\anime_FinalInfo_from_Kitsu_API.csv",delim = " ")

want <- read_delim("C:/Users/CKA/Desktop/guitar/anime_FinalInfo_from_Kitsu_API.csv",delim = " ")

G %>% group_by(year) %>% summarize(mean_rating = mean(rating),mean_user = mean(user_count),mean_favorite = mean(favorite_count))





























