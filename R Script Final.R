Movies <- read.csv(file = "Movies.csv", header = T)
Ratings <-  read.csv(file = "Ratings.csv", header = T)
Tags <- read.csv(file = "Tags.csv", header = T)

Tags <- Tags %>%
  group_by_all()%>%
  summarise(tag_date = as_datetime(timestamp), .groups = "keep")
  
head(Tags,3)

Ratings <- Ratings %>%
  group_by_all()%>%
  summarise(rating_date = as_datetime(timestamp), .groups = "keep")

head(Ratings,3)



rateandtag <- semi_join(Tags, Ratings, by = c("userId","movieId"), copy = TRUE)
  
  
tagandrate <- left_join(Tags, Ratings, by = c("userId"="userId","movieId"="movieId"))

moviejoin <- full_join(tagandrate, Movies, by = "movieId")%>%
  na.omit%>%
  ungroup()%>%
  select(userId, rating, rating_date, tag, tag_date, title, genres)
  setorder(method = c("userId", "rating", "rating_date", "tag", "tag_date", "title", "genres"))
  
  
releaseyear <- moviejoin%>%
  mutate(release_year = str_extract(string = title, pattern = "\\((\\d+)\\).*"))


topgenres <- Ratings %>%
  full_join(Ratings, Movies, by = "userId")
  group_by(genres)%>%
  summarise(number_rows = n())%>%
  arrange(desc(number_rows))%>%
  head(n=10L)

releaseyear %>%
  group_by(genres)%>%
  summarise(number_rows = n())%>%
  arrange(desc(number_rows))%>%
  head(n=10L)%>%
  ggplot(aes(x= genres, y  = number_rows, fill = genres))+
  geom_bar(stat = "identity")+
  xlab("Genre")+
  ylab("Amount of Times Rated")+
  labs(title = "Top 10 Genres Rates Most Frequently")+
  theme(axis.text.x = element_text(angle = 90))+
  axis(side =1, at=seq(0, 300, by=50))

ratedgenres <- Movies %>%
  semi_join(Movies, Ratings, by = "movieId", "rate")
head(ratedgenres,2)

ratedgenres%>%
  group_by(genres)%>%
  summarise(number_rows = n())%>%
  arrange(desc(number_rows))%>%
  head(n=10L)

ratedgenres%>%
  group_by(genres)%>%
  summarise(number_rows = n())%>%
  arrange(desc(number_rows))%>%
  head(n=10L)%>%
  ggplot(aes(x= genres, y  = number_rows, fill = genres))+
  geom_bar(stat = "identity")+
  xlab("Genre")+
  ylab("Amount of Times Rated")+
  labs(title = "Top 10 Genres Rates Most Frequently")+
  theme(axis.text.x = element_text(angle = 90))
  axis(side =1, at=seq(0, 300, by=50))# tried to scale the axis but could not get it to work
  
  




### another question I was looking at

latestmovies<- releaseyear%>%
  filter(release_year>"(2000)")%>%
  summarise(release_year = n())

mostpoptags<-releaseyear%>%
  group_by(tag)%>%
  summarise(populartags = n())%>%
  arrange(desc(populartags))%>%
  head(n=5L)
  
  
  
releaseyear%>%
  group_by(tag)%>%
  summarise(populartags = n())%>%
  arrange(desc(populartags))%>%
  head(n=5L)%>%
  ggplot(aes(x= tag, y  = populartags, fill = tag))+
  geom_bar(stat = "identity")+
  xlab("Tag used")+
  ylab("Amount of times specific tag is used")+
  labs(title = "Top 5 most utilized tags")
  

  


  
  
  
  


  


  



  



  



  
  
 



  
  


                         