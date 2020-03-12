#Memory 
memory.limit()
memory.limit(size=56000)

# Data Loading
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
install.packages("splitstackshape")
install.packages("DT")
install.packages("lubridate")
# Libraries
library(tidyverse)
library(caret)
library(data.table)
library(splitstackshape)
library(DT)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Data Analysis & Exploration
summary(edx)
# Quantitative: Rating analysis
# Table Showing the Rating Count
head(sort(table(edx$rating)),10)
#Plot Histogram (Graphical Representation)
ggplot(edx, aes(x= edx$rating)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
    labs(x="Rating", y="No. of ratings", caption = "Source Data: edx set") +
  ggtitle("Histogram : Ratings Tally")

#Quantitative:MovieId Vs Ratings Analysis
#Plot Histogram (Graphical Representation)
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(movieId)) + geom_histogram(bins = 10) +
  labs(x="MovieID", y="No. of ratings", caption = "Source Data: edx set") +
  ggtitle("Number of Movies Ratings")
#Quantitative:UserId Vs Ratings Analysis
#Plot Histogram (Graphical Representation)
edx %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(userId)) + geom_histogram(bins = 10) +
  labs(x="UserID", y="No. of ratings", caption = "Source Data: edx set") +
  ggtitle("Number of User Ratings")

# Qualitative:Genres vs Ratings Analysis
# Split the Genres
edx_Splitted <- cSplit(edx, "genres", sep = "|" ,  direction = "long")

#Rating Count per Genre
edx_Genre_rating <- edx_Splitted %>% 
  group_by(genres) %>%
  summarize(RatingCount = n()) %>%
  arrange(desc(RatingCount))

#Tabular Representation
datatable(edx_Genre_rating, rownames = FALSE, filter="top", options = list(pageLength = 50, scrollX=T) ) %>%
  formatRound('RatingCount',digits=0, interval = 3, mark = ",")

#Graphical representation (Point Chart)
ggplot(edx_Genre_rating, aes(x= genres, y=RatingCount)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  scale_y_continuous(trans = "log2")+
  ggtitle("Genre - Ratings Point Chart")


# Qualitative: Movies Title vs Ratings
# Movies along with the corresponding Rating Counts
edx_Movie_Ratings <- edx %>% 
  group_by(title, genres) %>%
  summarize(RatingCount = n()) %>%
  arrange(desc(RatingCount))
#Tabular Representation
datatable(edx_Movie_Ratings, rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T) ) %>%
  formatRound('RatingCount',digits=0, interval = 3, mark = ",")

#Graphical representation (Bar Chart) for top 10 Movies
edx %>% group_by(title) %>% summarise(count = n()) %>% top_n(10,count) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x=reorder(title, count), y=count)) + coord_flip(y=c(0, 40000)) +
  geom_bar(stat='identity', fill="purple") + 
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title=" Top 10 Movies \n on number on ratings" , caption = "source data: edx set")

# Movie Age vs Rating Analysis

#Determine the Age of the Movie
#Extract The Premier Date from Movie Title
# Regex can also be used
PremierYear <- as.numeric(substr(as.character(edx$title),nchar(as.character(edx$title))-4,nchar(as.character(edx$title))-1))

#Include RatingYear and PremierYear
edx_Movie_Aging_Details <- edx %>% mutate(Rated_Year = year(as_datetime(timestamp)), Premier_Year = PremierYear) %>% select(-timestamp)
head(edx_Movie_Aging_Details)
#Validate the Premier Year
edx_Movie_Aging_Details %>% filter(Premier_Year < 1900 || Premier_Year > 2018) %>% group_by(movieId, title, Premier_Year,Rated_Year) %>% summarize(n = n())


#  Calculate Movie Age and Average Rating
edx_Movie_Aging_Details_Avg <- edx_Movie_Aging_Details %>% 
  mutate(Movie_age = 2018 - Premier_Year) %>% group_by(Movie_age) %>% 
  summarize(avg_rating_by_age = mean(rating))%>% arrange(desc(Movie_age))
head(edx_Movie_Aging_Details_Avg)

#Graphical Representation : Age of movie vs average movie rating
edx_Movie_Aging_Details_Avg %>%
  ggplot(aes(Movie_age, avg_rating_by_age)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  ggtitle("Movie Age vs Average Movie Rating")

#Graphical Representation : Premier Year vs average movie rating
edx_avg_ratings <- edx_Movie_Aging_Details %>% group_by(Premier_Year) %>% summarise(avg_rating_by_age = mean(rating))
edx_avg_ratings %>% ggplot(aes(Premier_Year, avg_rating_by_age)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  ggtitle("Premier Year vs Average Movie Rating")


#RMSE Calculation
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Choose Lambda Values for tuning
lambdas <- seq(0,5,.5)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_Movie_Aging_Details$rating)
  
  b_i <- edx_Movie_Aging_Details %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- edx_Movie_Aging_Details %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() +l))
  
  predicted_ratings <- edx_Movie_Aging_Details %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
  
  return(RMSE(predicted_ratings, edx_Movie_Aging_Details$rating))
})

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
paste('Optimal RMSE of',min(rmses),'is achieved with Lambda',lambda)
# Predicting the validation set

mu <- mean(validation$rating)
l <- lambda
b_i <- validation %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + l))

b_u <- validation %>%
  left_join(b_i, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() +l))

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE(predicted_ratings, validation$rating)
