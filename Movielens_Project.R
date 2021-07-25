##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
# Clear environment
rm(list = ls())
# Load library and install package if needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if (!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],title = as.character(title),genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

# Extract "year" from "title" in edx and validation set
pattern <- "\\((\\d{4})\\)$"
edx <- edx %>% mutate(year=str_extract_all(edx$title, pattern,simplify=FALSE))
edx <- edx %>% mutate(title=str_remove_all(edx$title,pattern))

validation <- validation %>% mutate(year=str_extract_all(validation$title, pattern,simplify=FALSE))
validation <- validation %>% mutate(title=str_remove_all(validation$title,pattern))

validation$year <- str_remove_all(validation$year,"[(]")
validation$year <- str_remove_all(validation$year,"[)]")

edx$year <- str_remove_all(edx$year,"[(]")
edx$year <- str_remove_all(edx$year,"[)]")
# Change class of Year from character to numeric
validation$year <- as.numeric(validation$year)
edx$year <- as.numeric(edx$year)
# Check new variable "year" added
head(edx)
head(validation)

#load file from computer
load("C:/Users/haung/Desktop/Hau/Documents/Data Science - R Basics/Data/edx.RData")
load("C:/Users/haung/Desktop/Hau/Documents/Data Science - R Basics/Data/validation.RData")

#--------------------------2. Data preparation----------------------------------

#2.1. Data Description
# Summary of character variables
Character_variables <- c("title","genres")
Number_of_categories <- c(n_distinct(edx$title), n_distinct(edx$genres))
tab1 <- data.frame(Character_variables,Number_of_categories)
kbl(tab1, booktabs = T, caption = "Summary of character variables") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))
# Summary of numeric variables
Numeric_variables <- c("userId","movieId","rating","timestamp","year")
Min <- c(min(edx$userId),min(edx$movieId),min(edx$rating),
         min(edx$timestamp),min(edx$year))
Median <- c(median(edx$userId),median(edx$movieId),median(edx$rating),
            median(edx$timestamp),median(edx$year))
Mean <- c(mean(edx$userId),mean(edx$movieId),mean(edx$rating),
          mean(edx$timestamp),mean(edx$year))
Max <- c(max(edx$userId),max(edx$movieId),max(edx$rating),
         max(edx$timestamp),max(edx$year))
NA_number <- c(sum(is.na(edx$userId)),sum(is.na(edx$movieId)),
                  sum(is.na(edx$rating)),sum(is.na(edx$timestamp)),
                  sum(is.na(edx$year)))
tab2 <- data.frame(Numeric_variables,Min,Median,Mean,Max,NA_number)
kbl(tab2, booktabs = T,digits = 1,caption = "Summary of numeric variables") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))
#N/A values: No

#2.2 Data Cleaning
boxplot(edx$rating, xlab="Rating", col="blue")
boxplot(edx$year, xlab="Year", col="red")
#it seems to be there is no outlier

#--------------------3. Data exploration and visualization----------------------
#3.1. Number of rating count
# How many different movies
n_distinct(edx$movieId)

# Range of number of rating count per movie
rating_count <- edx %>% select(movieId,rating) %>%
  group_by(movieId) %>% summarize(rating_count = n())
range(rating_count)

# Histogram of number of rating count
rating_count %>%
  ggplot(aes(x=rating_count)) +
  geom_histogram(fill="blue",binwidth = 50) +
  scale_x_continuous(breaks=seq(0, 3000, by=500)) +
  coord_cartesian(x=c(0, 3000)) +
  labs(x="number of rating count", y="number of movies") +
  ggtitle("Histogram of rating count")

# Top movies with highest number of rating count
top_10_rating <- edx %>% select(movieId,title) %>%
  group_by(title) %>% summarize(rating_count = n()) %>%
  top_n(10,rating_count)
top_10_rating
ggplot(top_10_rating, aes(x=reorder(title, rating_count), y=rating_count)) +
  geom_bar(stat='identity', fill="blue") + 
  coord_flip() +
  labs(x="", y="Number of rating count") +
  geom_text(aes(label=rating_count), hjust=-0.1, size=3) +
  ggtitle("Top 10 movies with highest rating count")

#3.2. Average Rating
# Histogram of average rating
rating_avg <- edx %>% select(movieId,rating) %>%
  group_by(movieId) %>% summarize(rating_avg = mean(rating))
rating_avg %>%
  ggplot(aes(x=rating_avg)) +
  geom_histogram(fill="blue",binwidth=0.2) +
  labs(x="Average rating", y="number of movies") +
  ggtitle("Histogram of average rating")

# Top movies with highest average rating
top_10_rating_avg <- edx %>% select(rating,title) %>%
  group_by(title) %>% 
  summarize(rating_count = n(), rating_avg = mean(rating)) %>%
  top_n(10,rating_avg) %>%
  arrange(desc(rating_avg))
kbl(top_10_rating_avg, booktabs = T, 
    caption="The top 10 movies with highest average rating") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

#3.3. Genres
# Number of movies by genre
# Separate genres and number of movies by genre
cat("\nNumber of movies by each genre :\n")
index <- createDataPartition(y=edx$rating, times=1,p=0.1, list=FALSE)
edx1 <- edx[index,]
edx1$genres <- str_split(edx1$genres, pattern="\\|")
separate_genres <- enframe(edx1$genres) %>%
  unnest(value) %>%
  mutate(temp = 1) %>%
  pivot_wider(names_from = value, values_from = temp, values_fill = list(temp = 0))
edx1 <- cbind(edx1, separate_genres) %>% select(-name)
not_genres <- c("userId", "movieId", "rating", "title", "year","timestamp","genres")
genres_name <- colnames(edx1)[!colnames(edx1) %in% not_genres]
#names of each genre
genres_name

genres_number <- sapply(genres_name, function(g) {
  sum(str_detect(edx$genres, g))
})
genres_each <- data.frame(genres_name,genres_number)
genres_each
ggplot(genres_each, aes(x=reorder(genres_name,genres_number), y=genres_number)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  coord_flip() + 
  xlab("") +
  ylab("Number of each genre") +
  ggtitle("Total number of movie by genre") +
  geom_text(aes(label=genres_number), hjust=-0.1, size=3)


# Top highest rating count by combination of genres
#number of combination of genres
n_distinct(edx$genres)

# Top highest rating count by combination of genres
top_10_genres <- edx %>% select(movieId,genres) %>%
  group_by(genres) %>% summarize(genres_count = n()) %>% 
  top_n(10,genres_count) %>% arrange(desc(genres_count))  
kbl(top_10_genres, booktabs = T, 
    caption="The top 10 highest rating count by combination of genres") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Top highest average rating by combination of genres
top_genre_rating_avg <- edx %>% select(rating,genres) %>%
  group_by(genres) %>% summarize(rating_avg = mean(rating)) %>% 
  top_n(10,rating_avg) %>% arrange(desc(rating_avg))  
top_genre_rating_avg
ggplot(top_genre_rating_avg, aes(x=reorder(genres, rating_avg), y=rating_avg)) +
  geom_bar(stat='identity', fill="blue") + 
  coord_flip() +
  labs(x="", y="Average rating") +
  geom_text(aes(label=round(rating_avg, digits = 2)), hjust=-0.1, size=3) +
  ggtitle("Top highest average rating by combination of genres")

#3.4. Release Year
# Number of movies by year
# Number of movies each year
movie_year <- edx %>% select(year,movieId) %>% group_by(year) %>% 
  summarise(count=n()) %>% arrange(year)
# Plot year and number of movies
movie_year %>% ggplot(aes(year,count)) + geom_line(color="red") +
  ggtitle("Number of movies each year")

# Top highest number of movies by year
movie_year_10 <- edx %>% select(year,movieId) %>%
  group_by(year) %>% summarize(count = n()) %>% 
  top_n(10,count) %>% arrange(desc(count))  
kbl(movie_year_10, booktabs = T,
    caption ="The top 10 highest number of movies by year") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# Top 10 year with highest average rating
# Average rating each year and top 10 years with highest average rating
year_rating_avg <- edx %>% select(rating,year) %>% group_by(year) %>% 
  summarise(rating_avg=mean(rating)) %>% 
  top_n(10,rating_avg) %>% arrange(desc(rating_avg)) 
kbl(year_rating_avg, booktabs = T,
    caption="The top 10 years with highest average rating") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

#----------------------------4. Methodology-------------------------------------
# 4.2. Selecting The Best Model
# Calculate RMSE
test_index <- createDataPartition(y=edx$rating, times=1,p=0.1, list=FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure movieId, userId in test_set are also in train_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") 

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# Model 1: rating ~ as.factor(genres)
lambda <- seq(0,10,1)

rmse1 <- sapply(lambda,function(l){
  
  mu <- mean(train_set$rating) 
  
  b_g <- train_set %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu)/(n()+l))
  
  y_hat <- test_set %>% 
    left_join(b_g, by='genres') %>%
    mutate(y_hat = mu + b_g) %>%
    pull(y_hat)
  
  sqrt(mean((y_hat - test_set$rating)^2))
})

result1 <- data_frame(Method = "Genre effect model",
                      Optimal_lambda = lambda[which.min(rmse1)],
                      RMSE = min(rmse1))

# Model 2: rating ~ as.factor(genres) + year

rmse2 <- sapply(lambda,function(l){
  
  mu <- mean(train_set$rating) 
  
  b_g <- train_set %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_g, by='genres') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_g)/(n()+l))
  
  y_hat <- test_set %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    mutate(y_hat = mu + b_g + b_y) %>%
    pull(y_hat)
  
  sqrt(mean((y_hat - test_set$rating)^2))
})

result2 <- data_frame(Method = "Genre + year effect model",
                      Optimal_lambda = lambda[which.min(rmse2)],
                      RMSE = min(rmse2))

# Model 3: rating ~ as.factor(genres) + year + movieId

rmse3 <- sapply(lambda,function(l){
  
  mu <- mean(train_set$rating) 
  
  b_g <- train_set %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_g, by='genres') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_g)/(n()+l))
  
  b_m <- train_set %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu - b_g - b_y)/(n()+l))
  
  y_hat <- test_set %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    left_join(b_m, by='movieId') %>%
    mutate(y_hat = mu + b_g + b_y + b_m) %>%
    pull(y_hat)
  
  sqrt(mean((y_hat - test_set$rating)^2))
})

result3 <- data_frame(Method = "Genre + year + movieId effect model",
                      Optimal_lambda = lambda[which.min(rmse3)],
                      RMSE = min(rmse3))

# Model 4: rating ~ as.factor(genres) + year + movieId + userId

rmse4 <- sapply(lambda,function(l){
  
  mu <- mean(train_set$rating) 
  
  b_g <- train_set %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_g, by='genres') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_g)/(n()+l))
  
  b_m <- train_set %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu - b_g - b_y)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    left_join(b_m, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_g - b_y - b_m)/(n()+l))
  
  y_hat <- test_set %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(y_hat = mu + b_g + b_y + b_m + b_u) %>%
    pull(y_hat)
  
  sqrt(mean((y_hat - test_set$rating)^2))
})

result4 <- data_frame(Method = "Genre + year + movieId + userId effect model",
                      Optimal_lambda = lambda[which.min(rmse4)],
                      RMSE = min(rmse4))

# Combined result table
result <- bind_rows(result1,result2,result3,result4)
kbl(result, booktabs = T, caption ="Combined result of all models") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))

# 4.3. Final Result on Validation Set
# retrain optimal model on edx data set, then calculate RMSE on validation set
rmse <- sapply(lambda,function(l){
  
  mu <- mean(edx$rating) 
  
  b_g <- edx %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu)/(n()+l))
  
  b_y <- edx %>% 
    left_join(b_g, by='genres') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_g)/(n()+l))
  
  b_m <- edx %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu - b_g - b_y)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    left_join(b_m, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_g - b_y - b_m)/(n()+l))
  
  y_hat <- validation %>% 
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='year') %>%
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(y_hat = mu + b_g + b_y + b_m + b_u) %>%
    pull(y_hat)
  
  sqrt(mean((y_hat - validation$rating)^2))
})
# Plot lambda and rmse of validation set
qplot(lambda,rmse)

# Final result table on validation set
result_final <- data_frame(Method = "Genre + year + movieId + userId effect model",
                      Optimal_lambda = lambda[which.min(rmse)],
                      RMSE = min(rmse))

kbl(result_final, booktabs = T, 
    caption ="Final RMSE result on validation set") %>% 
  kable_styling(latex_options = c("striped", "hold_position"))
