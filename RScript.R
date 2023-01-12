##Fernando Gripe
##HarvardX PH125.9x Data Science: Capstone
##Movie recommendation system using the MovieLens dataset
##dataset source: https://grouplens.org/datasets/movielens/10m/


##########################################################
# Dataset
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(ggplot2)
library(dplyr)
library(stringi)
library(lubridate)
library(tinytex)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10m.zip"

if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"

if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"

if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)

colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)

colnames(movies) <- c("movieId", "title", "genres")

movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

#additional columns: rating_year, movie_year, movie_age
movielens <- mutate(movielens,
                    movieId = as.numeric(movieId),
                    title = as.character(title),
                    genres = as.character(genres),
                    rating_year= year(as_datetime(timestamp)),
                    movie_year = as.numeric(stri_extract_last(title, regex = "(\\d{4})", comments = TRUE)) %>% as.numeric(),
                    movie_age = 2022 - movie_year) %>%
            select(-timestamp)

#checking if regex worked
movielens %>% filter(movie_year < 1900 || movie_year > 2018) %>% 
  group_by(movie_year) %>% 
  summarize(n = n())

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


##########################################################
#Discovery and Analysis
##########################################################


#head of dataset
head(edx)

#Display the Structure of the dataset
str(edx)


#Data Sumary
summary(edx)

#Data Sumary
data.frame(n_of_rows = nrow(edx),
                          n_of_column = ncol(edx),
                          n_of_users = n_distinct(edx$userId),
                          n_of_movies = n_distinct(edx$movieId),
                          avg_rating = round(mean(edx$rating),2),
                          zero_rating = sum(edx$rating == 0),
                          three_rating = sum(edx$rating == 3),
                          number_of_genres = n_distinct(edx$genres))


##all user ratings to a single movie
#edx %>% separate(genres,c("single_genres"),sep = "\\|") %>% filter(title == "Boomerang (1992)")

#edx %>% separate_rows(genres, sep = "\\|") %>%
#  group_by(genres) %>%
#  summarize(count = n(), avg_rating = mean(rating)) %>%
#  arrange(desc(count))



ggplot(edx, aes(x= rating)) +
  geom_histogram( binwidth = 0.2) +
  scale_y_continuous(labels = scales::comma ) +
  ggtitle("number of ratings for each rating")+ 
  labs(x="rating", y="number of ratings", caption = "source data: movielens 10m") +
  geom_text(
    aes(label=after_stat(count)),
    stat='count', vjust=-0.2, size=4)


edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(20,count) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top 20 movies based on number of ratings" , caption = "source data: movielens 10m")


# histogram of number of ratings by movieId
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = "gold") +
  scale_x_log10() + 
  ggtitle("Movies") +
  labs(subtitle ="number of ratings by movieId", 
       x="movieId" , 
       y="number of ratings") +
  theme(panel.border = element_rect(colour="black", fill=NA)) 


# histogram of number of ratings by userId
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=40, color = "white") +
  scale_x_log10() + 
  ggtitle("Users") +
  labs(subtitle ="number of ratings by UserId", 
       x="userId" , 
       y="number of ratings") +
  theme(panel.border = element_rect(colour="grey", fill=NA))


# Plot number of ratings given by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 100, color = "grey") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users")


## Plot mean movie ratings given by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 100, color = "grey") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users")


#rating by separated genre
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  arrange(desc(count))


#Relationship between movie age and ratings  
edx %>% group_by(movie_age) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(movie_age, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average rating by movie age")


release_decade%>%
  ggplot(aes(date, avgrating, colour = premier_year_era_group)) +
  geom_point() +
  geom_smooth() +
  theme_bw() + theme(panel.grid = element_blank(),axis.title = element_blank()) +
  labs(colour = "Release decades", 
       title="Timestamp (unit in month)", 
       subtitle="Do movies prior 1980s recieve more star ratings than recent movies?")


##########################################################
#Methods
##########################################################


## Average movie rating model (AMR)

mu <- mean(edx$rating)
mu

AMR <- RMSE(final_holdout_test$rating, mu)
AMR

rmse_r <- tibble(method = "Average movie rating model", RMSE = AMR)


## Adding movie rating effect´s (MRE)

movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + final_holdout_test %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)

MRE <- RMSE(predicted_ratings, final_holdout_test$rating)

rmse_r <- bind_rows(rmse_r, tibble(method="Movie rating effect",  RMSE = MRE ))

MRE


movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"),
                     ylab = "Number of movies", main = "Number of movies vs b_i")


#Movie rating effect´s and user effect (MURE)

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- final_holdout_test%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

MURE <- RMSE(predicted_ratings, final_holdout_test$rating)

rmse_r <- bind_rows(rmse_r,
                          data_frame(method="Movie and user ratting effect",  
                                     RMSE = MURE))


MURE


#mure plot 
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))

user_avgs%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))


#final result

rmse_r




