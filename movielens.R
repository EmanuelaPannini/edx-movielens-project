################################################################################
# MovieLens Project
# Emanuela Pannini
# GitHub profile: https://github.com/EmanuelaPannini
################################################################################
# Create edx and final_holdout_test sets
# The following section of code was provided by Edx, this project starts after
################################################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages('tidyverse', repos = 'http://cran.us.r-project.org')
if(!require(caret)) install.packages('caret', repos = 'http://cran.us.r-project.org')

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- 'ml-10M100K.zip'
if(!file.exists(dl))
  download.file('https://files.grouplens.org/datasets/movielens/ml-10m.zip', dl)

ratings_file <- 'ml-10M100K/ratings.dat'
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- 'ml-10M100K/movies.dat'
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed('::'), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c('userId', 'movieId', 'rating', 'timestamp')
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed('::'), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c('movieId', 'title', 'genres')
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = 'movieId')

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind='Rounding') # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = 'movieId') %>%
  semi_join(edx, by = 'userId')

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################################################################
# The personal project starts here
################################################################################
# As explained below, some additional columns were added to the edx dataset 
# before it was split into training and test sets

set.seed(2024, sample.kind='Rounding')
library(mgcv)

# Extract the release year from the title and convert the review timestamp to 
# a readable date format. The same operation is performed for both edx and 
# final_holdout_test

edx <- edx %>% mutate(releaseYear = as.numeric(str_extract(title, '(?<=\\()\\d{4}(?=\\))')),
                      reviewDate = as_date(as_datetime(timestamp)))

final_holdout_test <- final_holdout_test %>% 
  mutate( releaseYear = as.numeric(str_extract(title, '(?<=\\()\\d{4}(?=\\))')), 
          reviewDate = as_date(as_datetime(timestamp)))

# For practical reasons, it will be useful to add two technical columns that 
# evaluate the deltas between the current year of release and the first one, 
# and the current review date and the first. In the latter case, the data has 
# been aggregated to a weekly level, as this speeds up the calculations and 
# reduces noise.

first_release_year <- min(edx$releaseYear)
first_review_date <- min(edx$reviewDate)
last_review_date <- year(max(edx$reviewDate)) 

edx <- edx %>% mutate(deltaReleaseYear = releaseYear - first_release_year,
                      deltaReviewDate = floor(as.numeric(reviewDate - first_review_date) / 7))

final_holdout_test <- final_holdout_test %>% 
  mutate( deltaReleaseYear = releaseYear - first_release_year,
          deltaReviewDate = floor(as.numeric(reviewDate - first_review_date) / 7))

################################################################################
# Plot section

edx_movies <- edx %>% summarize(distinctMovies = n_distinct(movieId)) %>% pull()
edx_users <- edx %>% summarize(distinctUsers = n_distinct(userId)) %>% pull()
releaseYear_per_movie <- edx %>% group_by(movieId) %>% 
                          summarize(num_distinct = n_distinct(releaseYear)) %>%
                          distinct(num_distinct) %>% 
                          pull()

# Definition of the personal theme to reproduce similar styles between plots
personal_theme <- function(current_plot_type){
  if(current_plot_type == 'heatmap')
  {
    theme(
      plot.title = element_text(size = 14, face = 'bold.italic'),
      legend.title = element_text(size = 8, vjust = 1.2),
      legend.key.size = unit(0.4, 'cm'),
      legend.text = element_text(size = 8, face = 'italic'),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size = 9, face = 'italic'),
      axis.title.y = element_text(size = 9, face = 'italic'),
      panel.border = element_rect(colour = 'black', fill = NA),
      panel.background = element_blank(),
      axis.text = element_blank())
  }
  else
  {
    if(current_plot_type == 'errorbar')
    { 
      theme(
        plot.title = element_text(size = 14, face = 'bold.italic'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8, face = 'italic'),
        axis.ticks.y = element_line(colour = 'black'),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 9, face = 'italic'),
        axis.title.y = element_text(size = 9, face = 'italic'),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank())
    }
    else
    {
      theme(
        plot.title = element_text(size = 14, face = 'bold.italic'),
        legend.title = element_text(size = 8, vjust = 1.2),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size = 8, face = 'italic'),
        axis.text = element_text(size = 8, face = 'italic'),
        axis.ticks = element_line(colour = 'black'),
        axis.title.x = element_text(size = 9, face = 'italic'),
        axis.title.y = element_text(size = 9, face = 'italic'),
        axis.line = element_line(colour = 'black'))
    }
  }
}
# Heatmap showing filling rate and the movie-user rating
# Note: it was pre-filtered on a random squared range in order to be informative
sparsity_plot <- edx %>% filter(userId <= 150 & movieId <= 150) %>%
                  ggplot(aes(movieId, userId, fill = rating)) +
                  geom_tile() + 
                  scale_fill_distiller(palette = 'RdPu', direction = 1) +
                  labs(x = 'movieId',
                       y = 'userId',
                       fill = 'rating') + 
                  personal_theme('heatmap')

# Histogram showing the number of full and half star reviews
rating_distribution <- edx %>% 
                        mutate(rating_type = ifelse((rating/0.5)%%2 == 0, 'Full star', 'Half star')) %>% 
                        ggplot(aes(rating, fill = rating_type)) +
                        geom_histogram(colour = 'black', binwidth = 0.25, center = 0.5) + 
                        scale_fill_manual(values = c('Full star' = '#c51b8a', 'Half star' = '#fbb4b9')) +
                        scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
                        scale_y_continuous(labels = scales::label_number (scale = 1e-6)) +
                        labs(
                          x = 'rating',
                          y = 'Million of reviews',
                          fill = 'rating type'
                        ) + 
                        personal_theme('histogram')

# Histogram showing the number of movies with a given number of ratings
# Note: it has been pre-filtered to movies with less than 1500 ratings
movie_rating <- edx %>% group_by(movieId) %>%
                  summarize(number_of_rating = n()) %>%
                  filter (number_of_rating < 1500) %>%
                  ggplot(aes(number_of_rating)) +
                  geom_histogram(bins = 50, colour = 'black', fill = '#c51b8a') + 
                  labs(
                    x = 'Ratings per movie',
                    y = 'Number of movies'
                  ) +
                  personal_theme('histogram')

# Histogram showing the distribution of the movies in relation to the average of 
# their ratings
movie_average <- edx %>% group_by(movieId) %>%
                  summarize(average_rating = mean(rating)) %>%
                  ggplot(aes(average_rating)) +
                  geom_histogram(bins = 50, colour = 'black', fill = '#c51b8a') + 
                  labs(
                    x = 'Average rating',
                    y = 'Number of movies'
                  ) +
                  personal_theme('histogram')

# Histogram showing the number of users with a given number of ratings
# Note: it has been pre-filtered to users with less than 750 ratings
user_rating <- edx %>% group_by(userId) %>%
                summarize(number_of_rating = n()) %>%
                filter (number_of_rating < 750) %>%
                ggplot(aes(number_of_rating)) +
                geom_histogram(bins = 50, colour = 'black', fill = '#c51b8a') + 
                labs(
                  x = 'Rating per user',
                  y = 'Number of users'
                ) +
                personal_theme('histogram')

# Histogram showing the distribution of the users in relation to the average of 
# their ratings
user_average <- edx %>% group_by(userId) %>%
                summarize(average_rating = mean(rating)) %>%
                ggplot(aes(average_rating)) +
                geom_histogram(bins = 50, colour = 'black', fill = '#c51b8a') + 
                labs(
                  x = 'Average rating',
                  y = 'Number of users'
                ) +
                personal_theme('histogram')

# Errorbar showing the distribution of genres, visualizing the mean and standard 
# error for each
# Note: it has been pre-filtered to genres with more than 1000 ratings
genres_distribution <- edx %>% group_by(genres) %>%
                        summarize(num_rows = n(),
                                  average_rating = mean(rating), 
                                  se_rating = sd(rating)/sqrt(num_rows)) %>%
                        filter(num_rows >= 1000) %>% 
                        mutate(genres = reorder(genres, average_rating)) %>%
                        ggplot(aes(x = genres, 
                                   y = average_rating, 
                                   ymin = average_rating - 2 * se_rating, 
                                   ymax = average_rating + 2 * se_rating)) + 
                        geom_errorbar(linewidth = 0.55, colour = '#7a0177') +
                        geom_point(size = 0.5, colour = '#fbb4b9') +
                        labs(
                          x = 'genres',
                          y = 'rating'
                        ) +
                        personal_theme('errorbar')

# Errorbar showing the distribution of genres, visualizing the mean and standard 
# error for each one, after dividing them into macro categories
split_genres_distribution <- edx %>% filter(genres != '(no genres listed)') %>% 
                              mutate(genere = strsplit(genres, '\\|')) %>%
                              unnest(genere) %>% group_by(genere) %>% filter(n() >= 1000) %>%
                              summarize(num_rows = n(),
                                        average_rating = mean(rating), 
                                        se_rating = sd(rating)/sqrt(num_rows)) %>%
                              mutate(genere = reorder(genere, average_rating)) %>%
                              ggplot(aes(x = genere, 
                                         y = average_rating, 
                                         ymin = average_rating - 2 * se_rating, 
                                         ymax = average_rating + 2 * se_rating)) + 
                              geom_errorbar(linewidth = 0.55, 
                                            width = 0.4, 
                                            colour = '#7a0177') +
                              geom_point(size = 1.1, colour = '#fbb4b9')  +
                              labs(
                                x = 'Splitted genres',
                                y = 'rating'
                              ) +
                              personal_theme('errorbar') +
                              theme(axis.ticks.x = element_line(),
                                    axis.text.x = element_text(size = 7, 
                                                                angle = 60,
                                                                vjust = 0.5,
                                                                colour = 'black'))

# Histogram showing the number of genres per movie
genres_per_movie <- edx %>% distinct(movieId, genres) %>% 
                      mutate(num_genres = ifelse(genres == '(no genres listed)', 
                                                 0, 
                                                 str_count(genres, '\\|') + 1)) %>%
                      group_by(num_genres) %>%
                      ggplot(aes(num_genres)) +
                      geom_histogram(colour = 'black', 
                                     fill = '#c51b8a', 
                                     binwidth = 0.25, 
                                     center = 0
                                     ) + 
                      labs(
                        x = 'Number of genres',
                        y = 'Number of movies'
                      ) +  
                      personal_theme('histogram')

# Boxplot illustrating the distribution of the number of ratings per releaseYear, 
# including also the outliers
ratings_per_releaseYear <- edx %>% group_by(movieId) %>%
                          summarize(num_ratings = n(),
                                    releaseYear = as.character(min((releaseYear)))) %>%
                          ggplot(aes(releaseYear, num_ratings)) + 
                          geom_boxplot(colour = '#c51b8a', 
                                       outlier.colour = '#fbb4b9', 
                                       outlier.size = 0.5) + 
                          scale_x_discrete(breaks = seq(1915, 2005, by = 10)) +
                          scale_y_sqrt() +
                          labs(
                            x = 'releaseYear',
                            y = 'Number of ratings'
                          ) +  
                          personal_theme('boxplot')

# Boxplot illustrating other relevant characteristics for each releaseYear, 
# including boxplot information (median, quantiles, outliers) and the mean
averages_per_release_year <- edx %>% group_by(movieId) %>%
                              summarize(ratings = mean(rating),
                                        releaseYear = as.character(min((releaseYear)))) %>%
                              ggplot(aes(releaseYear, ratings)) + 
                              geom_boxplot(colour = '#c51b8a', 
                                           outlier.colour = '#fbb4b9', 
                                           outlier.size = 0.5) + 
                              scale_x_discrete(breaks = seq(1915, 2005, by = 10)) +
                              stat_summary(fun = mean, 
                                           geom = 'point', 
                                           shape = 22, 
                                           size = 1, 
                                           colour = '#7a0177', 
                                           fill = '#7a0177') + 
                              labs(
                                x = 'releaseYear',
                                y = 'rating'
                              ) +  
                              personal_theme('boxplot')

# Histogram highlighting the review rate over the years
review_rate <- edx %>% mutate(reviewYear = year(reviewDate)) %>% 
                group_by(reviewYear) %>%
                ggplot(aes(reviewYear)) +
                geom_histogram(colour = 'black', 
                               fill = '#c51b8a', 
                               binwidth = 0.25, 
                               center = 1994) + 
                labs(
                  x = 'reviewYear',
                  y = 'Thousands of reviews'
                ) + 
                scale_x_continuous(breaks = seq(1994, 2010, by = 2)) +
                scale_y_continuous(labels = scales::label_number (scale = 1e-3)) + 
                personal_theme('histogram')

# Smooth fit to show the number of reviews per year affects the average rating
# Note: the first reviewDate is in 1995 so the range is quite small
review_rate_average_rating <- edx %>% 
                                filter(releaseYear >= 1995) %>% 
                                group_by(movieId) %>% 
                                summarize(num_rating = n(),
                                          effective_years = last_review_date - min(releaseYear),
                                          average_rating = mean(rating),
                                          total_rate = num_rating/effective_years) %>%
                                ggplot(aes(total_rate, average_rating)) +
                                geom_point(colour = '#fbb4b9', size = 0.3) +
                                geom_smooth(colour = '#c51b8a') +
                                labs(
                                  x = 'Review rate per year',
                                  y = 'Average rating'
                                ) +
                                personal_theme('')

# Smooth fit to show the reviewDate effect on the average rating
# Note: the first reviewDate is in 1995 so the range is quite small
review_time_dependence <- edx %>% 
                          group_by(reviewDate) %>%
                          summarize(average_rating = mean(rating)) %>%
                          ggplot(aes(reviewDate, average_rating)) +
                          geom_point(size = 0.3) +
                          geom_smooth(colour = '#c51b8a') +
                          labs(
                            x = 'reviewDate',
                            y = 'rating'
                          ) +
                          personal_theme ('')

################################################################################
# Definition of the test and training sets, including the new columns
# We will use a code similar to the one provided by Edx to split in edx and 
# final_holdout_test
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in edx_train and edx_test
edx_test <- temp %>% 
  semi_join(edx_train, by = 'movieId') %>%
  semi_join(edx_train, by = 'userId')

# Add rows removed from edx_test in edx_train
edx_removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, edx_removed)

rm(edx_removed, temp, test_index)

################################################################################
# Models

# Declaration of the function that rounds to the nearest feasible rating 
round_predictions <- function(value){
  rounded_result = case_when(
                    value < 0.5 ~ 0.5,
                    value > 5 ~ 5,
                    .default = round(value/0.5)*0.5)
  return(rounded_result)
}

# Simplest model: it includes movieId, userId, genres and releaseYear
# Note: the function has been created for clarity, even if it is not necessary 
# (e.g. it has no input parameters). Even if it is possible to parametrise it,
# this has not been done as it is out of the scope of the project. 
# The same observation is also true for other models, unless otherwise specified
simple_prediction <- function(){
  
  # Train the model on the edx_train set
  mu <- mean(edx_train$rating)
  movie_effect <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_m = mean(rating - mu))
  user_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_m))
  genre_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    group_by(genres) %>% 
    summarize(b_g = mean(rating - mu - b_m - b_u))
  year_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    left_join(genre_effect, by = 'genres') %>%
    group_by(releaseYear) %>%
    summarize(b_y = mean(rating - mu - b_m - b_u - b_g))
  
  # Test the model on the edx_test set
  predicted_ratings <- edx_test %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    left_join(genre_effect, by = 'genres') %>%
    left_join(year_effect, by = 'releaseYear') %>%
    mutate(prediction = mu + b_m + b_u + b_g + b_y, 
           rounded_prediction = sapply(prediction, round_predictions))
  
  # The function returns the standard RMSE and the rounded RMSE
  RMSE_standard <- RMSE(predicted_ratings$prediction, edx_test$rating)
  RMSE_rounded <- RMSE(predicted_ratings$rounded_prediction, edx_test$rating)  
  return(list(RMSE_standard = RMSE_standard, RMSE_rounded = RMSE_rounded))
}
simple <- simple_prediction()
sprintf('The standard RMSE with the simple model is %.5f', simple$RMSE_standard)
sprintf('The rounded RMSE with the simple model is %.5f', simple$RMSE_standard)

# First insight on time dependence
# As shown in the plot section, the reviewDate has a temporal effect, so a 
# smooth fit was created for this feature. The function below will only 
# elaborate on the value of the prediction as the function is fitted within 
# the model
review_week_effects <- function(current_dataframe, current_review_week_fit, current_mu){
  
  # The sampling points have been redefined to allow a different range between 
  # the training and prediction. For technical reasons, it was decided to use 
  # the deltaReviewDate instead of the reviewDate, aggregated to a weekly level
  sample_points <- seq(min(current_dataframe$deltaReviewDate),
                       max(current_dataframe$deltaReviewDate))
  
  # Evaluation of the predictions
  prediction <- predict.gam(current_review_week_fit, 
                        data.frame(deltaReviewDate = sample_points)) - current_mu
  return(prediction)
}

# The same idea and observations have been applied to releaseYear
release_year_effects <- function(current_dataframe, current_release_year_fit, current_mu){
  sample_points <- seq(min(current_dataframe$deltaReleaseYear),
                       max(current_dataframe$deltaReleaseYear))
  prediction <- predict.gam(current_release_year_fit, 
                        data.frame(deltaReleaseYear = sample_points)) - current_mu
  return(prediction)
}

# Double time dependence model: it includes all the features above and the 
# double information related to time

double_time_dependence_model <- function(){
  
  # Train the model on the edx_train set
  mu <- mean(edx_train$rating)
  
  # Smooth fit on the deltaReviewDate column and evaluation of the effects
  review_week_fit <- gam(rating ~ s(deltaReviewDate, bs = 'cs'),
                                family = gaussian(),
                                data = edx_train) 
  review_week_effect <- review_week_effects(edx_train, review_week_fit, mu)
  
  # Smooth fit on the deltaReleaseYear column and evaluation of the effects
  release_year_fit <- gam(rating ~ s(deltaReleaseYear, bs = 'cs'),
                                 family = gaussian(),
                                 data = edx_train)
  release_year_effect <- release_year_effects(edx_train, release_year_fit, mu)
  
  movie_effect <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_m = mean(rating - mu - review_week_effect[deltaReviewDate+1] 
                         - release_year_effect[deltaReleaseYear+1]))
  user_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_m - review_week_effect[deltaReviewDate+1] 
                         - release_year_effect[deltaReleaseYear+1]))
  genre_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    group_by(genres) %>% 
    summarize(b_g = mean(rating - mu - b_m - b_u 
                          - review_week_effect[deltaReviewDate+1] 
                          - release_year_effect[deltaReleaseYear+1]))
  
  # Test the model on the edx_test set
  predicted_ratings <- edx_test %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    left_join(genre_effect, by = 'genres') %>%
    mutate(prediction = mu + b_m + b_u + b_g + 
             review_week_effect[deltaReviewDate+1] + 
             release_year_effect[deltaReleaseYear+1], 
           rounded_prediction = sapply(prediction, round_predictions))
  
  # The function returns the standard RMSE and the rounded RMSE
  RMSE_standard <- RMSE(predicted_ratings$prediction, edx_test$rating)
  RMSE_rounded <- RMSE(predicted_ratings$rounded_prediction, edx_test$rating)  
  return(list(RMSE_standard = RMSE_standard, RMSE_rounded = RMSE_rounded))
}
double_time_dependence <- double_time_dependence_model()
sprintf('The standard RMSE with the double time dependence model is %.5f', 
        double_time_dependence$RMSE_standard)
sprintf('The rounded RMSE with the double time dependence model is %.5f', 
        double_time_dependence$RMSE_standard)

# Second insight on time dependence
# As shown by the plot section and the results above, it is not correct to 
# evaluate the time dependence also on the releaseYear, so the following model 
# was developed
time_dependence_model <- function(){
  
  # Train the model on the edx_train set
  mu <- mean(edx_train$rating)
  
  # Smooth fit on the deltaReviewDate column and evaluation of the effects
  review_week_fit <- gam(rating ~ s(deltaReviewDate, bs = 'cs'),
                                      family = gaussian(),
                                      data = edx_train) 
  review_week_effect <- review_week_effects(edx_train, review_week_fit, mu)
  
  movie_effect <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_m = mean(rating - mu - review_week_effect[deltaReviewDate+1]))
  user_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_m - review_week_effect[deltaReviewDate+1]))
  genre_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    group_by(genres) %>% 
    summarize(b_g = mean(rating - mu - b_m - b_u 
                         - review_week_effect[deltaReviewDate+1]))
  year_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    left_join(genre_effect, by = 'genres') %>%
    group_by(releaseYear) %>%
    summarize(b_y = mean(rating - mu - b_m - b_u - b_g 
                         - review_week_effect[deltaReviewDate+1]))
  
  # Test the model on the edx_test set
  predicted_ratings <- edx_test %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    left_join(genre_effect, by = 'genres') %>%
    left_join(year_effect, by = 'releaseYear') %>%
    mutate(prediction = mu + b_m + b_u + b_g + b_y 
                        + review_week_effect[deltaReviewDate+1], 
           rounded_prediction = sapply(prediction, round_predictions))
  
  RMSE_standard <- RMSE(predicted_ratings$prediction, edx_test$rating)
  RMSE_rounded <- RMSE(predicted_ratings$rounded_prediction, edx_test$rating)  
  return(list(RMSE_standard = RMSE_standard, RMSE_rounded = RMSE_rounded))
}
time_dependence <- time_dependence_model()
sprintf('The standard RMSE with the time dependence model is %.5f', 
        time_dependence$RMSE_standard)
sprintf('The rounded RMSE with the time dependence model is %.5f', 
        time_dependence$RMSE_standard)

# Regularized model: lambda improvement
# The model below is the time dependent one after applying a lambda term. 
# In this case, the training and test sections have been split to allow 
# detailed analysis and lambda tuning.
# Training function
regularized_time_dependent_model <- function(current_lambda){
  
  # Train the model on the edx_train set
  mu <- mean(edx_train$rating)
  
  # Smooth fit on the deltaReviewDate column and evaluation of the effects
  review_week_fit <- gam(rating ~ s(deltaReviewDate, bs = 'cs'),
                         family = gaussian(),
                         data = edx_train) 
  review_week_effect <- review_week_effects(edx_train, review_week_fit, mu)
  
  movie_effect <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu - review_week_effect[deltaReviewDate+1])
              /(n() + current_lambda))
  user_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_m - review_week_effect[deltaReviewDate+1])
              /(n() + current_lambda))
  genre_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu - b_m - b_u - review_week_effect[deltaReviewDate+1])
              /(n() + current_lambda))
  year_effect <- edx_train %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    left_join(genre_effect, by = 'genres') %>%
    group_by(releaseYear) %>%
    summarize(b_y = sum(rating - mu - b_m - b_u - b_g - 
                          review_week_effect[deltaReviewDate+1])/(n() + current_lambda))
  
  # Return the list of parameters after the training phase
  return(list(
    mu = mu,
    movie_effect = movie_effect, 
    user_effect = user_effect, 
    genre_effect = genre_effect,
    year_effect = year_effect,
    review_week_effect = review_week_effect,
    current_lambda = current_lambda
  ))
}

# Prediction function
prediction_regularized_time_dependent_model <- function(
                                        current_regularized_time_dependent_model, 
                                        target_dataframe){
  
  mu <- current_regularized_time_dependent_model$mu
  movie_effect <- current_regularized_time_dependent_model$movie_effect
  user_effect <- current_regularized_time_dependent_model$user_effect
  genre_effect <- current_regularized_time_dependent_model$genre_effect
  year_effect <- current_regularized_time_dependent_model$year_effect
  review_week_effect <- current_regularized_time_dependent_model$review_week_effect
  current_lambda <- current_regularized_time_dependent_model$current_lambda 
  
  # Test the model on the target set
  predicted_ratings <- target_dataframe %>%
    left_join(movie_effect, by = 'movieId') %>%
    left_join(user_effect, by = 'userId') %>%
    left_join(genre_effect, by = 'genres') %>%
    left_join(year_effect, by = 'releaseYear') %>%
    mutate(prediction = mu + b_m + b_u + b_g + b_y + review_week_effect[deltaReviewDate+1], 
           rounded_prediction = sapply(prediction, round_predictions))
  
  RMSE_standard <- RMSE(predicted_ratings$prediction, target_dataframe$rating)
  RMSE_rounded <- RMSE(predicted_ratings$rounded_prediction, target_dataframe$rating)
  return(list(lambda_value = current_lambda, 
              RMSE_standard = RMSE_standard, 
              RMSE_rounded = RMSE_rounded))
}

# Define a list of lambdas to pick the best one
# Due to performance limitations, the range is quite small, but it has also been
# tested on another machine with:
# lambdas <- seq(1, 10, 0.05)
# This test provided the same results and also confirmed the possibility of 
# reducing this range for the project
lambdas <- seq(3, 6, 0.25)

# Train on the edx_train set and test on the edx_test set to find the most 
# suitable lambda value
lambda_regularization <- sapply(lambdas, function(current_lambda)
{
  current_regularized_time_dependent_model <- regularized_time_dependent_model(current_lambda)
  prediction <- prediction_regularized_time_dependent_model(current_regularized_time_dependent_model, edx_test)
  return(prediction)
})

# Create a summary to visualize the results
summary_regularized_rmses <- as.data.frame(sapply(as.data.frame(t(lambda_regularization)), as.numeric))

# Select the lambda value that minimizes the RMSE
final_lambda <- summary_regularized_rmses %>% 
  filter(RMSE_standard == min(RMSE_standard)) %>% 
  select(lambda_value) %>%
  pull()

# Plot of the variation of the RMSE through lambdas
lambda_values <- summary_regularized_rmses %>%
                    ggplot(aes(lambda_value, RMSE_standard)) +
                    geom_line(linewidth = 0.45) +
                    geom_point(size = 1.8, colour = '#7a0177', shape = 19) +
                    scale_x_continuous(breaks = seq(1, 10, by = 0.5)) +
                    labs(
                      x = 'lambda',
                      y = 'RMSE standard'
                    ) +
                    personal_theme('')

################################################################################
# Final model evaluation on final_holdout_test
# Target RMSE < 0.86490
# Train the regularized_time_dependent_model with the final_lambda value on 
# edx_train set and test it against final_holdout_test
final_model <- regularized_time_dependent_model(final_lambda)
final_evaluation <- prediction_regularized_time_dependent_model(final_model, final_holdout_test)
sprintf('The standard RMSE evaluated against final_holdout_test is %.5f', final_evaluation$RMSE_standard)
