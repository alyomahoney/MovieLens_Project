##############################
# installing required packages
##############################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggthemes)
library(lubridate)
library(recommenderlab)
library(magrittr)




##########################################################
# code provided by HarvardX
# create edx set, validation set (final hold-out test set)
##########################################################

# note: this process could take a couple of minutes

# movieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# download data set
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# validation set will be 10% of movieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# add release year and rating month/year
edx %<>% mutate(release_year = as.numeric(str_sub(title, start = -5, end = -2)),
                year_rated = year(as_datetime(timestamp)),
                month_rated = month(as_datetime(timestamp), label = TRUE))

validation %<>% mutate(release_year = as.numeric(str_sub(title, start = -5, end = -2)),
                       year_rated = year(as_datetime(timestamp)),
                       month_rated = month(as_datetime(timestamp), label = TRUE))




##################
# data exploration
##################

# structure of edx data set
edx_str <- capture.output(str(edx)) # the reason for doing it like this is so that the output can be saved and loaded in the rmd file
cat(edx_str, sep = "\n")            # that way, the entire edx data set doesn't have to be loaded

# define the mean movie rating
mu <- mean(edx$rating)

# set global theme (google docs theme)
theme_set(new = theme_gdocs())

# histogram of ratings - visualise the distribution
edx %>% ggplot(aes(rating)) +
  geom_histogram(bins = 10, color = I("black")) +
  geom_vline(xintercept = mu, linetype = "dashed", colour = "red") +
  ggtitle("Movie Ratings") +
  ylab("Count") +
  xlab("Rating")

# how does release year effect the rating?
edx %>%
  group_by(release_year) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(release_year, mean_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(colour = "red") +
  ylim(3.25, 4.25) +
  ggtitle("Mean Rating for Each Release Year") +
  ylab("Mean Rating") +
  xlab("Release Year")

# how does the year in which the movie was rated effect the rating?
edx %>%
  group_by(year_rated) %>%
  summarize(mean_rating = mean(rating)) %>% ggplot(aes(year_rated, mean_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(colour = "red") +
  ylim(3.25, 4.25) +
  ggtitle("Mean Rating for Each Year of Review") +
  ylab("Mean Rating") +
  xlab("Year of Review")

# how about the month in which the movie was rated?
edx %>%
  group_by(month_rated) %>%
  summarize(mean_rating = mean(rating)) %>% ggplot(aes(as.numeric(month_rated), mean_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(colour = "red") +
  scale_x_continuous(breaks = 1:12, labels = month(1:12, label = T)) +
  ylim(3.25, 4.25) +
  ggtitle("Mean Rating for Each Month of Review") +
  ylab("Mean Rating") +
  xlab("Month of Review")

# how does the genre effect the rating?
set.seed(2, sample.kind = "Rounding")
edx_by_genre <- edx %>%
  slice(sample(1:nrow(edx),100000)) %>% # can't handle entire data set
  separate_rows(genres, sep = "\\|")    # still takes a minute or two to run

edx_by_genre %>%
  group_by(genres) %>%
  summarize(n_movies = n_distinct(movieId),
            n_ratings = n(),
            mean_rating = mean(rating),
            se = sd(rating)/sqrt(n()),
            lower = mean_rating - se*qt(0.975, n()),
            upper = mean_rating + se*qt(0.975, n())) %>%
  mutate(genres = reorder(genres, mean_rating)) %>%
  ggplot(aes(genres, mean_rating)) +
  geom_point(aes(size = n_ratings)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = mu, color = "red", linetype = "dashed") +
  geom_errorbar(aes(ymin = upper, ymax = lower)) +
  ylim(3.2, 4.25) +
  ggtitle("Mean Rating for Each Genre") +
  ylab("Mean Rating") +
  xlab("Genre") +
  labs(size = "No. of ratings")




##########################
# defining a loss function
##########################

# the RMSE will be used to measure the performance of the models
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}




#######################################################
# constructing the most basic model, 'just the average'
#######################################################

# create train and test sets from the edx data set
set.seed(1, sample.kind="Rounding")
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx %>% slice(-edx_test_index)
edx_temp <- edx %>% slice(edx_test_index)

# make sure userId and movieId in test set are also in train set
edx_test <- edx_temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# add rows removed from test set back into train set
removed <- anti_join(edx_temp, edx_test)
edx_train <- rbind(edx_train, removed)

# calculate mean rating
mu_train <- edx_train$rating %>% mean

# rmse for the model using just the average movie rating for prediction
rmse_1 <- RMSE(mu_train, edx_test$rating)

# storing the rmse (and future results when they are calculated) in a data frame
rmse_results <- data.frame(Method = "Just the Average",
                           RMSE = rmse_1)




###########################
# accounting for movie bias
###########################

# motivational plot - movies are rated differently
edx %>%
  group_by(movieId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(color = I("black")) +
  ggtitle("Mean Rating for Each Movie") +
  ylab("Count") +
  xlab("Mean Rating")

# calculate b_m, the bias for each movie
movie_bias <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating-mu_train))

# define predictions
predictions_2 <- mu_train + edx_test %>%
  left_join(movie_bias, by = "movieId") %>%
  .$b_m

# calculate rmse for the new model and add it to the results data frame
rmse_2 <- RMSE(edx_test$rating, predictions_2)
rmse_results <-  rbind(rmse_results, data.frame(Method = "Movie Bias",
                                                RMSE = rmse_2))




####################################
# accounting for movie and user bias
####################################

# motivational plot - users have different rating tendencies
edx %>%
  group_by(userId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(color = I("black")) +
  ggtitle("Mean Rating for Each User") +
  ylab("Count") +
  xlab("Mean Rating")

# calculate b_u, the bias for each user after accounting for movie bias
user_bias <- edx_train %>%
  left_join(movie_bias, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_train - b_m))

# define predictions
predictions_3 <- edx_test %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(user_bias, by = "userId") %>%
  mutate(pred = mu_train + b_m + b_u) %>%
  .$pred

# ensure the predictions don't exceed the rating limits
predictions_3[predictions_3<0.5] <- 0.5
predictions_3[predictions_3>5] <- 5

# calculate rmse for the new model and add it to the results data frame
rmse_3 <- RMSE(predictions_3, edx_test$rating)
rmse_results <- rbind(rmse_results, data.frame(Method = "Movie + User Bias",
                                               RMSE = rmse_3))




########################
# regularised movie bias
########################

# motivational tables (these aren't convincing top/bottom five lists)
movies <- edx %>% select(movieId, title) %>% unique() %>% arrange(movieId)

top_five <- edx %>% count(movieId) %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(movies, by = "movieId") %>%
  arrange(desc(b_m)) %>%
  select(title, b_m, n) %>%
  slice(1:5)

bottom_five <- edx %>% count(movieId) %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(movies, by = "movieId") %>%
  arrange(b_m) %>%
  select(title, b_m, n) %>%
  slice(1:5)


# k fold cross validation will be used to choose optimal tuning parameter, lambda
lambdas_m <- seq(1, 3, 0.1)                          # tuning parameter values to be tested
k = 5                                                # 5-fold cross validation
set.seed(1, sample.kind = "Rounding")
ind <- createFolds(1:nrow(edx_train), k = k)         # create indices for the k folds

# empty matrix which will store the cv rmse results
rmses_lambda_m <- matrix(nrow = k, ncol = length(lambdas_m))

# could take a couple of minutes to run
for (i in 1:k) {
  
  # define train/test set for cv
  cv_train <- edx_train %>% slice(ind[[i]])
  cv_temp  <- edx_train %>% slice(-ind[[i]])
  
  # make sure movieId in test set is also in train set
  cv_test <- cv_temp %>%
    semi_join(cv_train, by = "movieId") 
  
  # add removed terms back into train set
  removed <- cv_temp %>% anti_join(cv_test)
  cv_train <- rbind(cv_train, removed)
  
  # define mean rating of cv train set
  cv_mu <- mean(cv_train$rating)
  
  # the sum element of the regularised regression loss term
  just_the_sum <- cv_train %>%
    group_by(movieId) %>%
    summarize(s = sum(rating - cv_mu), n_m = n())
  
  # for each lambda, obtain predictions and rmse value
  rmses_lambda_m[i,] <- sapply(lambdas_m, function(l){
    
    predicted_ratings <- cv_test %>%
      left_join(just_the_sum, by = "movieId") %>%
      mutate(b_m_reg = s/(n_m + l)) %>%
      mutate(pred = cv_mu + b_m_reg) %>%
      .$pred
    
    return(RMSE(cv_test$rating, predicted_ratings))
  })
  # remove unnecessary variables
  rm(cv_train, cv_temp, cv_test, just_the_sum, removed, cv_mu)
}
rm(i, ind, k) # remove unnecessary variables

# define the optimal value for lambda
lambda_m <- lambdas_m[which.min(colMeans(rmses_lambda_m))]

# vector of mean RMSE for each value of lambda
colMean_m <- data.frame(rmse = colMeans(rmses_lambda_m),
                        lambda = lambdas_m)

# visualise how each value for lambda performs
colMean_m %>%
  ggplot(aes(lambda, rmse)) +
  geom_point() +
  geom_point(aes(x = lambda_m, y = min(rmse)), shape = 5, size = 5) +
  ggtitle("RMSE for Various Values of \u03BB") +
  ylab("RMSE") +
  xlab("\u03BB")

# define the regularised movie bias with the optimal lambda
movie_bias_reg <- edx_train %>%
  group_by(movieId) %>%
  summarise(b_m_reg = sum(rating - mu_train)/(n()+lambda_m))

# define predictions for the model
predictions_4 <- edx_test %>%
  left_join(movie_bias_reg, by = "movieId") %>%
  mutate(pred = mu_train + b_m_reg) %>%
  .$pred

# calculate rmse for the new model and add it to the results data frame
rmse_4 <- RMSE(edx_test$rating, predictions_4)
rmse_results <- rbind(rmse_results, data.frame(Method = "Regularised Movie Bias",
                                               RMSE = rmse_4))



# these new top/bottom five lists are much more convincing
top_five_reg <- edx %>% count(movieId) %>%
  left_join(movie_bias_reg, by = "movieId") %>%
  left_join(movies, by = "movieId") %>%
  arrange(desc(b_m_reg)) %>%
  select(title, b_m_reg, n) %>%
  slice(1:5)

bottom_five_reg <- edx %>% count(movieId) %>%
  left_join(movie_bias_reg, by = "movieId") %>%
  left_join(movies, by = "movieId") %>%
  arrange(b_m_reg) %>%
  select(title, b_m_reg, n) %>%
  slice(1:5)




#################################
# regularised movie and user bias
#################################

# the optimal lambda chosen for movie bias in the previous model is fixed

lambdas_u <- seq(4, 6, 0.1)                          # tuning parameters to be tested
k = 5                                                # 5-fold cross validation
set.seed(1, sample.kind = "Rounding")
ind <- createFolds(1:nrow(edx_train), k = k)         # create indices for the k folds

# empty matrix which will store the cv rmse results
rmses_lambda_u <- matrix(nrow = k, ncol = length(lambdas_u))

# could take a couple of minutes to run
for (i in 1:k) {
  
  # define train/test set for cv
  cv_train <- edx_train %>% slice(ind[[i]])
  cv_temp  <- edx_train %>% slice(-ind[[i]])
  
  # make sure movieId and userId in test set is also in train set
  cv_test <- cv_temp %>%
    semi_join(cv_train, by = "movieId") %>%
    semi_join(cv_train, by = "userId")
  
  # add removed terms back into train set
  removed <- cv_temp %>% anti_join(cv_test)
  cv_train <- rbind(cv_train, removed)
  
  # define mean rating of cv train set
  cv_mu <- mean(cv_train$rating)
  
  # define resularised movie bias for cv train set
  cv_b_m_reg <- cv_train %>%
    group_by(movieId) %>%
    summarize(b_m_reg = sum(rating - cv_mu)/(n() + lambda_m)) %>%
    data.frame()
  
  # for each lambda, obtain predictions and rmse value
  rmses_lambda_u[i,] <- sapply(lambdas_u, function(l){
    
    # regularised user bias
    cv_b_u_reg <- cv_train %>%
      left_join(cv_b_m_reg, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u_reg = sum(rating - b_m_reg - cv_mu)/(n()+l)) %>%
      data.frame()
    
    predicted_ratings <- cv_test %>%
      left_join(cv_b_m_reg, by = "movieId") %>%
      left_join(cv_b_u_reg, by = "userId") %>%
      mutate(pred = cv_mu + b_m_reg + b_u_reg) %>%
      .$pred
    
    return(RMSE(cv_test$rating, predicted_ratings))
  })
  # remove unnecessary variables
  rm(cv_train, cv_temp, cv_test, removed, cv_b_m_reg, cv_mu)
}
rm(i, ind, k) # remove unnecessary variables

# define the optimal value for lambda
lambda_u <- lambdas_u[which.min(colMeans(rmses_lambda_u))] 

# vector of mean RMSE for each value of lambda
colMean_u <- data.frame(rmse = colMeans(rmses_lambda_u),
                        lambda = lambdas_u)

# visualise how each value for lambda performs
colMean_u %>%
  ggplot(aes(lambda, rmse)) +
  geom_point() +
  geom_point(aes(x = lambda_u, y = min(rmse)), shape = 5, size = 5) +
  ggtitle("RMSE for Various Values of \u03BB") +
  ylab("RMSE") +
  xlab("\u03BB")

# define the regularised movie bias with the optimal lambda
user_bias_reg <- edx_train %>%
  left_join(movie_bias_reg, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u_reg = sum(rating - b_m_reg - mu_train)/(n()+lambda_u))

# define predictions for the model
predictions_5 <- edx_test %>%
  left_join(movie_bias_reg, by = "movieId") %>%
  left_join(user_bias_reg, by = "userId") %>%
  mutate(pred = mu_train + b_m_reg + b_u_reg) %>%
  .$pred

# ensure the predictions don't exceed the rating limits
predictions_5[predictions_5<0.5] <- 0.5
predictions_5[predictions_5>5] <- 5

# calculate rmse for the new model and add it to the results data frame
rmse_5 <- RMSE(edx_test$rating, predictions_5)
rmse_results <- rbind(rmse_results, data.frame(Method = "Regularised Movie + User Bias",
                                               RMSE = rmse_5))




####################################
# user based collaborative filtering
####################################

# this section creates a recommendation system using Stefan Nikolic's adaptaion of Recommenderlab
# Nikolic's adaptation is suitable for large data sets - recommenderlab alone is not able to handle `edx_test`
# Nikolic's github link: https://github.com/smartcat-labs/collaboratory
# I thank and fully credit Nikolic for this solution to use collaborative filtering with large data sets

# create matrix (each row is a user and each column is a movie. the values are the residual ratings from reg movie + user bias model)
train_mat <- edx_train %>%
  left_join(movie_bias_reg) %>%
  left_join(user_bias_reg) %>%
  mutate(residual = rating - mu_train - b_m_reg - b_u_reg) %>% # define the residuals from the reg movie + user bias model
  select(userId, movieId, residual) %>%
  spread(movieId, residual)
rownames(train_mat) <- train_mat$userId                        # define row names (userId) which are in the first column of the matrix
train_mat %<>% select(-userId)                                 # remove the first column (which had the userId values)
train_mat <- as.matrix(train_mat)                              # coerce to matrix
dimnames(train_mat) <- list(userId = dimnames(train_mat)[[1]], # add dimension names
                            movieId = dimnames(train_mat)[[2]])

# running the scripts provided by Nikolic
source("cf_scripts/cf_algorithm.R")         # the collaborative filtering algorithm
source("cf_scripts/similarity_measures.R")  # defining similarity measures (cosine or pearson)

# coercing the matrix to a realRatingMatrix as required by recommenderlab
edx_train_r <- as(train_mat, "realRatingMatrix")
edx_train_rm <- edx_train_r@data

# the dimension names of the matrix are ignored in Nikolic's code, so some sort of mapping is required
# to link matrix indices to user nd movie IDs. for instance, entry 300,300 is not user 300 and movie 300
# this step is crucial in ensuring the returned predictions get matched up with the correct user/movie IDs
row_map_edx <- data.frame(userId = dimnames(train_mat)[[1]] %>% as.integer,
                          i = 1:dim(train_mat)[1])
col_map_edx <- data.frame(movieId = dimnames(train_mat)[[2]] %>% as.numeric,
                          j = 1:dim(train_mat)[2])

# define the missing entries in edx which are in test these are the entries we want to predict
test_subset <- edx_test %>%
  select(userId, movieId) %>%
  left_join(row_map_edx) %>%
  left_join(col_map_edx)

# run the ubcf algorithm
# Nikolic's code requires that the transpose of the matrix be used for ubcf
preds_ubcf <- predict_cf(t(edx_train_rm), test_subset %>% select(j, i) %>% as.matrix, alg_method = "ubcf", normalization = FALSE, similarity_metric = cal_cos,
                         k = 100, make_positive_similarities = FALSE, rowchunk_size = 15000, columnchunk_size = 1000)

# convert results into a more usable format
preds_ubcf %<>% t %>% summary %>% as.data.frame
preds_ubcf <- test_subset %>%
  left_join(preds_ubcf, by = c("i","j")) %>%
  select(-i, -j)

# note that some values are NA. this is due to the nature of the algorithm. sometimes it's impossible to determine similarity measures
# between users/movies due to the matrix being so sparse
is.na(preds_ubcf$x)

# replace with zeros
preds_ubcf[is.na(preds_ubcf$x),"x"] <- 0

# since a residual matrix was used, the regularised parameter estimates have to be added to achieve final predictions
predictions_6 <- edx_test %>%
  mutate(ubcf = preds_ubcf$x) %>%
  left_join(movie_bias_reg) %>%
  left_join(user_bias_reg) %>%
  mutate(preds = ubcf + mu_train + b_m_reg + b_u_reg) %>%
  .$preds

predictions_6[predictions_6<0.5] <- 0.5  # keep ratings within realistic bounds
predictions_6[predictions_6>5] <- 5      # keep ratings within realistic bounds

# calculate rmse for the new model and add it to the results data frame
rmse_6 <- RMSE(edx_test$rating, predictions_6)
rmse_results <- rbind(rmse_results, data.frame(Method = "User-Based Collaborative Filtering",
                                               RMSE = rmse_6))




####################################
# item-based collaborative filtering
####################################

# this approach is much the same as the last
preds_ibcf <- predict_cf(edx_train_rm, test_subset %>% select(i, j) %>% as.matrix, alg_method = "ibcf", normalization = FALSE, similarity_metric = cal_cos,
                         k = 100, make_positive_similarities = FALSE, rowchunk_size = 18000, columnchunk_size = 5500)

# convert results into a more usable format
preds_ibcf %<>% summary %>% as.data.frame
preds_ibcf <- test_subset %>%
  left_join(preds_ibcf, by = c("i","j")) %>%
  select(-i, -j)

# again, some values are missing
is.na(preds_ibcf$x)
preds_ibcf[is.na(preds_ibcf$x),"x"] <- 0

# again, the regularised parameter estimates have to be added to achieve final predictions
predictions_7 <- edx_test %>%
  mutate(ibcf = preds_ibcf$x) %>%
  left_join(movie_bias_reg) %>%
  left_join(user_bias_reg) %>%
  mutate(preds = ibcf + mu_train + b_m_reg + b_u_reg) %>%
  .$preds

predictions_7[predictions_7<0.5] <- 0.5  # keep ratings within realistic bounds
predictions_7[predictions_7>5] <- 5      # keep ratings within realistic bounds

# calculate rmse for the new model and add it to the results data frame
rmse_7 <- RMSE(edx_test$rating, predictions_7)
rmse_results <- rbind(rmse_results, data.frame(Method = "Item-Based Collaborative Filtering",
                                               RMSE = rmse_7))




####################################################
# user-item-based collaborative filtering (ensemble)
####################################################

# this model ensembles the two collaborative filtering models by taking the average of their ratings

# ibcf performs better, so it might be of interest to give more weight to the ibcf predictions
# cv could be used to determine coefficients, however only predictions are available, and we're not really training a model
# this section just tries various p values and assesses the RMSE on the test set

proportions <- seq(0.6, 0.9, by = 0.02)        # values of p to try (p is the weight given to the ibcf alg)
ens <- data.frame(ibcf = predictions_7,        # data frame with predictions from both cf models
                   ubcf = predictions_6)
rmses_ens <- sapply(proportions, function(p){  # return rmse values with various values of p
  ens %>%
    transmute(preds = p*ibcf+(1-p)*ubcf) %$%
    RMSE(preds, edx_test$rating)
})

opt_p <- proportions[which.min(rmses_ens)]     # the value of p which returned the lowers rmse
prop_df <- data.frame(p = proportions,         # data frame used to create the visualisation below
                      rmse = rmses_ens)

# visualise which value of p gave the lowest rmse
prop_df %>%
  ggplot(aes(p, rmse)) +
  geom_point() +
  geom_point(aes(x = opt_p, y = min(rmse)), shape = 5, size = 5) +
  ggtitle("RMSE for Various Values of p") +
  ylab("RMSE") +
  xlab("p")

# calculate the (weighted) mean predictions
predictions_8 <- data.frame(ibcf = predictions_7,
                            ubcf = predictions_6) %>%
  transmute(preds = opt_p*ibcf+(1-opt_p)*ubcf) %>%
  .$preds

# calculate rmse for the new model and add it to the results data frame
rmse_8 <- RMSE(edx_test$rating, predictions_8)
rmse_results <- rbind(rmse_results, data.frame(Method = "User-Item-Based Collaborative Filtering (Ensemble)",
                                               RMSE = rmse_8))

knitr::kable(rmse_results) # view all of the RMSE values. model 8 performs the best




#########################################################################
# user-item-based CF performance assessed on validation set - FINAL MODEL
#########################################################################

# the uibcf model performed best in terms of RMSE, so a new model of the same style will be constructed using the whole edx data set
# this model's RMSE will be assessed using the validation set, which has not been used so far

# create matrix (each row is a user and each column is a movie. the values are the ratings)
movie_bias_reg_final <- edx %>%
  group_by(movieId) %>%
  summarise(b_m_reg = sum(rating - mu)/(n()+lambda_m))

# define the regularised movie bias with the optimal lambda
user_bias_reg_final <- edx %>%
  left_join(movie_bias_reg_final, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u_reg = sum(rating - b_m_reg - mu)/(n()+lambda_u))

# new user-movie matrix accounting for entire edx data set
edx_mat <- edx %>%
  left_join(movie_bias_reg_final) %>%
  left_join(user_bias_reg_final) %>%
  mutate(residual = rating - mu_train - b_m_reg - b_u_reg) %>%
  select(userId, movieId, residual) %>%
  spread(movieId, residual)
rownames(edx_mat) <- edx_mat$userId                        # define row names (userId) which are in the first column of the matrix
edx_mat %<>% select(-userId)                               # remove the first column (which had the userId values)
edx_mat <- as.matrix(edx_mat)                              # coerce to matrix
dimnames(edx_mat) <- list(userId = dimnames(edx_mat)[[1]], # add dimension names
                          movieId = dimnames(edx_mat)[[2]])

# coercing the matrix to a realRatingMatrix as required by recommenderlab
edx_r <- as(edx_mat, "realRatingMatrix")
edx_rm <- edx_r@data

# the dimension names of the matrix are ignored in Nikolic's code, so some sort of mapping is required
# to link matrix indices to user nd movie IDs. for instance, entry 300,300 is not user 300 and movie 300
# this step is crucial in ensuring the returned predictions get matched up with the correct user/movie IDs
row_map_edx_final <- data.frame(userId = dimnames(edx_mat)[[1]] %>% as.integer,
                          i = 1:dim(edx_mat)[1])
col_map_edx_final <- data.frame(movieId = dimnames(edx_mat)[[2]] %>% as.numeric,
                          j = 1:dim(edx_mat)[2])

# define the missing entries in edx which are in test these are the entries we want to predict
validation_subset <- validation %>%
  select(userId, movieId) %>%
  left_join(row_map_edx_final) %>%
  left_join(col_map_edx_final)

# run the ubcf algorithm
# Nikolic's code requires that the transpose of the matrix be used for ubcf
preds_ubcf_final <- predict_cf(t(edx_rm), validation_subset %>% select(j, i) %>% as.matrix, alg_method = "ubcf", normalization = FALSE, similarity_metric = cal_cos,
                               k = 100, make_positive_similarities = FALSE, rowchunk_size = 15000, columnchunk_size = 1000)

# convert results into a more usable format
preds_ubcf_final %<>% t %>% summary %>% as.data.frame
preds_ubcf_final <- validation_subset %>%
  left_join(preds_ubcf_final, by = c("i","j")) %>%
  select(-i, -j)

# note that some values are NA. this is due to the nature of the algorithm. sometimes it's impossible to determine similarity measures
# between users/movies due to the matrix being so sparse
is.na(preds_ubcf_final$x)
preds_ubcf_final[is.na(preds_ubcf_final$x),"x"] <- 0

# again, the regularised parameter estimates have to be added to achieve final predictions
predictions_u <- validation %>%
  mutate(temp = preds_ubcf_final$x) %>%
  left_join(movie_bias_reg_final) %>%
  left_join(user_bias_reg_final) %>%
  mutate(final_test = temp + mu + b_m_reg + b_u_reg) %>%
  .$final_test

predictions_u[predictions_u<0.5] <- 0.5  # keep ratings within realistic bounds
predictions_u[predictions_u>5] <- 5      # keep ratings within realistic bounds

# this approach is much the same as the last
preds_ibcf_final <- predict_cf(edx_rm, validation_subset %>% select(i, j) %>% as.matrix, alg_method = "ibcf", normalization = FALSE, similarity_metric = cal_cos,
                               k = 100, make_positive_similarities = FALSE, rowchunk_size = 18000, columnchunk_size = 5500)

# convert results into a more usable format
preds_ibcf_final %<>% summary %>% as.data.frame
preds_ibcf_final <- validation_subset %>%
  left_join(preds_ibcf_final, by = c("i","j")) %>%
  select(-i, -j)

# again, some values are missing
is.na(preds_ibcf_final$x)
preds_ibcf_final[is.na(preds_ibcf_final$x),"x"] <- 0

# again, the regularised parameter estimates have to be added to achieve final predictions
predictions_i <- validation %>%
  mutate(temp = preds_ibcf_final$x) %>%
  left_join(movie_bias_reg_final) %>%
  left_join(user_bias_reg_final) %>%
  mutate(final_test = temp + mu + b_m_reg + b_u_reg) %>%
  .$final_test

predictions_i[predictions_i<0.5] <- 0.5  # keep ratings within realistic bounds
predictions_i[predictions_i>5] <- 5      # keep ratings within realistic bounds

# calculate the (weighted) mean predictions using the optimal p
predictions_final<- data.frame(ibcf = predictions_i,
                               ubcf = predictions_u) %>%
  transmute(preds = opt_p*ibcf+(1-opt_p)*ubcf) %>%
  .$preds

# calculate rmse for the final model
rmse_final <- RMSE(validation$rating, predictions_final)
final_results <- data.frame(Method = "User-Item-Based Collaborative Filtering (Ensemble)",
                            RMSE = rmse_final)
knitr::kable(final_results) # display the final rmse


