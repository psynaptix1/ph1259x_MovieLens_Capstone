
RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}

if(!require(tidyverse)) 
    install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
    install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) 
    install.packages("kableExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(kableExtra)

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"),
                         simplify = TRUE), stringsAsFactors = FALSE)

colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"),
                         simplify = TRUE), stringsAsFactors = FALSE)

colnames(movies) <- c("movieId", "title", "genres")

movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier

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

# Quick look at data frame
head(edx, 5) %>% knitr::kable(booktabs = TRUE)  %>%
  kable_styling(latex_options = c("striped", "scale_down"))

str(edx)

# Summarized data frame with mean rating and timestamps
summary(edx) %>% knitr::kable(booktabs = TRUE, caption = "Summary Statistics") %>%
  kable_styling(latex_options = c("striped", "scale_down"))

edx %>%
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId),
            n_genres = n_distinct(genres)) %>% 
            knitr::kable(caption = "Unique Values") %>% 
            kable_styling(position = "center", 
            latex_options = c("scale_down", "scale_down"))

# Convert timestamp to datetime
edx <- edx %>%
  mutate(datetime = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) %>%
  select(-timestamp) %>%
  relocate(datetime, .after = rating) %>%
  tibble()

final_holdout_test <- final_holdout_test %>%
  mutate(datetime = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) %>%
  select(-timestamp)  %>%
  relocate(datetime, .after = rating) %>%
  tibble()

# Extract release year from title
edx <- edx %>%
  mutate(release_year = as.integer(str_extract(title, "(?<=\\()\\d{4}(?=\\))"))) %>%
  relocate(release_year, .after = title)
  

final_holdout_test <- final_holdout_test %>%  
  mutate(release_year = as.integer(str_extract(title, "(?<=\\()\\d{4}(?=\\))"))) %>%
  relocate(release_year, .after = title)  

# Calculate movie age
edx <- edx %>%
  mutate(movie_age = 2024 - release_year) %>%
  relocate(movie_age, .after = release_year)

final_holdout_test <- final_holdout_test %>%  
  mutate(movie_age = 2024 - release_year) %>%
  relocate(movie_age, .after = release_year)

ratings_per_user <- edx %>%
                    group_by(userId) %>%
                    summarize(nb_ratings = n()) %>%
                    ungroup()
                    
summary(ratings_per_user) %>% 
knitr::kable(booktabs = TRUE, caption = "Ratings per User Summary Statistics") %>%
kable_styling(position = "center", latex_options = c("striped", "scale_down"))

length(which(ratings_per_user$nb_ratings <= 20))

ratings_per_user %>%
  ggplot(aes(x = nb_ratings)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  scale_x_log10() + 
  labs(title = "Log Transformed Distribution of Ratings per User",
       x = "Log Number of Ratings",
       y = "Number of Users") +
  theme(plot.title = element_text(size = 16 , face = "bold", hjust = 0.5)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

ratings_per_movie <- edx %>%
                    group_by(movieId) %>%
                    summarize(nb_ratings = n()) %>%
                    ungroup()

summary(ratings_per_movie) %>% 
knitr::kable(booktabs = TRUE, caption = "Ratigns per Movie Summary Statistics") %>%
kable_styling(position = "center", latex_options = c("striped", "scale_down"))

ratings_per_movie %>%
  ggplot(aes(x = nb_ratings)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  scale_x_log10() + 
  labs(title = "Log Transformed Distribution of Ratings per Movie",
       x = "Log Number of Ratings",
       y = "Number of Movies") +
  theme(plot.title = element_text(size = 16 , face = "bold", hjust = 0.5)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

ratings_per_month <- edx %>%
                    mutate(datetime = as.Date(datetime),
                     datetime = make_date(year(datetime), month(datetime))) %>%
                    group_by(datetime) %>%
                    summarize(nb_ratings = n()) %>%
                    ungroup()

summary(ratings_per_month) %>%
knitr::kable(booktabs = TRUE, caption = "Monthly Ratings Summary Statistics") %>%
kable_styling(position = "center", latex_options = c("striped", "scale_down"))  

ratings_per_month %>%
  ggplot(aes(x = datetime, y = nb_ratings)) +
  geom_point(color = "deepskyblue4", size = 1.5) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 300000, 50000), labels = scales::comma) +
  labs(title = "Distribution of Monthly Ratings",
         x = "Rating Date",
         y = "Number of Ratings") +
  geom_rug(color = "deepskyblue4") +
  geom_smooth(color = "black") +
  theme_linedraw() +
  theme(plot.title = element_text(size = 18 , face = "bold", hjust = 0.5)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, fill = "darkgoldenrod1", color = "darkgray") +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  ylab("Distribution of Rating Values") +
  xlab("Rating Value") +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Ratings (0 - 5)") +
  theme(plot.title = element_text(size = 18 , face = "bold", hjust = 0.5)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

avg_rating_per_user <- edx %>%
                      group_by(userId) %>%
                      summarize(avg_rating = mean(rating)) %>%
                      ungroup()

summary(avg_rating_per_user) %>% 
knitr::kable(booktabs = TRUE, caption = "Summary Statistics") %>%
  kable_styling(position = "center", latex_options = c("striped", "scale_down"))

avg_rating_per_user %>%
  ggplot(aes(x = avg_rating, fill = after_stat(density))) +
  geom_histogram(bins = 30, color = "darkgray") +
  scale_fill_gradient(low="deepskyblue2", high="darkgoldenrod2") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  theme(plot.title = element_text(size = 18 , face = "bold", hjust = 0.5)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

ratings_avg_movie_age <- edx %>%
                        group_by(movie_age) %>%
                        summarize(avg_rating = mean(rating)) %>%
                        ungroup()

summary(ratings_avg_movie_age) %>% 
knitr::kable(booktabs = TRUE, caption = "Summary Statistics") %>%
kable_styling(position = "center", latex_options = c("striped", "scale_down"))

ratings_avg_movie_age %>%
ggplot(aes(x = movie_age, y = avg_rating)) +
  geom_point(color = "deepskyblue4", size = 1.5) +
  geom_smooth(color = "black") +
  geom_rug(color = "deepskyblue4") +
  labs(title = "Mean Rating vs. Movie Age",
       x = "Movie Age",
       y = "Average Rating") +
  theme(plot.title = element_text(size = 18 , face = "bold", hjust = 0.5)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

# Compute the dataset's mean rating
mu <- mean(edx$rating)

# Test results based on simple prediction
naive_rmse <- RMSE(edx$rating, mu)

# Check results
# Save prediction in data frame
rmse_results <- tibble(method = "Average movie rating model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

movie_bias <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_bias %>%
ggplot(aes(b_i)) +
geom_histogram(bins=30, color = "darkgray", fill = "darkred") +
ylab("Number of Movies")  +
ggtitle("Distribution of Movie Bias") +
theme(plot.title = element_text(size = 18 , face = "bold", hjust = 0.5)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"),
        axis.title.x = element_blank())

# Test and save rmse results 
predicted_ratings <- mu + edx %>%
  left_join(movie_bias, by='movieId') %>%
  pull(b_i)
model_2_rmse <- RMSE(edx$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Bias model",  
                                     RMSE = model_2_rmse ))
# Check results
rmse_results %>% knitr::kable()

user_bias <- edx %>% 
  left_join(movie_bias, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

user_bias %>% 
ggplot(aes(b_u)) +
geom_histogram(bins = 30, color = "black", fill = "coral") +
ylab("Number of Movies") +
ggtitle("Distribution of User Bias") +
theme(plot.title = element_text(size = 18 , face = "bold", hjust = 0.5)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"),
        axis.title.x = element_blank())

# Test and save rmse results 
predicted_ratings <- edx %>%
left_join(movie_bias, by='movieId') %>%
left_join(user_bias, by='userId') %>%
mutate(pred = mu + b_i + b_u) %>%
pull(pred)

model_3_rmse <- RMSE(edx$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results, tibble(method="Movie and User Bias model",  
                          RMSE = model_3_rmse))

# Check result
rmse_results %>% knitr::kable()

lambdasReg <- seq(0, 10, 0.25)

# Function to test lambdas
RMSEreg <- sapply(lambdasReg, function(l){

  mu <- mean(edx$rating)
  
  b_m <- edx %>%
    group_by(movieId) %>%
    summarize(bm = sum(rating - mu)/(n() + l))
  
  b_u <- edx %>%
    left_join(b_m, by = 'movieId') %>% 
    group_by(userId) %>%
    summarize(bu = sum(rating - bm - mu)/(n() + l))
  
  predicted_ratings <- edx %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + bm + bu) %>%
    pull(pred)
  
return(RMSE(edx$rating, predicted_ratings))
})

lambda <- lambdasReg[which.min(RMSEreg)]

lambda

ggplot(mapping = aes(x = lambdasReg, y = RMSEreg)) +
  geom_point(color = "darkmagenta", size = 1.5) +
  labs(title = "Distribution of Lambdas",
         x = "Lambda",
         y = "RMSE") + 
         theme(plot.title = element_text(size = 18 , face = "bold", hjust = 0.5)) +
         theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

b_i_reg <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lambda))

b_u_reg <- edx %>% 
  left_join(b_i_reg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() + lambda))

pred_reg <- edx %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE_4 <- RMSE(edx$rating, pred_reg)

result4_table <- tibble(Model = "Regularised Movie & User Biases", RMSE = RMSE_4)

result4_table %>% knitr::kable()

results_table <- tibble(Model = c("Benchmark", "Movie Bias",
    "Movie & User Biases", "Regularised Movie & User Biases"),
      RMSE = c(naive_rmse, model_2_rmse, model_3_rmse, RMSE_4))

results_table %>% knitr::kable()

b_if <- final_holdout_test %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lambda))

b_uf <- final_holdout_test %>% 
  left_join(b_if, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() + lambda))

pred_regf <- final_holdout_test %>% 
  left_join(b_if, by = "movieId") %>%
  left_join(b_uf, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE_finalHT <- RMSE(final_holdout_test$rating, pred_regf)

resultfinal2_table <- tibble(Model = "Regularised Movie & User Biases",
                             RMSE = RMSE_finalHT)

resultfinal2_table %>% knitr::kable()

print("Operating System:")
version
