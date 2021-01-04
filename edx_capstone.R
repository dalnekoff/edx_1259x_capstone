#HarvardX: PH125.9x Data Science Capstone
#ddalnekoff 


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(request)) install.packages("request", repos = "http://cran.us.r-project.org")
if(!require(igraph)) install.packages("igraph", repos = "http://cran.us.r-project.org")
#if(!require(kknn)) install.packages("kknn", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
#if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(matrixstats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
#if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")
if(!require(BiocManager)) install.packages("BiocManager", repos = "http://cran.us.r-project.org")
if(!require(modelr)) install.packages("modelr", repos = "http://cran.us.r-project.org")



data <- read_csv("~/R/capstone/data.csv")
#data_artist <- read_csv("~/R/capstone/data_by_artist.csv")
#data_genres <- read_csv("~/R/capstone/data_by_genres.csv")
#data_year <- read_csv("~/R/capstone/data_by_year.csv")
data_w_genres <- read_csv("~/R/capstone/data_w_genres.csv")

#Perform some data cleaning to create a new column with the square brackets and single quotes removed from the list of artists.
data <- data %>% mutate( artists = gsub("\\[|\\]", "", artists))
data <- data %>% mutate( artists = gsub("\\'|\\'", "", artist_clean)) 
data <- data %>% mutate( artists = gsub("\\|\\'", "", artist_clean))
#head(working_data)

#let's find and review any NA's in the data
apply(is.na(data), 2, which)



#what is the mean popularity score of the data?
mu <- mean(data$popularity)
mu

std_error <- sd(data$popularity)
std_error

#let's look at popularity and number of observations
popularity_summary <- data %>% group_by(popularity) %>% summarize( songs = n())
plot(popularity_summary$popularity, popularity_summary$songs)


print(paste('Mean Popularity:' , floor(mean(data$popularity))))
print(paste('Standard Error:' , floor(sd(data$popularity))))


#What is considered a hit?
#https://en.wikipedia.org/wiki/Hit_song#:~:text=In%20the%20United%20States%20and,for%20at%20least%20one%20week.
#according to wikipedia, it is roughly one of the top 40 songs in a given week
#top 40 songs by week, 52 weeks/year
#this is going to extremely complicated to do, as the release date data is mostly by year
#let's add a flag to denote a "hit" song via the following steps, defined at the top 1 percentile of populairty scores in the population

quantile(data$popularity, c(.50, .90, .99))

data <- data %>% mutate( popularity_group = 
  ifelse( popularity <= quantile(data$popularity, c(.50)), 'Low',
  ifelse(popularity <= quantile(data$popularity, c(.90)), 'Medium',
  ifelse(popularity <= quantile(data$popularity, c(.99)), 'High',
  'Hit')))) 

#Convert the popularity group to a factor
data$popularity_group <- factor(data$popularity_group)

#Produce a quick summary of the number of observations in each popularity group
popularity_class_summary <- data %>% group_by( popularity_group) %>% summarise( population = n())
popularity_class_summary <- popularity_class_summary[order(-popularity_class_summary$population),]

print(popularity_class_summary)


#Let's see some of the hits, these should be recognizable
data %>% filter(popularity_group == "Hit") %>% select(name, artists, popularity)


#Let's evaluate the "artist factor"
#We can assume that some artists create more hits than others, but also once an artist has more hits, they are more popular and more likely to have additional hits as they have a larger audience.
#To help answer this question, our code will do the following:
#Create a data frame with song, artists, individual artist, and popularity_group
#We will split the list of "artists" for each song into separate elements
# For example ['John Smith','John Adams'] would become two separate observations.
#We can then analyze for each artist the "hit rate", defined as # of hits/# of songs
#We can also define for each artist the impact that artist tends to have on a song's popularity

artist_factor <- data %>% select(artists, name, popularity_group, popularity, id)
head(artist_factor)

#split observations into multiple rows for each artist
artist_bridge <- stack(setNames(strsplit(artist_factor$artists,','), artist_factor$artists)) %>% distinct()

artist_bridge <- artist_bridge %>% mutate(individual = str_squish(values), artists = ind) %>% select(artists, individual)

#combine the unique artist names back into the artists song listing
artist_factor <- artist_factor %>% left_join(artist_bridge, by = "artists") %>% distinct()

artist_factor %>% filter(individual %like% 'Yankovic' && name %like% 'stu')

artist_results <- artist_factor %>% 
  group_by(individual) %>% 
  summarize( mean_popularity = mean(popularity), artist_effect = mean(popularity - mu), hits = sum( ifelse( popularity_group == "Hit",1,0)), songs = n())

#Let's check in on a popular artist
artist_results %>% filter(individual %like% 'Yankovic')  

#Now bring the individual artist stats back to the artists - song - artist table
artist_factor <- artist_factor %>% left_join(artist_results, by = "individual")
    
#now cleanup some objects no longer needed
#rm(artist_bridge, artist_results)
artist_factor %>% filter(individual %like% 'Yankovic') %>% filter(individual %like% 'Weird')  
examples <- artist_factor %>% filter(id == "3PXi72ZtSqx1PZc40KS0Qj")


#now let's aggregate the individual artist back to the artists per song
id_stats <- artist_factor %>% group_by(id) %>% summarize( hits = sum(hits), song_count = sum(songs), artists_effect = weighted.mean(x = artist_effect, w = songs), mean_popularities = weighted.mean(x = mean_popularity, w = songs)) %>% distinct


head(artist_factor)  

#now let's join this data back to our working dataset
data <- data %>% left_join(id_stats, by = "id")


#let's take a similar approach to add in genre data
#a dataset of artist and genres has been provided with the original data
#as-is there are over 10k genre combinations across all artists
head(data_w_genres)
genres <- data_w_genres %>% select(genres) %>% distinct()
dim(genres)

#let's first take each artists' list of genres and split this into rows
artist_genre_list <- stack(setNames(strsplit(data_w_genres$genres,','), data_w_genres$artists)) %>% distinct()

#we need to do some text cleanup on the genres
#data <- data %>% mutate( artists = gsub("\\|\\'", "", artist_clean))
artist_genre_list <- artist_genre_list %>% 
  mutate( artist = ind, genres = values) %>% select(artist, genres)

artist_genre_list <- artist_genre_list %>%
  mutate( genres = str_squish(gsub("\\[|\\]", "", genres)))


#Use some logic to identify the "major" genres
#first, let's get a list of the genre frequency
genre_list <- artist_genre_list %>% select(genres)
genre_summary <- genre_list %>% group_by(genres) %>% summarize(frequency = n())

genre_summary <- genre_summary[order(-genre_summary$frequency),]

#let's combine the artist stats with the genres
artist_genre_stats <- artist_results %>% mutate(individual = str_squish(gsub("\\'|\\'", "", individual)))

artist_genre_stats <- artist_genre_stats %>% left_join(artist_genre_list, by = c("individual" = "artist"))

#do we see any relationship between artist genres and popularity?
artist_genre_view <- artist_genre_stats %>% group_by(genres) %>% summarise( weighted_mean_popularity = weighted.mean(mean_popularity, songs), total_songs = sum(songs))

#check out top rated artist genres
artist_genre_view <- artist_genre_view[order(-artist_genre_view$weighted_mean_popularity),]

#note that they have a very low # of songs
head(artist_genre_view)


#check out genres with the most number of songs
artist_genre_view <- artist_genre_view[order(-artist_genre_view$total_songs),]

#popularity is mixed
head(artist_genre_view)

# a quick plot reveals what we expected, a large mix of number of songs compared to weighted average popularity
qplot(artist_genre_view$weighted_mean_popularity, artist_genre_view$total_songs)

#Due to the large number of genres, and the association between artist and genre, instead of observation to genre has led us to make genres something to consider for the future.

#The idea of adding potentially hundreds, if not thousands of dimensions to the model, or a factor with thousands of classes will add significant complexity to this data science challenge.  In the interest of completing this work on time and leaving some future items to come back and explore we will not attempt more work at this time to gain additional model enhancements though the genre(s) variable. Please see artist_genre_classification.R for a more detailed and in-progress set of work specific to the genre feature.

rm(artist_genre_view, artist_genre_stats, artist_results, artist_bridge, artist_factor)


#let's now do some cleansing work to do the following:
#Convert some of our variables to factors
#Remove columns that are no appropriate for use
#Apply scaling where appropriate


#convert categorical columns to factors:
cats <- c('explicit','key','mode')
data[,cats] <- lapply(data[,cats] , factor)


#Are there some columns that nearZeroVar suggests to remove?
#These are columns which have near zero variance in the data and are unlikely to impact our model performance
removeColumns <- nearZeroVar(data)
#This suggests we should ignore instrumentalness, so we can remove this from the data

data <- data %>% select(-removeColumns)


#######################################################
#Additional data discovery
#
#
#
#
#########################################################



#are more recent songs more popular?
year_summary <- data %>%  group_by(year) %>% summarize( rating = mean(popularity))

plot(year_summary$year, year_summary$rating)

release_year_summary <- data %>% mutate(release_year = substr(release_date,0,4)) %>% group_by(release_year) %>% summarize(rating = mean(popularity))

plot(release_year_summary$release_year, release_year_summary$rating)


release_year_details <- data %>% mutate(release_year = substr(release_date,0,4)) %>% group_by(release_year, id) %>% summarize(rating = mean(popularity))

boxplot(rating~release_year, data = release_year_details)

#There is a clear trend that indicates more recent releases have a higher mean popularity, but what is interesting is that the "hits" - the outliers at the top are relatively flat from the 60's through the early 2000's. I found this to be a very interesting observation.

#The specifics of the approach to any data science challenge will always be nuanced and depend on the circumstances, knowledge of the data, and the end goal.  In the case of this project, we want to determine if a new song will be a hit.  One thing that we don't have the ability to do is go back in time and release a song in a prior year.  Consequently, we will remove the variables in this data which relate to release year.  

yearColumns <- c('year','release_date')

data <- data %>% select(-yearColumns)

#A note on artists, song id, name
#Artists is a character vector of considerable size, which has the potential to be very impactful to our model performance.  Rather than pass the full list of artists into our model, we will pass in the previously generated metadata which includes:
   #hits -> combined # of hits of all artists
   #song_count -> combined # of songs of all artists
   #artists_effect -> this is mean effect on popularity across all artists (weighted average of the popularity of each artists songs less the mean population popularity across all songs)

#The same thing applies to name, which is the song name.  In a future analysis, I would like to investigate if certain characteristics of the song name, for example keywords, sentiment scores, etc have an impact on the popularity.  For the purposes of this project though, we will exclude this column from the data.

#In summary, we will remove the following columns:
#name -> song name
#id -> this is a unique id from the Spotify song database
#artists -> as discussed, the list of artists
#mean_popularities -> this is represented by artists_effect, no need to introduce two directly related features into the model 

idColumns <- c('name','id', 'artists','mean_popularities')

data <- data %>% select(-idColumns)


#We can find some interesting observations by visualizing the popularity across some of the variables in the data

#It appears that we do tend to like explicit songs
boxplot(popularity~explicit, data = data)

#"Louder" songs tend to be more popular
plot(popularity~loudness, data = data)
boxplot(loudness~popularity_group, data = data)

#Speechiness is interesting, a "U" shaped popularity curve
plot(popularity~speechiness, data = data)

#Who doesn't love to dance to their favorite song?
plot(popularity~danceability, data = data)
boxplot(danceability~popularity_group, data = data)
dance_pop_summary <- data %>% group_by(popularity) %>% summarize( danceability = mean(danceability))
plot( dance_pop_summary$popularity,dance_pop_summary$danceability, xlab = "Popularity", ylab = "Mean Danceability")


#High energy songs tend to be more popular
plot(popularity~energy, data = data)
boxplot(energy~popularity_group, data = data)

#Songs with a high level of acousticness are less popular
boxplot(acousticness~popularity_group, data = data)
plot(popularity~acousticness, data = data )


#There are a number of other variables we can easily visualize to make some interesting observations of the data.

plot(popularity~valence, data = data )
boxplot(valence~popularity_group, data = data)

plot(popularity~duration_ms, data = data)
boxplot(duration_ms~popularity_group, data = data)

plot(popularity~liveness, data = data)
boxplot(liveness~popularity_group, data = data)

plot(popularity~speechiness, data = data)
boxplot(speechiness~popularity_group, data = data)

plot(popularity~tempo, data = data)
boxplot(tempo~popularity_group, data = data)

boxplot(popularity~mode, data = data)
boxplot(popularity~key, data = data)

popularity_of_hits <- data %>% filter(popularity_group == "Hit") %>% select(popularity)
popularity_of_hits[which.min(popularity_of_hits$popularity),]


#establish a dataset where we scale our numeric variables so they have the same avg and standard deviation
scale_me <- c(1,2,3,4,5,8,9,12,13,15,16,17)

data_scaled <- 
  data %>%
  mutate_at(scale_me, funs(c(scale(.))))


#for the classification models, we will remove popularity from the dataset

classification_data <- data %>% select(-popularity)
classification_data_scaled <- data_scaled %>% select(-popularity)

#for the regression models, we will remove the popularity_group from the dataset

regression_data <- data %>% select(-popularity_group)
regression_data_scaled <- data_scaled %>% select(-popularity_group)


#Set our seed for consistency
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = regression_data$popularity, times = 1, p = 0.1, list = FALSE)

r_test_data <- regression_data[test_index,]
r_train_data <- regression_data[-test_index,] 

#set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = regression_data_scaled$popularity, times = 1, p = 0.1, list = FALSE)

r_test_data_scaled <- regression_data_scaled[test_index,]
r_train_data_scaled <- regression_data_scaled[-test_index,] 

#set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = classification_data$popularity_group, times = 1, p = 0.1, list = FALSE)

c_test_data <- classification_data[test_index,]
c_train_data <- classification_data[-test_index,] 

#set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = classification_data_scaled$popularity_group, times = 1, p = 0.1, list = FALSE)

c_test_data_scaled <- classification_data_scaled[test_index,]
c_train_data_scaled <- classification_data_scaled[-test_index,] 


#define training control for 10 repetitions of 10 fold Cross validation
fitControl <- trainControl(
  method = "repeatedcv",
  verboseIter = TRUE,
  number = 10,
  repeats = 10)



#tuning grid for pls
pls_tune <- expand.grid(
  ncomp = c(25)
)

pls_tune_50 <- expand.grid(
 ncomp = c(50)
)



#for gamLoess tuning, I attempted to run groups of several different sets of testing parameters to see if there was somewhere that a significant improvement was found.  Across multiple runs of different parameters, all of the model runs land close to one another in terms of performance.

training_gamloess_tune <- expand.grid(
  #span = c(.01 , .1 , .5),
  span = seq(.01, .5, .05),
  degree = 1
)

gamloess_tune <- expand.grid(
  #span = c(.01 , .1 , .5),
  span = .01,
  degree = 1
)

#this is commented out as it appears (from errors here plus some research online) that degree = 2 does not work
#gamloess_tune_3 <- expand.grid(
#  span = c(.01, .1, .5, 1),
#  degree = 2
#)


#svmLinear tuning grid
svmLinear3_tune <- expand.grid(
  cost = seq(.01, .5, .05),
  Loss = 2
)


#for pls tuning, I wanted to try 3 scenarios, 10, 50, 25 (this was where I saw an earlier cutoff of component explanation when using 30)

r_train_pls <- 
  train(popularity ~ ., method = "pls", data = r_train_data_scaled, trControl = fitControl, verbose = TRUE, tuneGrid = pls_tune)

#this is just a quick test to see if over 25 components would get used
r_train_pls_50 <- 
  train(popularity ~ ., method = "pls", data = r_train_data_scaled, trControl = fitControl, verbose = TRUE, tuneGrid = pls_tune_50)

#this is just a quick test to see if all 10 components would get used
r_train_pls_nogrid <- 
  train(popularity ~ ., method = "pls", data = r_train_data_scaled, trControl = fitControl, verbose = TRUE, tuneLength = 10)


r_train_pls$bestTune
r_train_pls_50$bestTune
r_train_pls_nogrid$bestTune
summary(r_train_pls)
summary(r_train_pls_nogrid$finalModel)
summary(r_train_pls_50$finalModel)



#let's review how the pls model regression algorithm performs

r_train_pls$results$RMSE
r_train_pls$results$RMSE
r_train_pls_50$results$RMSE


c_train_pls <- 
  train(popularity_group ~ ., method = "pls", data = c_train_data_scaled, trControl = fitControl, tuneGrid = pls_tune, verbose = TRUE)


#let's review how the pls model classification algorithm performs
summary(c_train_pls)
c_train_pls$results$Accuracy




#train support vector machine algorithm
r_train_svmLinear3<- 
  train(popularity ~ ., method = "svmLinear3", data = r_train_data_scaled , trControl = fitControl, verbose = TRUE, tuneGrid = svmLinear3_tune)

plot(r_train_svmLinear3)
r_train_svmLinear3$results$RMSE


c_train_svmLinear3 <- 
  train(popularity_group ~ ., method = "svmLinear3", data = c_train_data_scaled, trControl = fitControl, verbose = TRUE, tuneGrid = svmLinear3_tune)


plot(c_train_svmLinear3)
c_train_svmLinear3$results$Accuracy
confusionMatrix(c_svmLinear3_results, c_test_data_scaled$popularity_group)

#plot(c_train_gamLoess)
#c_train_gamLoess$bestTune
#c_train_gamLoess$Accuracy


#gamLoess training and tuning

r_train_gamLoess <- 
  train(popularity ~ ., method = "gamLoess", data = r_train_data_scaled, trControl = fitControl, verbose = TRUE, tuneGrid = gamloess_tune)
plot(r_train_gamLoess)
r_train_gamLoess$bestTune
r_train_gamLoess$results$RMSE


c_train_gamLoess <- 
  train(popularity_group ~ ., method = "gamLoess", data = c_train_data_scaled, trControl = fitControl, verbose = TRUE, tuneGrid = gamloess_tune)

c_train_gamLoess$results$Accuracy


#quick review of all three algorithms across regression and classification approaches, and the performance of the best tuning parameter sets
r_train_gamLoess$bestTune
c_train_gamLoess$bestTune

r_train_svmLinear3$bestTune
c_train_svmLinear3$bestTune

r_train_pls$bestTune
c_train_pls$bestTune

c_train_pls$results$Accuracy
r_train_pls$results$RMSE
r_train_pls$results$RMSE[which.min(r_train_pls$results$RMSE)]

c_train_gamLoess$results$Accuracy
c_train_gamLoess$results$Accuracy[which.max(c_train_gamLoess$results$Accuracy)]

r_train_gamLoess$results$RMSE
r_train_gamLoess$results$RMSE[which.min(r_train_gamLoess$results$RMSE)]

c_train_svmLinear3$results$Accuracy
c_train_svmLinear3$results$Accuracy[which.max(c_train_svmLinear3$results$Accuracy)]

r_train_svmLinear3$results$RMSE
r_train_svmLinear3$results$RMSE[which.min(r_train_svmLinear3$results$RMSE)]
plot(r_train_svmLinear3)

#predict results
c_pls_results <- predict(c_train_pls, newdata = c_test_data_scaled)
r_pls_results <- predict(r_train_pls, newdata = r_test_data_scaled)
c_gamLoess_results <- predict(c_train_gamLoess, newdata = c_test_data_scaled)
r_gamLoess_results <- predict(r_train_gamLoess, newdata = r_test_data_scaled)
c_svmLinear3_results <- predict(c_train_svmLinear3, newdata = c_test_data_scaled)
r_svmLinear3_results <- predict(r_train_svmLinear3, newdata = r_test_data_scaled)

#review the confusion matrix output from the three classification algorithms
cm_pls <- confusionMatrix(c_pls_results, c_test_data_scaled$popularity_group)
cm_svmLinear <- confusionMatrix(c_svmLinear3_results, c_test_data_scaled$popularity_group)
cm_gamLoess <- confusionMatrix(c_gamLoess_results, c_test_data_scaled$popularity_group)

cm_pls$table
cm_svmLinear$table
cm_gamLoess$table

plot(cm_svmLinear$table, main = "SVM Classification Confusion Matrix")
plot(cm_pls$table, main = "PLS Classification Confusion Matrix")




#let's build a few data frames consisting of the test data plus the predicted values from each model (one data frame for regression and one for classification) for some quick reviews

c_test_data_scaled_with_preds <- add_predictions(c_test_data_scaled, c_train_pls, var = "c_pls", type = NULL)
c_test_data_scaled_with_preds <- add_predictions(c_test_data_scaled_with_preds, c_train_gamLoess, var = "c_gamLoess", type = NULL)
c_test_data_scaled_with_preds <- add_predictions(c_test_data_scaled_with_preds, c_train_svmLinear3, var = "c_svmLinear3", type = NULL)

r_test_data_scaled_with_preds <- add_predictions(r_test_data_scaled, r_train_gamLoess, var = "r_gamLoess", type = NULL)

r_test_data_scaled_with_preds <- add_predictions(r_test_data_scaled_with_preds, r_train_svmLinear3, var = "r_svmLinear3", type = NULL)

r_test_data_scaled_with_preds <- add_predictions(r_test_data_scaled_with_preds, r_train_pls, var = "r_pls", type = NULL)

c_test_data_scaled_with_preds %>% filter(c_pls == "Hit")
c_test_data_scaled_with_preds %>% filter(c_gamLoess == "Hit")
c_test_data_scaled_with_preds %>% filter(c_svmLinear3 == "Hit") %>% filter(popularity_group == "Hit")
c_test_data_scaled_with_preds %>% filter(popularity_group == "Hit")


#let's dive further into the pls regression results
temp_hits_pls <- r_test_data_scaled_with_preds %>% filter(popularity >= 77 | r_pls >= 77)
plot(temp_hits_pls$popularity, temp_hits_pls$r_pls, main = "PLS Regression Model Results", xlab = "Actual Hits Popularity", ylab = "Predicted Hits Popularity")
RMSE(temp_hits_pls$r_pls,temp_hits_pls$popularity)
sd(temp_hits_pls$r_pls)


#let's dive further into the gamLoess regression results
RMSE(r_gamLoess_results, r_test_data_scaled$popularity)
temp_hits_gamLoess <- r_test_data_scaled_with_preds %>% filter(popularity >= 77 | r_gamLoess >= 77)
plot(temp_hits_gamLoess$popularity, temp_hits_gamLoess$r_gamLoess, main = "gamLoess Regression Model Results", xlab = "Actual Hits Popularity", ylab = "Predicted Hits Popularity")
RMSE(temp_hits_gamLoess$popularity, temp_hits_gamLoess$r_gamLoess)
sd(temp_hits_gamLoess$r_gamLoess)

#let's dive further into the svm regression results
RMSE(r_svmLinear3_results, r_test_data_scaled$popularity)
temp_hits_svmLinear3 <- r_test_data_scaled_with_preds %>% filter(popularity >= 77 | r_svmLinear3 >= 77)
plot(temp_hits_svmLinear3$popularity, temp_hits_svmLinear3$r_svmLinear3 )
RMSE(temp_hits_svmLinear3$popularity, temp_hits_svmLinear3$r_gamLoess)
sd(temp_hits_svmLinear3$r_svmLinear3)



