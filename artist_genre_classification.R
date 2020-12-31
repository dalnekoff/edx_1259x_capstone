#EDX Capstone
#ddalnekoff
#You Wanted A Hit?
#Artist Genre Grouping Work
#A note on this file: This includes work I started to attempt to identify and assign a "major" genre to each artist in order to add additional variables to the dataset for the capstone project.

#In the interest of time, I had to put this on shelf, but when the time permits I will re-visit this work to help further enrich our data to support a more accurate model.  


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(request)) install.packages("request", repos = "http://cran.us.r-project.org")
if(!require(igraph)) install.packages("igraph", repos = "http://cran.us.r-project.org")
if(!require(kknn)) install.packages("kknn", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")

if(!require(matrixstats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")

library(matrixStats)


data <- read_csv("~/R/capstone/data.csv")
data_artist <- read_csv("~/R/capstone/data_by_artist.csv")
data_genres <- read_csv("~/R/capstone/data_by_genres.csv")
data_year <- read_csv("~/R/capstone/data_by_year.csv")
data_w_genres <- read_csv("~/R/capstone/data_w_genres.csv")


#make a mapping of consolidated artist genre to song
#steps:
#    "split_data" 1) get songs to a list of artist & artists

split_data <- stack(setNames(strsplit(data$artists,','), data$artists)) %>% distinct()

split_data %>% filter(values == 'Sergei Rachmaninoff')

#     "artist_split_song_list" 2) get a distinct list of all songs and each individual artist (now duplicated by song)

# too much memory temp_split_song_data <- unique(data, by = c("artists", "name")) %>% select(artists, name)
#this adds several hundred MB in data
#alternate approach

temp_split_song_data <- data %>% select(artists, name) %>% distinct()
# no memory change

artist_split_song_list <- temp_split_song_data %>% select (artists, name) %>% mutate(ind = artists) %>% left_join(split_data, by = 'ind' ) %>% distinct()
#1GB

artist_split_song_list <- artist_split_song_list %>% select(artists, name, values) %>% distinct()
#no mem change
#need to redesign the above staging step to be more memory efficient...the initial duplicated list is too large


#doing as a single step won't work
#artist_split_song_list <-   %>% select(artists, name) %>% %>% mutate(ind = artists) %>% left_join(split_data, by = 'ind' ) %>% unique(., by = c("artists","name","values")) %>% select(artists, name, values)



artist_split_song_list %>% filter(values == 'Sergei Rachmaninoff')
rm(artist_split_song_list)


#build the artist genre list
artist_genre <- data_w_genres %>% select( artists, genres) %>% mutate(artist_clean = artists) %>% distinct()

artist_genre %>% filter( artists == 'Wookiefoot') %>% select(artists,genres, artist_clean)


#     3) get artist and their genres
artist_genre_list<- stack(setNames(strsplit(artist_genre$genres,','), artist_genre$artists)) %>% distinct()



#clean the genre list
artist_genre_list <- artist_genre_list %>% mutate( values = gsub("\\[|\\]", "",values))
artist_genre_list <- artist_genre_list %>% mutate( values = gsub("\\'|\\'", "",values))
artist_genre_list <- artist_genre_list %>% mutate( genre = values, artist_clean = ind) %>% select(artist_clean, genre) %>% distinct()

#   artist genres 4) join in the genres and for each artist break out the list of genres into unique elements
#add genres to the list of artist, artists, song

artist_genre_temp <- artist_genre_list %>% mutate(values = artist_clean) %>% select(values, genre) %>% distinct()

artist_split_song_genre_list <- artist_split_song_list %>% left_join(artist_genre_temp, by = "values") %>% distinct()


# now build a list of unique genres from the artists for each song
#select song, comma separated list of genres that is unique
artists_name_genres <- artist_split_song_genre_list %>% group_by(artists, name) %>% summarise( genres = toString(genre)) %>% ungroup() %>% distinct()

artists_genre_list <- stack(setNames(strsplit(artists_name_genres$genres,','), artists_name_genres$artists)) %>% distinct()

artists_genre_list <- artists_genre_list %>% mutate( ind = gsub("\\[|\\]", "",ind))
artists_genre_list <- artists_genre_list %>% mutate( ind = gsub("\\'|\\'", "",ind))

artists_genre_list <- artists_genre_list %>% mutate( genre = values, artists = ind) %>% select(artists,genre) %>% distinct()

artists_genre_list <- artists_genre_list %>% mutate(artists = str_squish(artists), genre = str_squish(genre))


#12/5 working here
#the goal is to categorize genres so there are not so many different values
head(artists_genre_list)
temp_genre_rank <- artists_genre_list %>% group_by(genre) %>% summarize( counts = n()) 
temp_genre_rank <- temp_genre_rank[order(-temp_genre_rank$counts),]

temp_genre_rank <- temp_genre_rank %>% mutate( newgenre = 
                                                 ifelse(genre == 'NA', '',
                                                        ifelse(genre == '' , '',
                                                               ifelse(genre %like% 'classical', 'classical', 
                                                                      ifelse(genre %like% 'rap', 'rap', 
                                                                             ifelse(genre %like% 'hip hop', 'hip hop',
                                                                                    ifelse(genre %like% 'pop', 'pop', 
                                                                                           ifelse(genre %like% 'adult standards', 'adult standards', 
                                                                                                  ifelse(genre %like% 'romantic', 'romantic',
                                                                                                         ifelse(genre %like% 'urban', 'urban', 
                                                                                                                ifelse(genre %like% 'r&b', 'r&b',
                                                                                                                       ifelse(genre %like% 'rock', 'rock', 
                                                                                                                              ifelse(genre %like% 'latin', 'latin', 
                                                                                                                                     ifelse(genre %like% 'opera', 'opera',
                                                                                                                                            ifelse(genre %like% 'edm', 'edm', 
                                                                                                                                                   ifelse(genre %like% 'jazz', 'jazz',
                                                                                                                                                          ifelse(genre %like% 'rock', 'rock', 
                                                                                                                                                                 ifelse(genre %like% 'lounge', 'lounge', 
                                                                                                                                                                        ifelse(genre %like% 'tropic', 'tropic',              
                                                                                                                                                                               ifelse(genre %like% 'country', 'country', 0              
                                                                                                                                                                               ))))))))))))))))))))


temp_genre_rank <- temp_genre_rank %>% filter(newgenre != 0)


artists_genre_list_new <- artists_genre_list %>% left_join(temp_genre_rank, by = "genre") %>% select(artists, newgenre) %>% distinct()

#resume
artists_genre_temp <- artists_genre_list_new %>% select(artists, newgenre) %>% distinct()

artist_split_song_list <- artist_split_song_list %>% mutate( artists =  gsub("\\[|\\]", "",gsub("\\'|\\'", "",artists)))

artists_split_song_genre_list <- artist_split_song_list %>% left_join(artists_genre_temp, by = "artists") %>% distinct()

head(artist_split_song_list)
head(artists_genre_temp)

#ignore this section for now 12/4
#now join unique name artists back to genres
#working_data %>% filter(artists %like% 'Sergei') %>% select(name, artists)
#test <- working_data %>% left_join(artists_name_genres, by = c('name', 'artists'))
#test %>% filter(artists %like% 'Sergei') %>% select(name, artists, genres)

#other idea - add genres as features
#1) take artist genre, spread on genres, join back in

#not required for artists work
artist_genre_pivot <- artist_genre_list %>% dplyr::count(artist_clean, genre) %>% tidyr::spread(key = genre, value = n) %>% distinct()

artist_genre_pivot %>% filter(artist_clean %like% 'Sergei') %>% select(artist_clean, classical)



#can we group or optimize the genre list?


artists_genre_pivot <- artists_genre_list_new %>% dplyr::count(artists, newgenre) %>% tidyr::spread(key = newgenre, value = n) %>% distinct()



#clean up
artists_genre_pivot <- artists_genre_pivot %>% mutate(artists =  gsub("\\'|\\'", "",artists))
artists_genre_pivot <- artists_genre_pivot %>% mutate(artists =  gsub("\\[|\\]", "",artists))


#need to clean up NA and v1 column
artist_genre_pivot[is.na(artist_genre_pivot)] <- 0

artists_genre_pivot[is.na(artists_genre_pivot)] <- 0

#use nearZeroVar to look for cols to keep ( we can use this to remove the least common genres as there are over 4400 in the data)
#nearZeroVar recommends columns to remove due to near 0 variance
#nzv <- nearZeroVar(artist_genre_pivot)
#col_index <- setdiff(1:ncol(artist_genre_pivot), nzv)
#length(col_index)
# this came back with just 1,2 as the recommended keepers which won't work

#let's try to do this manually and take the top 5% of columns
column_sums <- colSums(artist_genre_pivot[,3:ncol(artist_genre_pivot)])
column_sums <-append(c(0,0) , column_sums)
genres_to_keep <- append('artist_clean',names(head(sort(column_sums, decreasing=TRUE), length(column_sums)*.05) ))

columns_sums <- colSums(artists_genre_pivot[,3:ncol(artists_genre_pivot)])
columns_sums <-append(c(0,0) , columns_sums)

#no longer needed after genre consolidation
#genres_to_keep_2 <- append('artists',names(head(sort(columns_sums, decreasing=TRUE), 5 ))) 
#changed from top 5% of genres to top 50
#length(columns_sums)*.005) ))

#genres_to_keep_2

#new version of artist_genre_pivot that is only the top 5% of designated genres
artist_genre_pivot_reduced <- artist_genre_pivot %>% select(genres_to_keep)

artists_genre_pivot_reduced <- artists_genre_pivot %>% select(genres_to_keep_2) %>% distinct()

#remove the column NA
artists_genre_pivot_reduced <- artists_genre_pivot[,-2]
artists_genre_pivot_reduced <- artists_genre_pivot_reduced[,-2]


#convert data frame values to 1 and 0 for the genre attributes
artists_genre_pivot_reduced <- artists_genre_pivot_reduced %>% mutate_if(is.numeric, ~1 * (. > 0))


#now join this set of genre flags (based on artists) back to our working data
data <- data %>% mutate(artists = gsub("\\'|\\'", "",gsub("\\[|\\]", "",artists))) %>% distinct()


#quick cleanup to add artists without genres into the data
artists_genre_pivot_reduced <- data %>% select(artists) %>% left_join(artists_genre_pivot_reduced, by = "artists") %>% distinct()

artists_genre_pivot_reduced[is.na(artists_genre_pivot_reduced)] <- 0

#quick data checks
working_data %>% filter(artists == "Sergei Rachmaninoff, James Levine, Berliner Philharmoniker" ) %>% select(artists)
artists_genre_pivot_reduced %>% filter(artists %like% "Sergei Rachmaninoff, James Levine, Berliner Philharmoniker" ) %>% select(artists)


working_data %>% filter(artists == "Dennis Day" ) %>% select(artists)
artists_genre_pivot_reduced %>% filter(artists %like% "Dennis Day" ) %>% select(artists)
