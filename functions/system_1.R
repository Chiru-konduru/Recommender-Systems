library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)



#loading data
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

image = sapply(movies$MovieID, 
               function(x) paste0(small_image_url, x, '.jpg?raw=true'))


popMovie = ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID') %>% 
  filter(ratings_per_movie > 500)
popID = popMovie %>% select(MovieID)
popImage = sapply(popID, 
                  function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# convert accented characters
movies$Title[73]
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Title[73]

# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
# users section
users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

# metric 1: popularity
popular_genre = function(gfrominput)
{
  genre = ratings %>%
    group_by(MovieID) %>%
    summarize(ratings_per_movie = sum(Rating > 2)) %>%
    inner_join(movies, by = 'MovieID') %>%
    group_by(Genres) %>%
    arrange(desc = ratings_per_movie)
  genre = genre[grepl(gfrominput, genre$Genres, fixed = TRUE),]
  nr = nrow(genre)
  low = nr-5+1
  return(genre[nr:low,])
}


#metric 2: highly-rated
# popular_genre = function(gfrominput)
# {
#   genre = ratings %>%
#     group_by(MovieID) %>%
#     summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
#     inner_join(movies, by = 'MovieID') %>%
#     group_by(Genres) %>%
#     filter(ratings_per_movie > 1000) %>%
#     top_n(5, ave_ratings)
#   genre = genre[grepl(gfrominput, genre$Genres, fixed = TRUE),]
#   return(genre)
# }