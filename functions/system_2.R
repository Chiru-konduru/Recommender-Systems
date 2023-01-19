#get ratings
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')


# extract movie year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
    
# read user data
users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

# filtering if ratings_per_movie > 500 and ratings_per_user >100 to get ratings_new
popMovie = ratings %>%
  group_by(MovieID) %>%
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID') %>%
  filter(ratings_per_movie > 500)
popID = popMovie %>% select(MovieID)
freqUser = ratings %>%
  inner_join(popID, by = 'MovieID')  %>%
  group_by(UserID) %>%
  summarize(ratings_per_user = n()) %>%
  filter(ratings_per_user >100)
freqID = freqUser %>%  select(UserID)
ratings_new = ratings %>%
  inner_join(freqID, by = 'UserID')%>%
  inner_join(popID, by = 'MovieID')

# build the training matrix

i = paste0('u', ratings_new$UserID)
j = paste0('m', ratings_new$MovieID)
x = ratings_new$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmatrix = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmatrix) = levels(tmp$i)
colnames(Rmatrix) = levels(tmp$j)
Rmatrix = new('realRatingMatrix', data = Rmatrix)

####################### System 2 - Approach 2: IBCF #########################

#IBCF Model Training
rec_IBCF = Recommender(Rmatrix, method = 'IBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 20))

callfromUI = function(movieIDList, ratingList){
  # new user
  n.item = ncol(Rmatrix) 
  new.ratings = rep(NA, n.item)  
  for (i in 1:length(ratingList)){
    mid = paste("m", movieIDList[i], sep='')
    index = charmatch(mid,colnames(Rmatrix))
    new.ratings[index] = ratingList[i]
  }
  #new.ratings captured using user clicks
  new.user = matrix(new.ratings, 
                    nrow=1, ncol=n.item,
                    dimnames = list(
                      user=paste('newUser'),
                      item=colnames(Rmatrix)
                    ))
  new.Rmat = as(new.user, 'realRatingMatrix')
  # prediction
  recom = predict(rec_IBCF, new.Rmat, type = 'ratings')
  
#recommendation results from IBCF
  recom_results = data.frame(mID=dimnames(recom)[[2]],pred_ratings=as.vector(as(recom, 'matrix')))
  recom_results = recom_results[order(recom_results$pred_ratings, decreasing=TRUE),][1:100,]
  set.seed(8365)
  if (length(unique(recom_results$pred_ratings[1:100])) < 3){
    rec_10 = sample(recom_results$mID[1:100],10)
  } else if (length(unique(recom_results$pred_ratings[1:10])) == 1){
    rec_10 = sample(recom_results$mID[1:20],10)
  } else{
    rec_10 =  recom_results$mID[1:10]
  }
  rec_10 = as.numeric(sub("m", "", rec_10)) 
  rec_10
  return(rec_10)
}
