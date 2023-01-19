## server.R

# load functions
source('functions/system_1.R')
source('functions/system_2.R')
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 10
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = popImage[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(popMovie$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", popID$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations on click of the submit button 
  dfgenre <- eventReactive(input$genrebtn, {
    withBusyIndicatorServer("genrebtn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      genre_result = popular_genre(input$genre)
      
      #realID = which(genre_result$MovieID %in% movies$MovieID)
      recomgenre_results <- data.table(
        MovieID = genre_result$MovieID, 
        Title = genre_result$Title, 
        Ave_rating =  genre_result$ratings_per_movie, #ratings_per_movie
        Genre = genre_result$Genres,
        Image = genre_result$image_url)
      
      
    }) # still busy
    
  }) # clicked on button
  
  # Calculate recommendations on click of the submit button 
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      
      user_ratings <- get_user_ratings(value_list)
      
      top_10 = callfromUI(user_ratings$MovieID,user_ratings$Rating)
      print(top_10)
      realID = which(popMovie$MovieID %in% top_10)
      print(realID)
      recom_results <- data.table(Rank = 1:10,
                                  MovieID = realID,
                                  Title = popMovie$Title[realID],
                                  Predicted_rating = top_10)
    }) # still busy
    
  }) # clicked on button
  # display the genre recommendations
  output$genreresults <- renderUI({
    num_rows <- 1
    num_movies <- 5
    recom_result <- dfgenre()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_result$Image[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong("Title: ",recom_result$Title[(i - 1) * num_movies + j])
            ),
            div(style="text-align:center; font-size: 100%", 
                strong("Rating: ", recom_result$Ave_rating[(i - 1) * num_movies + j])
            ) 
            
        )        
      }))) # columns
    }) # rows
    
  }) # Close renderUI function
  
  
  
  
  # display the recommendations
  output$results <- renderUI({
    
    num_rows <- 2 # No. of rows
    num_movies <- 5 #No. of movies per row
    recom_result <- df()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = popImage[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong("Movie Title: ",recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # close renderUI function
}) # close server function