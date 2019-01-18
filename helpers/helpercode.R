library(proxy)
library(recommenderlab)
library(reshape2)
library(dplyr)
movies <- read.csv("data/movies.csv", header = TRUE, stringsAsFactors=FALSE)
ratings <- read.csv("data/ratings.csv", header = TRUE)
tagsmovie <- read.csv("data/tags.csv")

movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
search <- read.csv("helpers/search.csv", stringsAsFactors=FALSE)
search_matrix2 <- merge(search, movies, by = c("movieId")) %>%
  select(-X, -title.y) %>%
  rename(title = title.x)

movie_recommendation <- function(movieId_input, rating_input) {
  userSelect <- matrix(NA,9724)   
  for (i in 1:length(movieId_input)){
    row_num <- which(movies[,1] == movieId_input[i])
    userSelect[row_num] <- rating_input[i]
  }
  
  userSelect <- t(userSelect)

  ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
  ratingmat <- ratingmat[,-1]
  colnames(userSelect) <- colnames(ratingmat)
  ratingmat2 <- rbind(userSelect,ratingmat)
  ratingmat2 <- as.matrix(ratingmat2)

  #Convert rating matrix into a sparse matrix
  ratingmat2 <- as(ratingmat2, "realRatingMatrix")

  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=30))
  n=100
  recom <- predict(recommender_model, ratingmat2[1], n=n)
  recom_list <- as(recom, "list")
  no_result <- data.frame(matrix(NA,1))
  recom_result <- data.frame(matrix(NA,n))
  
  if (as.character(recom_list[1])=='character(0)'){
    no_result[1,1] <- "Sorry, there is not enough information in our database on the movies you've selected. Try to select different movies you like."
    colnames(no_result) <- "No results"
    return(no_result) 
  } else {
    for (i in c(1:n)){
      recom_result[i,1] <- as.character(subset(movies, 
                                               movies$movieId == as.integer(recom_list[[1]][i]))$title)
    }
  colnames(recom_result) <- "title"
  recom_result$rank <- c(1:nrow(recom_result))
  
  recom_result2 <- recom_result
  recom_result2$pred_rating <- recom@ratings[[1]]
  
  recom_df2 <- merge(recom_result2, new_df, by = c("title")) %>%
    arrange(desc(pred_rating), desc(n), desc(year))

  recom_df3 <- recom_df2[,1:24] %>% select(-pred_rating)
  
  recom_df <- merge(recom_result, search_matrix2, by = c("title")) %>%
    arrange(rank) 
  
  recom_result2[1:10,]
  recom_df[1:10,]
  recom_df2[1:10,]
  
  return(recom_df3)
  }
}



movierating <- movies %>%
  inner_join(ratings, by = "movieId") %>%
  mutate(timestamp = as_datetime(timestamp), 
         year = year(timestamp), 
         wdays = wday(timestamp, label = T, abbr = F), 
         hour = hour(timestamp), 
         title = as.character(title))



#wordcloud
genrelist <- movierating %>%
  na.omit() %>%
  select(movieId, year, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  inner_join(tagsmovie, by = "movieId") %>%
  select(genres, tag) %>%
  group_by(genres) %>%
  nest() %>%
  filter(!genres == "(no genres listed)")


#clusteringplot
hc <- movierating %>%
  na.omit() %>%
  select(movieId, rating, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  mutate(genres = as.factor(genres)) %>%
  group_by(genres) %>%
  summarise(number = n(), rating = mean(rating)) %>%
  filter(!genres == "(no genres listed)") %>%
  ungroup() %>%
  mutate(number = scale(number),
         rating = scale(rating)) %>%
  column_to_rownames(var = "genres") %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")



cluscale <- movierating %>%
  na.omit() %>%
  select(movieId, rating, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  mutate(genres = as.factor(genres)) %>%
  group_by(genres) %>%
  summarise(number = n(), rating = mean(rating)) %>%
  filter(!genres == "(no genres listed)") %>%
  ungroup() %>%
  mutate(number = scale(number),
         rating = scale(rating)) %>%
  column_to_rownames(var = "genres")



rating_mean <- ratings %>% 
  group_by(movieId) %>%
  summarise(rating_mean = mean(rating))
rating_n <- ratings %>% 
  group_by(movieId) %>%
  summarise(n = n())
merge <- merge(search_matrix2, rating_mean, by = "movieId", sort=T)
merge <- merge(merge, rating_n, by = "movieId", sort=T)
new_df <- merge %>% arrange(desc(n), desc(rating_mean))
colnames(new_df)



withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

appCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"
