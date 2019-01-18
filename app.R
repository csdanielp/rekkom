library(shiny)
library(dplyr)
library(proxy)
library(recommenderlab)
library(reshape2)
library(shinyjs)
library(shinythemes)
library(data.table)
library(shinyWidgets)
library(ShinyRatingInput)
library(DT)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(factoextra)
library(wordcloud)
library(igraph)
library(nloptr)
library(minqa)
library(lme4)
library(RcppEigen)

source("helpers/helpercode.R")

genre <- sort(colnames(search)[c(5:17,19:22)])

year_list <- as.integer(unique(search$year))
year_list <- year_list[!is.na(year_list)]

movies_poster <- read.csv("data/movie_poster.csv", header = F,stringsAsFactors=FALSE)
colnames(movies_poster) <- c("movieId", "image_url" )

search_matrix3 <- merge(search_matrix2, movies_poster , by = c("movieId"), all.x=T) 

add_temp <- data.frame(
  movieId = character(),
  title = character(),
  genres = character(),
  myRating = character()
)

# which fields are mandatory
fieldsMandatory <- c("moviesInput")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

dropdownButton <- readRDS("helpers/dropdownButton.rds")

# CSS to use in the app
appCSS <- ".mandatory_star { color: red; }"

ui <- navbarPage("Rekkom",
                 theme = shinytheme("flatly"),
                 tabPanel("Home",
                          tags$head(
                            tags$style(HTML('@import url("//fonts.googleapis.com/css?family=Lobster|Cabin:400,700");
                                            @font-face {
                                              font-family: "Lato";
                                              font-style: normal;
                                              font-weight: 400;
                                              src: url(../fonts/Lato_400.ttf) format("truetype");
                                            }
                                            body {
                                              height: 100%;
                                              margin: 0;
                                              font-family: Arial, Helvetica, sans-serif;
                                            }
                                            .hero-image {
                                              background-image: url("deadpool-2-movie-cinema-01.jpg");
                                              background-color: #ffffff;
                                              height: 750px;
                                              background-position: center;
                                              background-repeat: no-repeat;
                                              background-size: cover;
                                              margin-left:-15px;
                                              margin-right:-15px;
                                              margin-top:-25px;
                                            }
                                            .btn-primary {
                                              font-weight: 800;
                                            }
                                            .hero-text {
                                              text-align: center;
                                              position: absolute;
                                              top: 50%;
                                              left: 50%;
                                              transform: translate(-50%, -50%);
                                              color: white;
                                            }
                                            .font-home {
                                              font-size: 6vw;
                                              font-family: "Lobster", cursive;
                                              font-weight: 500;
                                              line-height: 1.1;
                                              color: #ffffff;
                                            }
                                            div.transbox {
                                              margin: 30px;
                                              background-color: #ffffff;
                                                border: 1px solid black;
                                              opacity: 0.6;
                                              filter: alpha(opacity=60); /* For IE8 and earlier */
                                            }
                                            #start {
                                            background-color:#e1543b;
                                              color: white;
                                            }
                                            p {
                                              font-family: "Lato";
                                              font-size: 20px;
                                              font-weight: 500;
                                              line-height: 1.1;
                                              color: #ffffff;
                                            }
                            '))
                          ),
                          tags$div(class = "hero-image",
                                   br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                   column(12, align="center", offset = 0,
                                          tags$div(class = "font-home",
                                                   "Find Out What To Watch")),
                                   column(6, align="center", offset = 3,
                                          br(),
                                          tags$p("Rekkom is a recommendation engine to show you what movies to watch next, by using Memory-Based Collaborative Filtering Machine Learning algorithms.")),
                                   column(6, align="center", offset = 3,
                                          br(),
                                          actionButton("start", "Check It Out!", class = "btn-primary"))      
                          )
                 ),
                 tabPanel("Get Yours",
                          shinyjs::useShinyjs(),
                          shinyjs::inlineCSS(appCSS),
                          sidebarLayout(
                            sidebarPanel(
                              tags$h3("Step 1: Select movies you have watched"),
                              htmlOutput("years_input"),
                              htmlOutput("genres_input"),
                              htmlOutput("movies_input"),
                              column(width=6,withBusyIndicatorUI(
                                actionButton("submit", "Add movies >", class = "btn-warningg")
                              )) 
                            ),
                            mainPanel(
                              fluidRow(
                                box(width = 12, title = "Step 2: Rate the movies", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                    div(class = "rateitems",
                                        uiOutput('ratings')
                                    )
                                )
                              ),
                              withBusyIndicatorUI(
                                actionButton("btn2", "Done", class = "btn-warninggg")),
                              tags$h3("Step 3: Discover movies you might like"),
                              uiOutput("results")
                            )
                          ), id="form"
                 ),
                 tabPanel("Visualization",
                          includeCSS("www/movies.css"),
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              
                              h4("Custom your Plot1"),
                              selectizeInput(inputId = 'lineplot',
                                             labelMandatory('Multi-select the Genre: '),
                                             choices = unique(genrelist)[[1]],
                                             multiple = TRUE, 
                                             selected = c("Action","Animation","Comedy","Sci-Fi")),
                              
                              radioButtons("typeplot", 
                                           "Choose the type plot:", 
                                           c("Stacked Area Plot " = "area",
                                             "Line Plot" = "line"),
                                           "line"),
                              br(),
                              h4("Custom your Plot2"),
                              sliderInput("WC","Max Words:",50,200,50),
                              
                              selectInput("genrewc","Choose genre list: ", 
                                          choices = unique(genrelist)[[1]],
                                          selected = "Action"),
                              br(),
                              h4("Custom your Plot3"),
                              radioButtons("typeclust", "Choose the type plot:", c("phylogenetic" = "pylo",
                                                                                   "dendogram" = "rect",
                                                                                   "cluster" = "clus")),
                              
                              sliderInput("k", "Number of k:", 2,6,3)
                              
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(title = "Plot1", 
                                         h4("Genre Popularity by Time"),
                                         column(
                                           plotOutput("plot1", width = "900px", height = "500px"),
                                           width = 9
                                         )
                                ),
                                tabPanel(title = "Plot2",
                                         h4("Genre Wordcloud"),
                                         column(
                                           plotOutput("plot2", width = "800px", height = "600px"),
                                           width = 9
                                         )
                                ),
                                tabPanel(title = "Plot3",
                                         h4("Genre Similarity"),
                                         column(
                                           plotOutput("plot3", width = "800px", height = "500px"),
                                           width = 9
                                         )
                                )
                              )
                            ) 
                          )
                 ),
                 id="rekkom"
)



server <- function(input, output, session) {
  
  
  observeEvent(input$start, {
    updateNavbarPage(session, "rekkom", selected = "Get Yours")
  })  
  
  
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != "" 
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  
  output$years_input = renderUI({ 
    sliderInput(inputId = "yearsInput", 
                label = "Filter by Year",
                min = min(year_list),
                max = max(year_list),
                value = c(min, max), sep=""
    )
  })
  
  
  output$genres_input = renderUI({
    dropdownButton(
      label = "Filter by Genre", 
      status = "default", 
      width = 450,
      
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput(inputId = "check1a", label = NULL, choices = genre[1:6])
        ),
        column(
          width = 4,
          checkboxGroupInput(inputId = "check1b", label = NULL, choices = genre[7:12])
        ),
        column(
          width = 4,
          checkboxGroupInput(inputId = "check1c", label = NULL, choices = genre[13:17])
        )
      )
    )
  })
  
  
  output$movies_input = renderUI({
    validate( # to prefent the delayed time for processing the renderUI
      need(input$yearsInput != "", "Counting in One, Two, and Three") # display custom message in need
    )
    
    col_genre <- NULL
    check1a <- input$check1a
    check1b <- input$check1b
    check1c <- input$check1c

    col_genre <- c(col_genre, check1a, check1b, check1c)

    col_filter <- c("movieId","title","dummy",col_genre) 

    movie_list <- search[-which((search$movieId %in% ratings$movieId) == FALSE),] %>%
      mutate(dummy = 1) %>%
      filter(year >= input$yearsInput[1] & year <= input$yearsInput[2]) %>%
      select(col_filter) %>%
      filter_at(vars(-movieId,-title), all_vars(. ==1)) %>%
      pull(title)
    
    selectizeInput(inputId = 'moviesInput',
                   labelMandatory('Multi-select the Movies '),
                   choices = movie_list,
                   multiple = TRUE
    )
  })
  

  vals<-reactiveValues()
  vals$Data<-data.table(
    Title=character(),
    Rating=character())
  
  
  df2 <- eventReactive(input$submit, {
    withBusyIndicatorServer("submit", { # showing the busy indicator
      sm <- input$moviesInput
      if (is.null(sm))
        return(NULL)
      
      n = length(input$moviesInput)
      
      new_row=data.frame(
        Title=input$moviesInput[1:n],
        Rating=4)
      
      vals$Data<-rbind(vals$Data,new_row) %>%
        distinct(Title, .keep_all = TRUE)
      vals$Data
    }) 
  }) 
  
  
  # Calculate recommendations when the done button is clicked
  df <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
      
      get_user_ratings <- function(value_list) {
        dat <- data.table(movieId = sapply(strsplit(names(value_list), "__"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                          rating = unlist(as.character(value_list)))
        dat <- dat[!is.null(rating) & !is.na(movieId)]
        dat[rating == " ", rating := 0]
        dat[, ':=' (movieId = as.numeric(movieId), rating = as.numeric(rating))]
        dat <- dat[rating > 0]

        user_ratings <- dat
        user_ratings
      }
        
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)

      recom_result <- movie_recommendation(user_ratings$movieId, user_ratings$rating)
      recom_result
    })
  }) 
  

  output$ratings <- renderUI({
    DT3 <- df2()
    DT3 <- DT3 %>% rename(title=Title) 
    
    movie_dt <- merge(DT3, search_matrix3, by = c("title"))
    movie_dt <- merge(movie_dt, new_df[,c(2,23)],by = c("title")) %>% mutate(round_rating = round(rating_mean))
    
    nb <- nrow(movie_dt)
    
    num_books <- 3
    num_rows <- ceiling(nb/3)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        if (!is.na(movie_dt[((i - 1) * num_books + j),]$movieId)){
          list(box(width = 4,
                   # div(style = "text-align:center", img(src = movie_dt$image_url[(i - 1) * num_books + j], style = "max-height:150px")),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select__", movie_dt$movieId[(i - 1) * num_books + j]), label = "", dataStop = 5, dataFractions = 2, value = movie_dt$round_rating[(i - 1) * num_books + j])),
                   div(style = "text-align:center", strong(movie_dt$title[(i - 1) * num_books + j])),
                   div(style = "text-align:center; color: #999999; font-size: 80%", movie_dt$genres[(i - 1) * num_books + j]),
                   br())) 
        }
      })))
    })
  })
  
  
  # display the recommendations
  output$results <- renderUI({
    DT <- df2()
    recom_df <- df()
    selected_movies<- DT$Title
    
    selected_rows <- search_matrix2[search_matrix2$title %in% selected_movies,]
    selected_rows <- selected_rows[,-c(1:3,22)] %>% mutate(dummy=0)
    wide <- selected_rows %>% summarise_all(funs(sum)) %>% mutate(no = 0)
    top_row_id2 <- c()
    for (i in 1:nrow(selected_rows)){
      total = nrow(selected_rows)-i+1
      col_filter <- melt(wide, id.vars="no") %>% filter(value==total) %>% pull(variable)
      col_genre2 <- genre[genre %in% col_filter]
      at <- recom_df %>% select(movieId, rank, col_genre2) %>% mutate(dummy = 0)
      top_row_id <- melt(at,id.vars = c("movieId", "rank")) %>%
        filter(value==1) %>%
        group_by(movieId, rank) %>%
        count() %>%
        arrange(desc(n), rank) %>%
        pull(movieId)
      top_row_id2 <- unique(c(top_row_id2, top_row_id))
    }

    new_rank <- as.data.frame(top_row_id2) %>% rename(movieId = top_row_id2)
    new_rank$new_rank <- c(1:nrow(new_rank))
    bottom_row <- recom_df[!recom_df$movieId %in% top_row_id2,] %>% select(movieId)
    bottom_row$new_rank <- c((nrow(recom_df) - nrow(bottom_row) + 1):nrow(recom_df))
    new_rank <- rbind(new_rank, bottom_row)

    recom_df2 <- merge(new_rank, recom_df )
    recom_df2 <- merge(recom_df2, movies_poster)
    movie_dt <- recom_df2
    movie_dt <- merge(movie_dt, new_df[,c(2,23)],by = c("title")) %>% mutate(round_rating = round(rating_mean)) %>% arrange(new_rank)
    
    num_rows <- 3
    num_books <- 3
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        list(box(width = 4,
                 # div(style = "text-align:center", img(src = movie_dt$image_url[(i - 1) * num_books + j], style = "max-height:150px")),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movie_dt$movieId[(i - 1) * num_books + j]), label = "", dataStop = 5, dataFractions = 2, value = movie_dt$round_rating[(i - 1) * num_books + j])),
                 div(style = "text-align:center", strong(movie_dt$title[(i - 1) * num_books + j])),
                 div(style = "text-align:center; color: #999999; font-size: 80%", movie_dt$genres[(i - 1) * num_books + j]),
                 br()))
      })))
    })
  })
  
  
  output$plot1 <- renderPlot({
    #lineplot
    line <- movierating %>%
      na.omit() %>%
      select(movieId, year, genres) %>%
      separate_rows(genres, sep = "\\|") %>%
      mutate(genres = as.factor(genres)) %>%
      group_by(year, genres) %>%
      summarise(number = n()) %>%
      complete(year = full_seq(year, 1), genres, fill = list(number = 0)) %>%
      filter(genres %in% c(input$lineplot)) %>%
      ggplot(aes(x=year, y = number)) +
      geom_line(aes(color=genres), size = 1.5) +
      scale_color_brewer(palette = "Paired") +
      theme_minimal() +
      labs(
        x= "year",
        y="count") +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            axis.text=element_text(size=15),
            axis.title=element_text(size=15))
    area <- movierating %>%
      na.omit() %>%
      select(movieId, year, genres) %>%
      separate_rows(genres, sep = "\\|") %>%
      mutate(genres = as.factor(genres)) %>%
      group_by(year, genres) %>%
      summarise(number = n()) %>%
      complete(year = full_seq(year, 1), genres, fill = list(number = 0)) %>%
      filter(genres %in% c(input$lineplot)) %>%
      ggplot(aes(x=year, y = number)) +
      geom_area(aes(fill=genres), alpha = 0.6, size = 1.5) +
      scale_color_brewer(palette = "Paired") +
      theme_minimal() +
      labs(x= "year",
           y="count") +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            axis.text=element_text(size=15),
            axis.title=element_text(size=15))
    eval(parse(text=input$typeplot))
  })
  
  
  output$plot2 <- renderPlot({
    plot2 <- movierating %>%
      na.omit() %>%
      select(movieId, year, genres) %>%
      separate_rows(genres, sep = "\\|") %>%
      inner_join(tagsmovie, by = "movieId") %>%
      select(genres, tag) %>%
      group_by(genres) %>%
      nest() %>%
      filter(!genres == "(no genres listed)") %>% 
      filter(genres == input$genrewc) %>%
      unnest() %>%
      mutate(tag = str_to_lower(tag, "en")) %>%
      anti_join(tibble(tag = c(tolower(input$genrewc)))) %>%
      count(tag)
    set.seed(123)
    wordcloud(words = plot2$tag, 
              freq = plot2$n, 
              colors=brewer.pal(7, "Paired"),
              max.words = input$WC)
  })
  
  
  output$plot3 <- renderPlot({
    pylo <- factoextra::fviz_dend(hc, k = input$k, k_colors = "jco",
                                  type = "phylogenic", 
                                  repel = TRUE, lwd = 10, cex = 1.7,  
                                  main = "cluster plot based on rating",
                                  ggtheme = theme_minimal())
    kmeans <- kmeans(cluscale, input$k, nstart = 25)
    clus <-  factoextra::fviz_cluster(kmeans,
                                      data=cluscale,
                                      repel = TRUE,
                                      labelsize = 15,
                                      ggtheme = theme_minimal())
    rect <- factoextra::fviz_dend(hc, k = input$k , k_colors = "jco",
                                  type = "rectangle", repel = TRUE, lwd = 1, cex = 1.2, horiz = T,
                                  ggtheme = theme_minimal())
    eval(parse(text=input$typeclust))
  })
  
}

shinyApp(ui = ui, server = server)