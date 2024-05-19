# Title: Text Analysis of 5 Female Singers' Lyrics
# Description: Shiny app to visualize the results from a text analysis performed on the lyrics of 5 female singers.
# Details: 
# Author: Anushka R
# Date:


# ===============================================
# R packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(tidytext)  # for text mining
library(DT)        # to work with HTML table widgets


# =======================================================
# Sentiment Lexicons
# =======================================================
# bing = read_csv("bing.csv", col_types = "cc")
# nrc = read_csv("nrc.csv", col_types = "cc")
# loughran = read_csv("loughran.csv", col_types = "cc")
# afinn = read_csv("afinn.csv", col_types = "cd")

# The "tidytext" package comes with the sentiments dataset
# that contains Bing's lexicon of words and 
# their associated sentiment.

# =======================================================
# Import data
# Uncomment the commands below in order to import the data
# =======================================================
dat = read_csv(
  file = "lyrics.csv",
  col_types = cols(
    artist = col_character(),
    album = col_character(),
    year = col_character(),
    song = col_character(),
    lyrics = col_character()
  ))

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  titlePanel("Text Analysis of 5 Female Singers' Lyrics"),
  hr(),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  fluidRow(
    column(3,
           p(em("Artist Input")),
           selectInput(
             inputId ="widget1",
             label = "Which singer do you want to focus on?",
             choices=list("Beyonce","Katy Perry","Pink","Rihanna","Taylor Swift"),
             selected = NULL,
             multiple = FALSE,
             selectize = TRUE,
             width = NULL,
             size = NULL
           )
    ), # closes column 1
    column(3,
           p(em("See the sentiments and N-grams of __ # of words?")), 
           numericInput(inputId = "widget2", 
                        label = "Size of the plot and table", 
                        value = 20)
    )
    , # closes column 2
    column(3,
           p(em("View sentiments unfaceted or not?")),
           sliderInput("widget3", "Unfaceted = 1, Faceted = 2",
                       min = 1, max = 2, value = 1
           )
    ), # closes column 3
    column(3,
           p(em("What kind of words tend to be associated with other words?")),
           textInput(inputId = "widget4", 
                     label = "N-gram word", 
                     value = "love")
    )
    , # closes column 4
    column(3,
           p(em("Tokenize by how many words for the table?")), 
           numericInput(inputId = "widget5", 
                        label = "N = __ in N-gram?", 
                        value = 2)
    ) # closes column 5
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Main Panel with outputs: plot and table
  # -------------------------------------------------------
  mainPanel(
    h3("Sentiment Analysis"),
    plotOutput(outputId = "plot"),
    hr(),
    h3("Bigram Analysis"),
    dataTableOutput(outputId = "table"),
  ) # closes mainPanel
  
) # closes ui



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive object(s)
  # ------------------------------------------------------------
  dat_freq <- reactive({
    beyonce_text = tibble((dat %>% filter(artist == "Beyonce"))$lyrics)
    colnames(beyonce_text)="txt"
    katy_text = tibble((dat %>% filter(artist == "Katy Perry"))$lyrics)
    colnames(katy_text)="txt"
    pink_text = tibble((dat %>% filter(artist == "Pink"))$lyrics)
    colnames(pink_text)="txt"
    rihanna_text = tibble((dat %>% filter(artist == "Rihanna"))$lyrics)
    colnames(rihanna_text)="txt"
    taylor_text = tibble((dat %>% filter(artist == "Taylor Swift"))$lyrics)
    colnames(taylor_text)="txt"
    beyonce_tokens = unnest_tokens(tbl = beyonce_text, output = word, input = txt)
    katy_tokens = unnest_tokens(tbl = katy_text, output = word, input = txt)
    pink_tokens = unnest_tokens(tbl = pink_text, output = word, input = txt)
    rihanna_tokens = unnest_tokens(tbl = rihanna_text, output = word, input = txt)
    taylor_tokens = unnest_tokens(tbl = taylor_text, output = word, input = txt)
    if (input$widget1 =="Beyonce"){
      text= beyonce_text
      tokens=beyonce_tokens
    }
    if (input$widget1 =="Katy Perry"){
      text= katy_text
      tokens=katy_tokens
    }
    if (input$widget1 =="Pink"){
      text= pink_text
      tokens=pink_tokens
    }
    if (input$widget1 =="Rihanna"){
      text= rihanna_text
      tokens=rihanna_tokens
    }
    if (input$widget1 =="Taylor Swift"){
      text= taylor_text
      tokens=taylor_tokens
    }
    bigrams <- text |>
      unnest_tokens(
        output = bigram, 
        input = txt,
        token = "ngrams", 
        n = input$widget5)
    
    # filter those containing input$widget4
    bigrams_separated <- bigrams |>
      separate(bigram, c("word1", "word2"), sep = " ")
    
    this_bigrams = bigrams |>
      filter(str_detect(bigram, input$widget4)) |>
      count(bigram, sort = TRUE, name = "count")
    
    this_bigrams |>
      slice_head(n = input$widget2)
  })
  
  
  # ------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------
  output$plot <- renderPlot({
    beyonce_text = tibble((dat %>% filter(artist == "Beyonce"))$lyrics)
    colnames(beyonce_text)="txt"
    katy_text = tibble((dat %>% filter(artist == "Katy Perry"))$lyrics)
    colnames(katy_text)="txt"
    pink_text = tibble((dat %>% filter(artist == "Pink"))$lyrics)
    colnames(pink_text)="txt"
    rihanna_text = tibble((dat %>% filter(artist == "Rihanna"))$lyrics)
    colnames(rihanna_text)="txt"
    taylor_text = tibble((dat %>% filter(artist == "Taylor Swift"))$lyrics)
    colnames(taylor_text)="txt"
    beyonce_tokens = unnest_tokens(tbl = beyonce_text, output = word, input = txt)
    katy_tokens = unnest_tokens(tbl = katy_text, output = word, input = txt)
    pink_tokens = unnest_tokens(tbl = pink_text, output = word, input = txt)
    rihanna_tokens = unnest_tokens(tbl = rihanna_text, output = word, input = txt)
    taylor_tokens = unnest_tokens(tbl = taylor_text, output = word, input = txt)
    if (input$widget1 =="Beyonce"){
      text= beyonce_text
      tokens=beyonce_tokens
    }
    if (input$widget1 =="Katy Perry"){
      text= katy_text
      tokens=katy_tokens
    }
    if (input$widget1 =="Pink"){
      text= pink_text
      tokens=pink_tokens
    }
    if (input$widget1 =="Rihanna"){
      text= rihanna_text
      tokens=rihanna_tokens
    }
    if (input$widget1 =="Taylor Swift"){
      text= taylor_text
      tokens=taylor_tokens
    }
    if(input$widget3==1){
      sentiments <- tokens |>
        inner_join(sentiments, by = "word")

      sentiments |>
        count(word, sentiment, sort = TRUE) |>
        slice_head(n = input$widget2) |>
        ggplot() +
        geom_col(aes(y = reorder(word, n), x = n, fill = sentiment)) +
        labs(title = paste0(input$widget2," most common words with an associated sentiment"),
             subtitle = input$widget1,
             y = "",
             x = "count")
    }
    else{
      bing_word_counts <- tokens %>%
        inner_join(sentiments, by = "word") %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
      bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(input$widget2/2) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL,
             title = "Words that contribute to positive and negative sentiments") +
        coord_flip()
    }
  })
  
  
  # ------------------------------------------------------------
  # Table
  # ------------------------------------------------------------
  output$table <- renderDataTable({
    dat_freq()
  })

  
} # closes server



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

