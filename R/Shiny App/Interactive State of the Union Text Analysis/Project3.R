# ===============================================
# Fill in the following fields
# ===============================================
# Title: State of the Union Text Analysis
# Description: This Shiny App will conduct two basic text analyses for presidential speeches given each year 
# Author: Amber Shao
# Date: 04/29/22


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)


# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")
dat <- dplyr::starwars


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("State of the Union Text Analysis"),
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           checkboxInput(inputId = "remove",
                         label = strong("Remove Stop Word"),
                         value = FALSE)
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           selectInput(inputId = "facet", 
                       label = "Facet By Which Column",
                       choices = c("None" = "None",
                                   "President" = "president",
                                   "Year" = "year",
                                   "Party" = "party"),
                       selected = "None")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           radioButtons(inputId = "order", 
                        label = "Presenting Order", 
                        choices = c("Increasing frequency" = "inc_frq",
                                    "decreasing frequency" = "dec_frq"),
                        selected = "dec_frq")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           sliderInput(inputId = "number",
                       label = "Most-frequent Words Number",
                       min = 1,
                       max = 20,
                       value = 5)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis 1",
                       h3("Word Frequency Analysis"),
                       plotOutput("barplot"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis 2", 
                       h3("Sentiment Analysis"),
                       plotOutput("histogram", 
                                  height = "500px"),
                       dataTableOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in barchart)
  dat = reactive(read.csv(file = "state-union-2001-2022.csv"))
  tokens = reactive(dat() %>% unnest_tokens(word, message))
  
  words <- reactive({
    if (input$remove == TRUE) {
      words = tokens() %>% anti_join(stop_words, by = "word")
    } else {
      words = tokens()
    }
  })
  
  #generate result table for analysis 1 
  facet = reactive({
    if ((input$facet == 'None') && (input$order == 'dec_frq')) {
      facet = words() %>% count(word, sort = TRUE) %>% ungroup() %>% arrange(desc(n)) %>% top_n(input$number) 
    } else if ((input$facet == 'None') && (input$order == 'inc_frq')) {
      facet = words() %>% count(word, sort = TRUE) %>% ungroup() %>% arrange(n) %>% top_n(input$number)
    } else if ((input$facet != 'None') && (input$order == 'dec_frq')) {
      facet = words() %>% group_by_at(vars(input$facet, word)) %>% count() %>% arrange(desc(n)) %>% 
        group_by_at(input$facet) %>% top_n(input$number)
    } else if ((input$facet != 'None') && (input$order == 'inc_frq')) {
      facet = words() %>% group_by_at(vars(input$facet, word)) %>% count() %>% arrange(n) %>% 
        group_by_at(input$facet) %>% top_n(input$number)
    }
    facet
  })
  
  #generate result graph for analysis 1
  output_ggplot_1 = reactive({
    if ((input$facet == 'None') && (input$order == 'dec_frq')) {
      output_ggplot_1 = facet() %>% ggplot(aes(x = reorder(word, n), y = n)) + 
        theme_minimal() + geom_col(fill="lightblue") + coord_flip() + 
        labs(title = "Most Common Words in State of the Union Speeches", subtitle = "Decreasing Order", 
             y = "occurance", x = 'words')
    } else if ((input$facet == 'None') && (input$order == 'inc_frq')){
      output_ggplot_1 = facet() %>% ggplot(aes(x = reorder(word, desc(n)), y = n)) + 
        theme_minimal() + geom_col(fill="lightblue") + coord_flip() + 
        labs(title = "Most Common Words in State of the Union Speeches", subtitle = "Increasing Order",
             y = "occurance", x = 'words')
    } else if ((input$facet != 'None') && (input$order == 'dec_frq')){
      output_ggplot_1 = facet() %>% ggplot() + theme_minimal() +
        geom_col(aes(reorder_within(word, n, get(input$facet)), n), fill="lightpink") +
        scale_x_reordered() +
        facet_wrap(~get(input$facet), scales = "free") + 
        xlab(NULL) + 
        coord_flip() +
        labs(title = "Most Common Words in State of the Union Speeches Facet by Column Defined Above", 
             subtitle = "Decreasing Order",
             y = "occurance", x = 'words')
    } else if ((input$facet != 'None') && (input$order == 'inc_frq')){
      output_ggplot_1 = facet() %>% ggplot() + theme_minimal() +
        geom_col(aes(reorder_within(word, desc(n), get(input$facet)), n),fill="lightpink") +
        scale_x_reordered() +
        facet_wrap(~get(input$facet), scales = "free") + 
        xlab(NULL) + 
        coord_flip() +
        labs(title = "Most Common Words in State of the Union Speeches Facet by Column Defined Above", 
             subtitle = "Increasing Order",
             y = "occurance", x = 'words')
    }
    output_ggplot_1
  })
  
  #generate the data for result graph 2
  sentiment = reactive({
    if ((input$facet == 'None') && (input$order == 'dec_frq')) {
      sentiment = words() %>% inner_join(sentiments, by="word") %>% count(word, sentiment, sort = TRUE)  %>%
        group_by(sentiment) %>% arrange(desc(n)) %>% top_n(input$number)
    } else if ((input$facet == 'None') && (input$order == 'inc_frq')) {
      sentiment = words() %>% inner_join(sentiments, by="word") %>% count(word, sentiment, sort = TRUE) %>%
        group_by(sentiment) %>% arrange(n) %>% top_n(input$number)
    } else if ((input$facet != 'None') && (input$order == 'dec_frq')) {
      sentiment = words() %>% inner_join(sentiments, by="word") %>% group_by_at(vars(word, sentiment, input$facet)) %>% 
        count() %>% arrange(desc(n)) %>% group_by_at(vars(sentiment, input$facet)) %>% top_n(input$number) 
    } else if ((input$facet != 'None') && (input$order == 'inc_frq')) {
      sentiment = words() %>% inner_join(sentiments, by="word") %>% group_by_at(vars(word, sentiment, input$facet)) %>% 
        count() %>% arrange(n) %>% group_by_at(vars(sentiment, input$facet)) %>% top_n(input$number) 
    }
    sentiment
  })
  
  #generate result table for analysis 2
  with_sentiment = reactive({words() %>% inner_join(sentiments, by="word")})
  with_number = reactive({with_sentiment() %>% 
      mutate(sentiment = replace(sentiment, sentiment == 'negative', as.integer(-1))) %>% 
      mutate(sentiment = replace(sentiment, sentiment == 'positive', as.integer(1)))})
  output2 = reactive({
    if (input$facet == 'None'){
      output2 = data.frame(sentiment_score = mean(as.integer(with_number()$sentiment)))
    } else if ((input$facet != 'None') && (input$order == 'dec_frq')){
      output2 = with_number() %>% group_by_at(input$facet) %>%
        summarise(sentiment_score = mean(as.integer(sentiment))) %>% arrange(desc(sentiment_score))
    } else if ((input$facet != 'None') && (input$order == 'inc_frq')) {
      output2 = with_number() %>% group_by_at(input$facet) %>%
        summarise(sentiment_score = mean(as.integer(sentiment))) %>% arrange(sentiment_score)
    }
    output2
  })
  
  #generate result graph for analysis 2
  output_ggplot_2 = reactive({
    if ((input$facet == 'None') && (input$order == 'dec_frq')) {
      output_ggplot_2 = sentiment() %>% ggplot(aes(x = reorder(word, n), y = n))  +
        geom_col(aes(fill = sentiment)) + facet_wrap(~ sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL,
             title = "Words that contribute to positive and negative sentiments") +
        coord_flip()
    } else if ((input$facet == 'None') && (input$order == 'inc_frq')){
      output_ggplot_2 = sentiment() %>% ggplot(aes(x = reorder(word, desc(n)), y = n))  +
        geom_col(aes(fill = sentiment)) + facet_wrap(~ sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL,
             title = "Words that contribute to positive and negative sentiments") +
        coord_flip()
    } else if ((input$facet != 'None') && (input$order == 'dec_frq')){
      output_ggplot_2 = sentiment() %>%
        mutate(word = reorder(word, n)) %>%  ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ sentiment+get(input$facet), scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL,
             title = "Words that contribute to positive and negative sentiments") +
        coord_flip()
    } else if ((input$facet != 'None') && (input$order == 'inc_frq')){
      output_ggplot_2 = sentiment() %>%
        mutate(word = reorder(word, desc(n))) %>%  ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ sentiment+get(input$facet), scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL,
             title = "Words that contribute to positive and negative sentiments") +
        coord_flip()
    }
    output_ggplot_2
  })
  
  
  
  # ===============================================
  # Outputs for the first TAB (i.e. barchart)
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({
    # replace the code below with your code!!!
    output_ggplot_1()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    # replace the code below with your code!!!
    facet()
  })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  # code for histogram
  output$histogram <- renderPlot({
    # replace the code below with your code!!!
    output_ggplot_2()
  })
  
  # code for statistics
  output$table2 <- renderDataTable({
    # replace the code below with your code!!!
    output2()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

