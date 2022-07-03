#installing required packages

#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("syuzhet")

#packages loaded

library(shiny)     # Hosting using Shiny Package
library(ggplot2)   # visualization
library(tm)        # text mining
library(wordcloud) # generating word cloud
library(syuzhet)   # sentiment analysis
library(packcircles)

options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  
  titlePanel("Sentiment Analysis"),
  
  sidebarPanel(
    fileInput(inputId = "fileName", label = "Choose your file"),
    hr(),
    sliderInput("freq", "Minimum frequency: ", min = 1, max = 50, value = 10),
    sliderInput("max", "Maximum number of words: ", min = 1, max = 300, value = 200)
  ),
  
  mainPanel(
    plotOutput(outputId = "wordcloudPlotting"),
    plotOutput(outputId = "ggPlotting"),
    plotOutput(outputId = "histogramPlotting"),
    plotOutput(outputId = "bubblePlotting"),
    plotOutput(outputId= "piePlotting"),
    plotOutput(outputId= "linePlotting")
  )
)

server <- function(input, output) {
  
  output$wordcloudPlotting <- renderPlot({
    
    file <- input$fileName$datapath
    text <- readLines(file)
    
    df <- returnDataFrame(text)
    
    wordcloud(words = df$word, freq = df$freq, min.freq = input$freq, max.words = input$max, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
  })
  
  output$ggPlotting <- renderPlot({
    file <- input$fileName$datapath
    text <- readLines(file)
    returnGGPlot(text)
  })
  
  output$histogramPlotting <- renderPlot({
    file <- input$fileName$datapath
    text <- readLines(file)
    returnHistogramPlot(text)
  })
  
  output$bubblePlotting <- renderPlot({
    file <- input$fileName$datapath
    text <- readLines(file)
    returnBubblePlot(text)
  })
  
  output$piePlotting <- renderPlot({
    file <- input$fileName$datapath
    text <- readLines(file)
    returnPiePlot(text)
  })
  
  output$linePlotting <- renderPlot({
    file <- input$fileName$datapath
    text <- readLines(file)
    returnLinePlot(text)
  })
  
}

returnDataFrame <- function(text) {
  
  docs <- Corpus(VectorSource(text))
  
  trans <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  docs <- tm_map(docs, trans, "/")
  docs <- tm_map(docs, trans, "@")
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  
  dtm <- TermDocumentMatrix(docs)
  mat <- as.matrix(dtm)
  v <- sort(rowSums(mat), decreasing = TRUE)
  
  data.frame(word = names(v), freq = v)
}

returnGGPlot <- function(texts) {
  
  Sentiment <- get_nrc_sentiment(texts)
  text <- cbind(texts, Sentiment)
  
  TotalSentiment <- data.frame(colSums(text[, c(2:11)]))
  names(TotalSentiment) <- "count"
  TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
  
  rownames(TotalSentiment) <- NULL
  
  ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) + 
                geom_bar(aes(fill = sentiment), stat = "identity") +
                theme(legend.position = "none") +
                xlab("Sentiment") +
                ylab("Total Count") +
                ggtitle("Sentiment Score")
}

returnHistogramPlot <- function(texts) {
  Sentiment <- get_nrc_sentiment(texts)
  text <- cbind(texts, Sentiment)

  TotalSentiment <- data.frame(colSums(text[, c(2:11)]))
  names(TotalSentiment) <- "count"
  TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)

  rownames(TotalSentiment) <- NULL

  ggplot(TotalSentiment, aes(x=count)) +
    geom_histogram(binwidth=50, color = "white", fill = "red")
}

returnBubblePlot <- function(text) {
  
  Sentiment <- get_nrc_sentiment(text)
  texts <- cbind(text, Sentiment)
  
  TotalSentiment <- data.frame(colSums(texts[, c(2:11)]))
  names(TotalSentiment) <- "count"
  TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
  rownames(TotalSentiment) <- NULL
  
  packing <- circleProgressiveLayout(TotalSentiment["count"], sizetype='area')
  
  TotalSentiment <- cbind(TotalSentiment, packing)
  
  TotalSentiment.gg <- circleLayoutVertices(packing, npoints = 50)
  
  ggplot() + geom_polygon(data = TotalSentiment.gg, aes(x, y, group = id, fill = as.factor(id)), colour = "black", alpha = 0.6) +
    scale_size_continuous(range = c(1,4)) + geom_text(data = TotalSentiment, aes(x, y, size=count, label = sentiment)) +
    theme_void() + 
    theme(legend.position="none") +
    coord_equal()
}

returnPiePlot <- function(text){
  
  Sentiment <- get_nrc_sentiment(text)
  text <- cbind(text, Sentiment)
  
  TotalSentiment <- data.frame(colSums(text[, c(2:11)]))
  names(TotalSentiment) <- "count"
  TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
  
  rownames(TotalSentiment) <- NULL
  
  ggplot(TotalSentiment, aes(x ="" , y = count, fill = sentiment)) +
    geom_col() +
    coord_polar(theta = "y")
}

returnLinePlot <- function(text){
  Sentiment <- get_nrc_sentiment(text)
  text <- cbind(text, Sentiment)
  
  TotalSentiment <- data.frame(colSums(text[, c(2:11)]))
  names(TotalSentiment) <- "count"
  TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
  
  rownames(TotalSentiment) <- NULL
  
  ggplot(data=TotalSentiment, aes(x=sentiment, y=count, group=1)) +
    geom_line()+
    geom_point()
  
}

shinyApp(ui = ui, server = server)








