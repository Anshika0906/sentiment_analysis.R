library(tm)           
library(SnowballC)   
library(randomForest) 
library(caret)        
library(rpart)        
library(rpart.plot)  
library(readr)        
library(shiny)        
library(readr)

Dataset_SA <- read_csv("Dataset-SA.csv")
View(Dataset_SA)

head(Dataset_SA)
dim(Dataset_SA)
summary(Dataset_SA)

reviews <- Dataset_SA[, c("Review", "Summary", "Sentiment")]

set.seed(123)
max_rows <- 8000   
if (nrow(reviews) > max_rows) {
  rows_keep <- sample(nrow(reviews), max_rows)
  reviews <- reviews[rows_keep, ]
}

reviews$FullText <- paste(reviews$Review, reviews$Summary, sep = " . ")

reviews <- reviews[, c("FullText", "Sentiment")]
reviews <- reviews[!is.na(reviews$FullText) & !is.na(reviews$Sentiment), ]

reviews$Sentiment <- factor(
  reviews$Sentiment,
  levels = c("negative", "neutral", "positive")
)

corpus <- VCorpus(VectorSource(reviews$FullText))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)

dtm_small <- removeSparseTerms(dtm, 0.997)

X <- as.data.frame(as.matrix(dtm_small))

sentiment_data <- cbind(X, Sentiment = reviews$Sentiment)

set.seed(123)
trainIndex <- createDataPartition(
  sentiment_data$Sentiment,
  p = 0.7,
  list = FALSE
)
train <- sentiment_data[trainIndex, ]
test  <- sentiment_data[-trainIndex, ]

table(train$Sentiment)
table(test$Sentiment)

max_train_rows <- 4000   
if (nrow(train) > max_train_rows) 

rf_model <- randomForest(
  Sentiment ~ .,
  data  = train,
  ntree = 50          
)

predictions <- predict(rf_model, newdata = test)
accuracy <- mean(predictions == test$Sentiment)
cat("Accuracy:", accuracy, "\n")

tree_model <- rpart(Sentiment ~ ., data = train)
rpart.plot(tree_model, main = "Decision Tree for Review Sentiment")

rf_terms <- colnames(X)

text_to_features <- function(text_vec) {
  corp_new <- VCorpus(VectorSource(text_vec))
  corp_new <- tm_map(corp_new, content_transformer(tolower))
  corp_new <- tm_map(corp_new, removeNumbers)
  corp_new <- tm_map(corp_new, removePunctuation)
  corp_new <- tm_map(corp_new, removeWords, stopwords("english"))
  corp_new <- tm_map(corp_new, stripWhitespace)
  corp_new <- tm_map(corp_new, stemDocument)
  
  dtm_new <- DocumentTermMatrix(
    corp_new,
    control = list(dictionary = rf_terms)
  )
  
  as.data.frame(as.matrix(dtm_new))
}

ui <- fluidPage(
  titlePanel("Simple Sentiment Analysis (Random Forest)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter a product review:"),
      textAreaInput(
        "user_text",
        label = NULL,
        value = "Awesome sound and great battery life. Totally worth it!",
        rows = 5
      ),
      actionButton("predict_btn", "Predict Sentiment"),
      br(), br(),
      h5("Model info"),
      verbatimTextOutput("model_info")
    ),
    
    mainPanel(
      h3("Prediction"),
      verbatimTextOutput("prediction_out"),
      br(),
      h4("Example decision tree"),
      plotOutput("tree_plot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  
  output$model_info <- renderText({
    paste(
      "Training rows:", nrow(train), "\n",
      "Test rows:", nrow(test), "\n",
      "Accuracy:", round(accuracy, 4)
    )
  })
  
  observeEvent(input$predict_btn, {
    txt <- input$user_text
    
    if (nchar(trimws(txt)) == 0) {
      output$prediction_out <- renderText("Please enter a review first.")
    } else {
      new_X <- text_to_features(txt)
      pred  <- predict(rf_model, newdata = new_X)
      output$prediction_out <- renderText(
        paste("Predicted sentiment:", as.character(pred))
      )
    }
  })
  
  output$tree_plot <- renderPlot({
    rpart.plot(tree_model, main = "Decision Tree for Review Sentiment")
  })
}

shinyApp(ui = ui, server = server)
