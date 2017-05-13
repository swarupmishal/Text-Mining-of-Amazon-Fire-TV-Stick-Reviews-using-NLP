install.packages("rvest")
install.packages("rJava")
install.packages(c("NLP", "openNLP", "RWeka", "qdap"))
install.packages("RTextTools")

library(rvest)
library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(RTextTools)

#count for number of pages.
pages <- 10 

all_reviews <- NULL

for(page_num in 1:pages){
  
  #reading one page at a time
  single_page <- paste0("https://www.amazon.com/All-New-Fire-TV-Stick-With-Alexa-Voice-Remote-Streaming-Media-Player/product-reviews/B00ZV9RDKK/ref=cm_cr_getr_d_show_all?ie=UTF8&reviewerType=all_reviews&showViewpoints=1&sortBy=helpful&pageNumber=", page_num)
  
  #Converting to HTML
  single_doc <- read_html(single_page)
  
  #Parsing review text
  review <-html_nodes(x= single_doc, css = ".review-text") %>%
    html_text()
  
  #Matrix conversion of data for columner form 
  review <- as.matrix(review)
  
  #consolidating all reviews in one column
  all_reviews <- rbind(all_reviews,review)
}

View(all_reviews)

#Tokenization
scan_tokenizer <- function(x)
  scan(text = as.character(x), what = "character", quote = "",
       quiet = TRUE)

token <- Token_Tokenizer(scan_tokenizer)
tokenized_reviews <- token(all_reviews)


#Stemming
wordStem(tokenized_reviews)

#Sentiment Analysis

