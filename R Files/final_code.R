install.packages(c("NLP", "openNLP", "RWeka", "rvest","rJava","SnowballC","tm","RTextTools","RWekajars"))
library(tm)
library(RTextTools)
library(rvest)
install.packages('dplyr')
install.packages('plyr')
install.packages("stringr")
library(dplyr)
library(plyr)
library(stringr)

options(max.print=1000000)
#count for number of pages.
pages <- 50

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
iniReviews <- all_reviews
all_reviews <- iniReviews

#Initial cleaning of data
#Removing '/' character 
all_reviews <- gsub("/", "", all_reviews) 
#Removing '\' character
all_reviews <- gsub("'\'", "", all_reviews)
#Removing line breaks
all_reviews <- gsub("[\r\n]", " ", all_reviews)
#Removing garbage
all_reviews <- gsub("u0096", "", all_reviews)
#Removing Punctuation Marks
all_reviews <- gsub("[[:punct:]]", "", all_reviews)
#Removing digits or numbers.
all_reviews <- gsub('\\d+', '', all_reviews)
#Remove Unassigned & Other symbols like emojis
all_reviews <-gsub('\\p{So}|\\p{Cn}', '', all_reviews, perl = TRUE)


#Creating Corpus
myCorpus <- Corpus(VectorSource(all_reviews))
inspect(myCorpus)

#Cleaning Corpus
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
#Strip-off white spaces,plain text and lowercase
myCorpus <- tm_map(myCorpus,stripWhitespace)
myCorpus <- tm_map(myCorpus,PlainTextDocument)
myCorpus <- tm_map(myCorpus, tolower)

# remove stopwords
myCorpus <- tm_map(myCorpus,removeWords,stopwords("english"))
#Clearing whitespace again    
myCorpus <- tm_map(myCorpus,stripWhitespace)

#Tokenization
scan_tokenizer <- function(x)
scan(text = as.character(x), what = "character", quote = "",
           quiet = TRUE)
    
token <- Token_Tokenizer(scan_tokenizer)
#testtokenized_reviews <- token(myCorpus)
    
  
    
#positive word list stemming   
poslist <- read.delim(file="D:/Spring 2017/ADS/Final_Project/positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
View(poslist)
names(poslist) <- c('posword')
poslist <- wordStem(as.matrix(poslist))

#negative word list stemming
neglist <- read.delim(file="D:/Spring 2017/ADS/Final_Project/negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
View(neglist)
names(neglist) <- c('negword')
neglist <- wordStem(as.matrix(neglist))

#function to calculate review sentiments scores    
sentimentAnalysis <- function(all_reviews, neglist, poslist){
  
      result <- matrix('', 0, 3)
      
     reviews_scores <- laply(all_reviews, function(review, neglist, poslist){
        #preserving review text 
        initial_review <- review
        
        #creating wordlist of review
        tokenized_review <- token(review)
       
        #stemming review list
      stemmed_review <- wordStem(tokenized_review)
        
        #print(stemmed_review)
        
        #Cleaning the stemmed text
        stemmed_review <- gsub('[[:punct:]]', '', stemmed_review )
        #Removing Control Characters
        stemmed_review  <- gsub('[[:cntrl:]]', '', stemmed_review)
    
       
        #List with matches between review words and each category(positive & negative)
        positiveMatch_list <- match(stemmed_review, poslist)
        negativeMatch_list <- match(stemmed_review, neglist)
        
        #calculate sum of number of words in each matched category
        positiveMatch_list <- sum(!is.na(positiveMatch_list))
        negativeMatch_list <- sum(!is.na(negativeMatch_list))
        
        #total <- positiveMatch_list + negativeMatch_list
        #posRatio <- (positiveMatch_list/total)
        #negRatio <- (negativeMatch_list/total)
        
        score <- c(positiveMatch_list, negativeMatch_list)
        
        #add row to scores table
        newrow <- c(initial_review, score)
        result  <- rbind(result, newrow)
        return(result)
        
      }, neglist, poslist)
     
      return(reviews_scores)
     
    }    
  
    #Review table with scores
    fin_Result <- as.data.frame(sentimentAnalysis(myCorpus, neglist, poslist))
    colnames(fin_Result) <- c('sentence', 'posRatio', 'negRatio')
    View(fin_Result)
    
   # idx <- fin_Result$posRatio > fin_Result$negRatio
  #  myStopwords <- a[-idx]
    
    fin_Result[,1] <- as.character( fin_Result[, 1] )
    fin_Result[,2] <- as.numeric(as.character( fin_Result[, 2] ))
    fin_Result[,3] <- as.numeric(as.character( fin_Result[, 3] ))
    
    
    # Convert corpus to Stem document
    corpus_clean <- tm_map(myCorpus, stemDocument,language="english")
    inspect(corpus_clean)
    
    #Transform Corpus to Document Term Matrix
    corpus_dtm <- DocumentTermMatrix(corpus_clean)
    inspect(corpus_dtm)
    
    #inspect(corpus_dtm[1:10,1:10])
    
    #  TDM <- TermDocumentMatrix(doc.corpus)
    