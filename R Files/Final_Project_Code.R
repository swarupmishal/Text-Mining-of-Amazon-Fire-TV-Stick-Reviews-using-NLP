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
install.packages('e1071')
install.packages('ade4')
library(e1071)
library(ade4)
install.packages('wordcloud')
library(wordcloud)
options(max.print=1000000)
#count for number of pages.
pages <- 500

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
inspect(myCorpus)
myCorpus_dtm <- DocumentTermMatrix(myCorpus,control=list(tolower=T,removeNumbers=T,removePunctuation=T,stopwords=T,stripWhitespace=T,stemDocument=T))
rmspa_corpus2 = removeSparseTerms(myCorpus_dtm,0.70)
inspect(rmspa_corpus2)


# show the average frequency of the top 20 most frequent words
mean_corpus2 <- sort(colMeans(as.matrix(myCorpus_dtm)),decreasing = T)

wordcloud(names(mean_corpus2[1:30]),mean_corpus2[1:30],scale = c(3,1),colors = brewer.pal(8,"Dark2"))

barplot(mean_corpus2[1:5],col = c("blue","orange","cyan","red","yellow") ) #,border = NA, las=3,xlab = "Most Used 30 words",ylab = "Frequency",ylim = c(0,1))
#hist(mean_corpus2[1:30])
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
    colnames(fin_Result) <- c('Review', 'posRatio', 'negRatio')
    View(fin_Result)
    
    fin_Result[,1] <- as.character( fin_Result[, 1] )
    fin_Result[,2] <- as.numeric(as.character( fin_Result[, 2] ))
    fin_Result[,3] <- as.numeric(as.character( fin_Result[, 3] ))
    
    ##########################################################
    #Creating new column with result of positive/negative values
    fin_Result["Result"] <-NA
    for(i in 1:dim(fin_Result)[1]){
      if(fin_Result[i,2]>fin_Result[i,3]) {
        fin_Result[i,4]<-"Positive" 
      }else if(fin_Result[i,2]==fin_Result[i,3]) {
        fin_Result[i,4]<-"Neutral" 
      }
      else{
        fin_Result[i,4]<-"Negative"
      }
    }
   
    
    # Convert corpus to Stem document
    corpus_clean <- tm_map(myCorpus, stemDocument,language="english")
    inspect(corpus_clean)
    
    #Transform Corpus to Document Term Matrix
    corpus_dtm <- DocumentTermMatrix(corpus_clean)
    inspect(corpus_dtm)
    
    #Writing output in csv file
    write.csv(cbind(as.matrix(corpus_dtm),fin_Result$Result),"D:/Spring 2017/ADS/Final_Project/final.csv")
    
    
    #inspect(corpus_dtm[1:10,1:10])
    
    #  TDM <- TermDocumentMatrix(doc.corpus)
    
 #----------------------------------------------------------------------------------   
    #SVM
    
    #Read Final CSV
    getwd()
    setwd("D:/Spring 2017/ADS/Final_Project")
    final <-read.csv(file="final.csv", header=TRUE, sep=",")
    View(final)
    str(final)
    ncol(final)
    
    # Divided the data with train data and test data keeping last 10000 records for test data.
    train<-final[1:1000,]
    test<-final[1001:1460,]
    
    #Tuning SVM for finding best parameters for all possible 4 kernels.
    obj<-tune(svm, RESULT~.,kernel ="radial", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
    summary(obj)
    
    obj1<-tune(svm, RESULT~.,kernel ="linear", data= train, ranges=list(cost=10^(-2:2)))
    summary(obj1)
    
    obj2<-tune(svm, RESULT~.,kernel ="sigmoid", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
    summary(obj2)
    
    obj3<-tune(svm, RESULT~.,kernel ="polynomial", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
    summary(obj3)
    
    #Applying SVM with the best parameters we found after tuning. 
    model1 = svm(RESULT ~ .,type='C-classification', kernel = "linear", cost =0.1, data = train, scale = F)
    
    # Predicting the Values using model created using SVM.
    predictions <-  predict(model1, test)
    
    View(predictions)
    
    class(predictions)
    #Create Confusion Matrix
    table(test[,6251], predictions)
    confusionMatrix(test[,6251], predictions)
    
#------------------------------------------------------------------------------
    #CNN
    #install.packages("drat", repos="https://cran.rstudio.com")
    drat:::addRepo("dmlc")
    #install.packages("mxnet")
    library(mxnet)
    library(deepnet)
    #load input
    setwd("C:/Users/hu.ziy/Desktop")
    mydata=read.csv("C:/Users/hu.ziy/Desktop/ADSfinal/final_temp.csv")
    #standardize
    ncol(mydata)
    ncol(mydata)
    train.x <- as.matrix(mydata[1:4000,1:6241])
    #train.y<-mydata[1:100,1635]
    train.y <- as.array(as.numeric(mydata[1:4000,6251])-1)
    #train.y
    test <- mydata[4001:5000,1:6241]
    train.x<-t(train.x)
    test<-t(test)
    ##LeNet setup
    # input
    data <- mx.symbol.Variable('data')
    # first conv
    conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=25, pad=c(2,2))
    tanh1 <- mx.symbol.Activation(data=conv1, act_type="tanh")
    pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",  kernel=c(2,2), stride=c(2,2))
    # second conv
    conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=55, pad=c(2,2))
    tanh2 <- mx.symbol.Activation(data=conv2, act_type="tanh")
    pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max", kernel=c(2,2), stride=c(2,2))
    # first fullc
    flatten <- mx.symbol.Flatten(data=pool2)
    fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
    tanh3 <- mx.symbol.Activation(data=fc1, act_type="tanh")
    # second fullc
    fc2 <- mx.symbol.FullyConnected(data=tanh3, num_hidden=10)
    # loss
    lenet <- mx.symbol.SoftmaxOutput(data=fc2)
    ##Then let us reshape the matrices into arrays:
    train.array <- train.x
    dim(train.array) <- c(79, 79, 1, ncol(train.x))
    test.array <- test
    dim(test.array) <- c(79, 79, 1, ncol(test))
    ##cpu
    n.gpu <- 1
    device.cpu <- mx.cpu()
    device.gpu <- lapply(0:(n.gpu-1), function(i) {
      mx.gpu(i)
    })
    #as.array()
    mx.set.seed(123)
    model1 <- mx.model.FeedForward.create(lenet, X=train.array, y=as.array(train.y),
                                          ctx=device.cpu, num.round=2, array.batch.size=100,
                                          learning.rate=0.05, momentum=0.9, wd=0.00001,
                                          eval.metric=mx.metric.accuracy,
                                          #arg.params = modelx$arg.params,
                                          batch.end.callback=mx.callback.log.train.metric(100))
    m #predict
    preds <- predict(model1, test.array)
    pred.label <- max.col(t(preds)) - 1
    library(caret)
    con<-confusionMatrix(pred.label, as.numeric(mydata[4001:5000,6251])-1)
    summary(con)
    
    #----------------------------------------------------------------------
    #DBN
    
    #install.packages("deepnet")
    library(deepnet)
    
    #Load file
    setwd("E:/ADS/Final Project")
    final<-read.csv( "final_temp1.csv", header= TRUE)
    View(final)
    #Divide train and test
    train<-final[1:4000,]
    test<-final[4001:5000,]
    
    #Divide the variables as Dependent and Independant variables 
    ncol(train)
    train.x<-as.matrix(train[, 2:6250])
    train.y<- as.matrix(train$RESULT)
    test.x<-as.matrix(test[, 2:6250])
    test.y<-as.matrix(test$RESULT)
    
    #Predict using Deep Belief Network
    dnn <- dbn.dnn.train(train.x, train.y, hidden = c(500,500,500), numepochs = 5, cd=2)
    
    #Predict
    yy.dnn <- nn.predict(dnn, test.x)
    yy.dnn
    
    predictions <- unlist(yy.dnn)
    predictions
    
    dbn.out<-as.matrix(dbn.out)
    View(dbn.out)
    xtab <- table(unlist(test.y),unlist(round(dbn.out)))
    xtab
    
    err.dnn <- nn.test(dnn, test.x, test.y)
    err.dnn
    
    #Confusion Matrix
    library(caret)
    a <- confusionMatrix(unlist(yy.dnn), unlist(test.y))
    a
    
    #Summary
    summary(yy.dnn)
    