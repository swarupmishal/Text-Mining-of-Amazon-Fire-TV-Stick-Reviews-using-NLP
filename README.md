# Text Mining and Deep Learning of Amazon Fire TV Stick with Alexa Voice Remote Reviews

![alt text](https://github.com/swarupmishal/Text-Mining-of-Amazon-Reviews/blob/master/Extras/data-mining-859x312.jpg)

## What exactly the Data is?
I have downloaded customer reviews for Fire TV Stick with Alexa Voice Remote from Amazon website.


## How can one obtain the Data?
The code will automatically download 50000 Amazon reviews for Fire TV Stick with Alexa Voice Remote.


## Implementation.

### Data PreProcessing:
1. Performed Data Cleaning by removed punctuations, special characters, whitespaces, graphical emojis(other symbols & unassigned characters). 
2. Converted words to lower case. 
3. Created corpus from the dataset. 
4. Performed Sentiment Analysis and tagged each review as positive, negative or neutral.

### Sentiment Analysis
We analyzed the customer reviews for that Product and process it as below :
1. Scraped the reviews using rvest
2. Performed Word Stemming and lemmatization using openNLP in R
3. Performed Sentiment Analysis using list of Positive and Negative Lexicons
4. Created a Sparse Representation for extracted Amazon Reviews
5. Word Cloud  

![alt text](https://github.com/swarupmishal/Text-Mining-of-Amazon-Reviews/blob/master/Extras/Capture.PNG)

6. Top 5 frequent words

![alt text](https://github.com/swarupmishal/Text-Mining-of-Amazon-Reviews/blob/master/Extras/Capture1.PNG)

### Processing:
Data was divided in 85% Train dataset and 15% Test dataset.
1. Support Vector Machine Classification was performed with an accuracy of 90.1%
2. Convolutional Neural Networks Classification was performed with an accuracy of 89.7%
3. Deep Belief Networks Classification was performed with an accuracy of 96.97%


## Conclusion:
So we can conclude that Deep Belief Network has the best prediction amongst all.
