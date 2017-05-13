install.packages("rvest")
library(rvest)

pages <- 10

reviews_all <- NULL
for(page_num in 1:pages){
  #amazon <- read_html("https://www.amazon.com/Optimum-Nutrition-Standard-Protein-Cookies/product-reviews/B000GIPJZ2/ref=cm_cr_arp_d_show_all?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber=", page_num)
  url <- paste0("https://www.amazon.com/Optimum-Nutrition-Standard-Protein-Cookies/product-reviews/B000GIPJZ2/ref=cm_cr_arp_d_show_all?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber=", page_num)
  reviews <- amazon %>% html_nodes(".review-text") %>% html_text()
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}

reviews_all

prod <- html_nodes(lego_movie, "B000GIPJZ2")

lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

lego_movie %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()




pacman::p_load(XML, dplyr, stringr, rvest, audio)

#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

prod_code = "B0043WCH66"
url <- paste0("https://www.amazon.com/dp/", prod_code)
doc <- read_html(url)

#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
prod

#Source funtion to Parse Amazon html pages for data
source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")

pages <- 10

reviews_all <- NULL
for(page_num in 1:pages){
  url <- paste0("http://www.amazon.com/product-reviews/",prod_code,"/?pageNumber=", page_num)
  doc <- read_html(url)
  
  reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}
