#install the necessary packages
#install.packages("ROAuth")
#install.packages("twitteR")
#install.packages("wordcloud")
#install.packages("tm")

library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")
library("RCurl")

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
require(twitteR)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "poSLbA9LM3SsjPVUfZ18PaBaS"
consumerSecret <- "rzx5quHpI2hLCPxtE89wLsTktgG7O2lRqU4PgEbZYaBq25sRTM"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
6373146
registerTwitterOAuth(twitCred)
user <- getUser("YottaLab")

#the cainfo parameter is necessary on Windows
r_stats<- searchTwitter("#gonzalez", n=1000, cainfo="cacert.pem")
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))
#clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, tolower) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus, max.words=100, colors="orange", rot.per=.5)

