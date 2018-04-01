install.packages("twitteR")
install.packages("tm")
install.packages("wordcloud")
library(twitteR)
library(tm)
library(wordcloud)
library(stringr)
library(plyr)
library(dplyr)
library(tidytext)

consumer_key <- "hdhNcJwUfnID9dJ8PkLP4Nf8b"
consumer_secret <- "TtrQ3ezY4hNQukMDNXBC38vsXM3sHRbGuvbBDJpGzhEAReJL4x"
access_token <- "1630609542-ZyQs6UhV85edcAkBxkAfnZB2sPZ7P1h3S1BqYr5"
access_secret <- "yOmppluxBqBOQDQ5X0WLZoh88sLUryey6fyBnwGef3jDJ"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

neg = scan("/Users/lunaluan/Documents/Academic/Berkeley/Spring 2018/STAT 222/opinion-lexicon-English/negative-words.txt", what="character", comment.char=";")
pos = scan("/Users/lunaluan/Documents/Academic/Berkeley/Spring 2018/STAT 222/opinion-lexicon-English/positive-words.txt", what="character", comment.char=";")

pos = c(pos, 'new','nice','good', 'horizon')
neg = c(neg, 'wtf', 'behind', 'ugly', 'back','worse','shitty', 'bad', 'no','freaking','sucks','horrible')

score.sentiment = function(tweets, pos.words, neg.words)
{
  require(plyr)
  require(stringr)
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

tweets = searchTwitter('@United',since = '2018-03-14', until ='2018-03-15', n = 100000, lang = 'en')
Tweets.text = laply(tweets,function(t)t$getText()) # gets text from Tweets

analysis = score.sentiment(Tweets.text, pos, neg) # calls sentiment function

tw = twListToDF(tweets)
tw2 = cbind.data.frame(text = tw$text, time = tw$created, score = analysis$score)

tweets.df = ldply(tweets, function(t) t$toDataFrame())
write.csv(tweets.df, file = "tweets3.14-3.15.csv")






