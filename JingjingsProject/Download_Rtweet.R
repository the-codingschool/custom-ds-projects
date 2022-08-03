#### install/download packages ####
install.packages("rtweet")
install.packages("tidytext")

library(rtweet)
library(tidytext)
library(ggplot2)
library(dplyr)


library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(plyr)
library(stringr)
library(ggeasy)
library(plotly)

library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)

#### setting up API ####

auth_setup_default()
df <- search_tweets("#rstats", token = auth)
auth_as(auth)
auth_save(auth, "API_first")

app <- rtweet_app(bearer_token = Sys.getenv("RTWEET_API_KEY"))
auth_as(app)


#### authorization of API ####
appname <- "Globalization_314"
key <- "l5Rrr8sQ3BjdFcm9sTq08gGRO"
secret <- "Z3pztCeg74img8KRjWS3Hhot5aDUR23HOpW7aNkLTNRO4lGFV8"

## token for authentication
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)
twitter_token

#### Searching and Cleaning for Tweets ####
brexit_tweet <- search_tweets(q = "#brexit",
                                n = 500) 

View(brexit_tweet)
n.tweet <- length(brexit_tweet)

# convert tweets to a data frame

tweets.df <- as.data.frame(brexit_tweet)
?twListToDF

View(tweets.df)

tweets.txt <- sapply(brexit_tweet, function(t)t$getText())
?str_replace_all
?sapply

# Ignore graphical parameters to avoid input errors

# pre-processing text:

clean.text = function(x)
