#### install/download packages ####
install.packages("rtweet")
install.packages("tidytext")

library(rtweet)
library(tidytext)
library(ggplot2)
library(dplyr)

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

#### Searching for Tweets ####
r_pract_tweets <- search_tweets(q = "#rstats",
                                n = 500) 
