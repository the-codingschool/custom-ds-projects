#### load in required libraries ####
library(ggplot2)


#### load is csv file better ####

election_data <- read.csv("JingjingsProject/data/tweets.csv")
View(election_data)

brexit_data <- read.csv("JingjingsProject/data/brexit.csv")
View(brexit_data)

#### Save R object as file to easily load later####
saveRDS(election_data, "JingjingsProject/data/tweets.csv")
saveRDS(brexit_data, "JingjingsProject/data/corpus.csv")


#### Cleaning the Brexit Data ####

  
clean.text = function(x){
    # convert to lower case
    x = tolower(x)
    # remove rt
    x = gsub("rt", "", x)
    # remove at
    x = gsub("@\\w+", "", x)
    # remove punctuation
    x = gsub("[[:punct:]]", "", x)
    # remove numbers
    x = gsub("[[:digit:]]", "", x)
    # remove links http
    x = gsub("http\\w+", "", x)
    # remove tabs
    x = gsub("[ |\t]{2,}", "", x)
    # remove blank spaces at the beginning
    x = gsub("^ ", "", x)
    # remove blank spaces at the end
    x = gsub(" $", "", x)
    # some other cleaning text
    x = gsub('https://','',x)
    x = gsub('http://','',x)
    x = gsub('[^[:graph:]]', ' ',x)
    x = gsub('[[:punct:]]', '', x)
    x = gsub('[[:cntrl:]]', '', x)
    x = gsub('\\d+', '', x)
    x = str_replace_all(x,"[^[:graph:]]", " ")
    return(x)
  }

cleanText <- clean.text(brexit_data$text)
idx <- which(cleanText == " ")
cleanText <- cleanText[cleanText != " "]
