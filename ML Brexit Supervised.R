#reticulate::py_install('transformers', pip = TRUE)
install.packages("tensorflow")
#library(keras)
library(tensorflow)
library(dplyr)
library(tfdatasets)
?keras
?tensorflow
.libPaths("/var/folders/lz/t1l16sr170b6fldj_w28ybkw0000gn/T//Rtmpwqocin/downloaded_packages")


#transformer = reticulate::import('transformers')

transformer$RobertaTokenizer$from_pretrained('roberta-base', do_lower_case=TRUE)
transformer$TFRobertaModel$from_pretrained('roberta-base')
?remove.packages

.libPaths()
library(tfdatasets)
library(dplyr)
.libPaths()

remove.packages("tensorflow", "/Library/Frameworks/R.framework/Versions/4.2/Resources/library")
remove.packages("tfdatasets", "/Library/Frameworks/R.framework/Versions/4.2/Resources/library")

install.packages("tensorflow", "/Library/Frameworks/R.framework/Versions/4.2/Resources/library")
library(tensorflow)
?library



#### Estimating Sentiment Score ####
View(brexit_data)
positive = scan("https://ptrckprry.com/course/ssd/data/positive-words.txt", what = "character", comment.char = ";")
?scan
negative = scan("https://ptrckprry.com/course/ssd/data/negative-words.txt", what = "character", comment.char = ";")

# add your list of words below as you wish if missing in above read lists
#pos words = pro stay with EU
# neg words = vote brexit
pos.words = c(positive, "StrongerIn", "#StrongerIn", "membership", "stay")
neg.words = c(negative, "@vote_leave", "out", "leave")




#### Sentiment Scoring Function ####


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # sentence is a parameter
  # vector of sentences as input
  # plyr takes a list or vector and can return as list
  # want a simple array (simple column and row arrangement)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    #clean up sentences with RR's regex-driven global substitute, gsub() function:
    sentence = gsub("httpsL//", "", sentence)
    sentence = gsub("http://", "", sentence)
    sentence = gsub("[^[:graph:]]", " ", sentence)
    sentence = gsub("[[:punct:]]", "", sentence)
    sentence = gsub("[[:cntrl:]]", "", sentence)
    sentence = gsub("\\d+", "", sentence)
    sentece = str_replace_all(sentence, "[^[:graph:]]", " ")
    #and convert everything to lowercase
    sentence = tolower(sentence)
    
    #split everything into words. str_split from stringr package
    word.list = str_split(sentence, "\\s+")
    #sometimes a list() is one level of hierarchy too much, turns into regular vector
    words = unlist(word.list)
    
    
    # compare our words to the positive and negatives words
    pos.matches = match(words, pos.words)
    neg.matches = match (words, neg.words)
    
    
    # match() returns the position of the matched term or NA
    # we're just looking for TRUE/FALSE
    # gets rid of NA stuff
    pos.matches = !is.na(pos.matches) #if not NA stuff
    neg_matches = !is.na(neg.matches)
    
    
    # True = 1 and False = 0 by sum() function:
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  #creates dataframe of column of score and the column of text
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
  
}

# stuff in the big {} means the actual function
# stuff in parentheses at beginning are the parameters

#assigning parameters


## tge actually right function

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we are giving vector of sentences as input. 
  # plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub() function:
    sentence = gsub('https://','',sentence)
    sentence = gsub('http://','',sentence)
    sentence = gsub('[^[:graph:]]', ' ',sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

  
  
#### Using function to calculate sentiment score ####

analysis <- score.sentiment(brexit_data$text, pos.words, neg.words)

analysis
# sentiment score frequency table
table(analysis$score)

#### Graphical Representations using new sentiment data ####

## histogram

ggplot(analysis, aes(x = score)) +
  geom_histogram(binwidth = 1, fill = "springgreen3") +
  ylab("Frequency")

nrow(analysis)
nrow(brexit_data)

analysis

names(analysis)
names(brexit_data)

brexit_data <- left_join(brexit_data, analysis)
View(brexit_data)


#### training classification model based on the sentiment score ####
## general boosting machine

brexit_len <- nrow(brexit_data) #nrow shows the number of rows in iris

brexit_data$ml <- c(rep("training", ceiling(.6*brexit_len)),
                rep("test", ceiling(.2*brexit_len)),
                rep("validation", ceiling(.2*brexit_len))) %>%
  sample(brexit_len, replace = F)

View(brexit_data)

brexit_train <- filter(brexit_data, ml == "training")
brexit_test <- filter(brexit_data, ml == "test")
brexit_valid <- filter(brexit_data, ml == "validation")

## General Boosting Machine

library(gbm)


brexit_data <- brexit_data %>%
  mutate(label2 = label)

View(brexit_data)

brexit_data$label2 <- revalue(brexit_data$label2, c("Stay"=1))
brexit_data$label2 <- revalue(brexit_data$label2, c("Leave"=0))

View(brexit_data)

brexit_gbm <- gbm(label2 ~ score,
                data = brexit_data,
                distribution = "bernoulli")

brexit_gbm_preds <- brexit_test %>%
  select(score) %>%
  predict(object = brexit_gbm)

brexit_test$brexit_preds <- brexit_gbm_preds
View(brexit_test)

brexit_test <- brexit_test %>%
  mutate(label3 = round(brexit_data$brexit_preds, digits = 0))

View(brexit_test)
## gbm uses only 1 variable to predict the label, very inaccurate and predicts everything is "Stay"


## Calculate Error and Accuracy

library(Metrics)

#calculate rmse between predictions and true values
rmse(as.numeric(brexit_test$label2), brexit_test$label3)
?rmse
##0.5729921
#better for mse to be smaller (not that small here)

#calculate mae between predictions and true values, the error
mae(as.numeric(brexit_test$label2), brexit_test$label3)
##0.3188302
##lower is better


true_vals <- sum(brexit_test$label3 == brexit_test$label2)
total_vals <- nrow(brexit_test)

accuracy <- true_vals/total_vals
accuracy
##0.6861832
table(brexit_test$label) ## more stay ones in the test data so accuracy is ok



# ft score tells us about false positive and false negative rates, how much is wrong

f1(brexit_test$label3, brexit_test$label2)
##0.5 (closer to 1 is better)

#### testing the model using validation data ####
brexit_gbm_preds_v <- brexit_valid %>%
  select(score) %>%
  predict(object = brexit_gbm)

brexit_valid$brexit_predsv <- brexit_gbm_preds_v
brexit_valid <- brexit_valid %>%
  mutate(label3 = round(brexit_valid$brexit_predsv, digits = 0))

true_vals <- sum(brexit_valid$label3 == brexit_valid$label2)
total_vals <- nrow(brexit_valid)

accuracy <- true_vals/total_vals
accuracy
#0.6816283

View(brexit_valid)


