#### Load in dataset and libraries ####
install.packages("textstem")
install.packages("ruimtehol")

library(ruimtehol)
library(textstem)
library(ggplot2)
library(tidytext)
library(data.table)
library(magrittr)

brexit_data2 <- brexit_data
View(brexit_data2)
brexit_data2 <- brexit_data2 %>%
  select(name:label)

#### prepping for training model ####

# x
#brexit_data2$text

# y 
#brexit_data2$label

#model
brexit_data2



#### Training Data ####

model <- embed_tagspace(x = tolower(brexit_data2$text),
                        y = brexit_data2$label,
                        early_stopping = 0.8,
                        dim = 30,
                        minCount = 3)



plot(model)

## use model, practice


predict(model, brexit_data2$text[72], type = "label")
# predicted leave (incorect)

predict(model, brexit_data2$text[1107], type = "label")
# predicted stay (correct)

predict(model, brexit_data2$text[9999], type = "label")
# predicted stay (correct)

predict(model, brexit_data2$text[71865], type = "label")
# predicted leave 

predict(model, brexit_data2$text[2], type = "label")

## Evaluate Accuracy of model

# add model to dataset
for(i in nrow(brexit_data2)){
  prediction <- predict(model, brexit_data2$text[i], type = "label")
  brexit_data2$predicted_val[i] <- ifelse(as.numeric(prediction$Leave) >= as.numeric(prediction$Stay), "Leave", "Stay")
}

View(brexit_data2)


#find accuracy of model
brexit_data2 <- na.omit(brexit_data2)

true_vals <- sum(brexit_data2$label == brexit_data2$predicted_val)
true_vals
total_vals <- nrow(brexit_data2)
accuracy <- true_vals/total_vals

accuracy
View(brexit_data2)
#### New Starspace Improvement ####
library(rlang)
starspace_model = function(data, model, key_word, label1, label2, rights){
  data$plain <- strsplit(brexit_data3$text, "\\W")
  data$plain <- sapply(data$plain, FUN=function(x) paste(x, collapse = " "))
  data$plain <- tolower(data$plain)
  
  # model comes in
  model <- model
  # get embeddings of words and categories
  embed_words <- as.matrix(model, type = "words")
  embed_cats <- as.matrix(model, type = "labels")
  
  #embedding of the keyword and then similarity between category
  embedding_comb <- starspace_embedding(model, expr_text(expr(key_word)), type = "document")
  embedding_similarity(embedding_comb, 
                       embed_cats, 
                       top_n = 3)
  
  #using model on whole dataset
  for(i in 1:nrow(data)){
    prediction <- predict(model, data$plain[i], type = "label")
    data$new_pred[i] <- ifelse(prediction$label1 >= prediction$label2, expr_text(expr(label1)), expr_text(expr(label2)))
  }
  
  return(data)
  
  # accuracy
  true_vals <- sum(data$rights == data$new_pred)
  true_vals
  total_vals <- nrow(data)
  accuracy <- true_vals/total_vals
  return(accuracy)
  
  # f1 true pos and tru negs
  f1(data$rights, data$new_pred)
}


brexit_model <- starspace_model(brexit_data, model, brexit, Leave, Stay, label)
brexit_model

names(brexit_model)

View(brexit_data3)

View(brexit_model)

View(brexit_data)
brexit_data3 <- brexit_data


library(ruimtehol)
data(brexit_data3, package = "ruimtehol")


## clean text of the question in parliament
brexit_data3$plain <- strsplit(brexit_data3$text, "\\W")
brexit_data3$plain <- sapply(brexit_data3$plain, FUN=function(x) paste(x, collapse = " "))
brexit_data3$plain <- tolower(brexit_data3$plain)

model <- embed_tagspace(x = brexit_data3$plain, 
                        y = brexit_data3$label, 
                        dim = 50, 
                        ngram = 3, loss = "hinge", similarity = "cosine", adagrad = TRUE,
                        early_stopping = 0.8, minCount = 2)

## Get embeddings of the dictionary of words as well as the categories
embed_words <- as.matrix(model, type = "words")
embed_cats <- as.matrix(model, type = "labels")

## Find closest label / predicting embedding similarity
embedding_comb <- starspace_embedding(model, "brexit", type = "document")
embedding_similarity(embedding_comb, 
                     embed_cats, 
                     top_n = 3)

predict(model, brexit_data3$plain[1], type = "label")

## Evaluate Accuracy and Error

for(i in 1:nrow(brexit_data3)){
  prediction <- predict(model, brexit_data3$plain[i], type = "label")
  brexit_data3$new_pred[i] <- ifelse(prediction$Leave >= prediction$Stay, "Leave", "Stay")
}

View(brexit_data3)

true_vals <- sum(brexit_data3$label == brexit_data3$new_pred)
true_vals
total_vals <- nrow(brexit_data3)
accuracy <- true_vals/total_vals
accuracy
#0.9813546 YAYYYYY

# f1 score tells us about false positive and false negative rates, how much is wrong

f1(brexit_data3$label, brexit_data3$new_pred)
# 1 means it's very accurate, few false pos and few false negs

#### Starspace New on Election Data ####

View(election_data)

library(ruimtehol)
data(election_data, package = "ruimtehol")


## clean text of the question in parliament
election_data$plain <- strsplit(election_data$text, "\\W")
election_data$plain <- sapply(election_data$plain, FUN=function(x) paste(x, collapse = " "))
election_data$plain <- tolower(election_data$plain)

model <- embed_tagspace(x = election_data$plain, 
                        y = election_data$handle,#gotta change
                        dim = 50, 
                        ngram = 3, loss = "hinge", similarity = "cosine", adagrad = TRUE,
                        early_stopping = 0.8, minCount = 2)

## Get embeddings of the dictionary of words as well as the categories
embed_words <- as.matrix(model, type = "words")
embed_cats <- as.matrix(model, type = "labels") #embeddings of trump vs hillary

## Find closest label / predicting embedding similarity
embedding_comb <- starspace_embedding(model, "make america great", type = "document")
embedding_similarity(embedding_comb, 
                     embed_cats, 
                     top_n = 3)

predict(model, election_data$plain[1], type = "label")

## Eval accuracy
for(i in 1:nrow(election_data)){
  prediction <- predict(model, election_data$plain[i], type = "label")
  election_data$new_pred[i] <- ifelse(prediction$realDonaldTrump >= prediction$HillaryClinton, "realDonaldTrump", "HillaryClinton")
}

View(election_data)

election_data <- select(election_data, -(longitude:place_holding_box))
View(election_data)

true_vals <- sum(election_data$handle == election_data$new_pred)
true_vals
total_vals <- nrow(election_data)
accuracy <- true_vals/total_vals
accuracy
#0.9857232 YAYY

f1(election_data$handle, election_data$new_pred)
#1 yayyyyy, few to none false positives and false negatives





