### Word Embeddings ####
install.packages("textdata")

#### Get libraries ####
library(tidytext)
library(textdata)

#### Clean Data ####
library(dplyr)
election_data_samp <- election_data
View(election_data_samp)

election_data_samp <- election_data_samp %>%
  rename(screen_name = handle) %>%
  filter(screen_name == "realDonaldTrump") %>%
  select(screen_name, plain)
  
  


election_data_samp$plain <- strsplit(election_data_samp$text, "\\W")
election_data_samp$plain <- sapply(election_data_samp$plain, FUN=function(x) paste(x, collapse = " "))
election_data_samp$plain <- tolower(election_data_samp$plain)

# after cleaning election data
# election_data_samp <- select(election_data_samp, screen_name, plain)

View(election_data_samp)

View(brexit_data3)
brexit_data_samp <- brexit_data3 %>%
  filter(label == "Leave") %>%
  select(screen_name, plain)
View(brexit_data3)
View(brexit_data_samp)

## random sample of 5000 rows
brexit_3k <- slice_sample(brexit_data_samp, n = 3000)
election_3k <- slice_sample(election_data_samp, n = 3000)

## combine into 1 dataset
word_data <- rbind(brexit_3k, election_3k)
View(word_data)

# get rid of urls
library(stringr)
word_data$plain <- str_replace_all(string = word_data$plain,
                                            pattern = "https.+",
                                            replacement = "")


#### get sentiments ####

get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc") # use this one


#### Analysis ####
tidy_books <- word_data %>%
  unnest_tokens(word, plain)

View(tidy_books)

tidy_books <-tidy_books %>%
  mutate(stance = ifelse(screen_name == "realDonaldTrump", "trump", "brexit"))

## see which side is angrier 

nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

# returns most popular words trump tweets about anger
tidy_books %>%
  filter(screen_name == "realDonaldTrump") %>%
  inner_join(nrc_anger) %>% 
  dplyr::count(word, sort = TRUE)

tidy_books %>%
  filter(screen_name != "realDonaldTrump") %>%
  inner_join(nrc_anger) %>%
  dplyr::count(word, sort = TRUE)

View(tidy_books)

## Compare sentiments 
tidy_books <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(screen_name) %>%
  dplyr::count(sentiment, sort = TRUE) 
  
  #summarise(sentiment = )
  #inner_join(get_sentiments("nrc"))
#tidy_books
View(tidy_books)

## bar plot with x at diff emotions with different colors showing the count of each emotion
# find 1 emotion per screen name no named trump
no_trump_sentiments <- tidy_books %>%
  filter(screen_name != "realDonaldTrump") %>%
  inner_join(get_sentiments("nrc"))
View(no_trump_sentiments)

sentiment_concise <- no_trump_sentiments %>%
  group_by(screen_name, sentiment) %>%
  summarize(count = n()) %>%
  group_by(screen_name) %>%
  summarize(sentiment = ifelse(count == max(count), sentiment,"")) %>%
  filter(sentiment != "") %>%
  slice_sample(n=1)

View(sentiment_concise)

ggplot(sentiment_concise, aes(x=sentiment, fill = sentiment)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(x = "Sentiment", y = "Percent of Total", 
       title = "Sentiments of Twitter Users who Support Brexit") +
  theme(text = element_text(family = "Courier"))


#sentiments w/o condensing
ggplot(no_trump_sentiments, aes(x=sentiment, fill = sentiment)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(x = "Sentiment", y = "Percent of Total", 
       title = "Twitter Sentiments of Parliament Members who Support Brexit") +
  theme(text = element_text(family = "Courier"), axis.text.x = element_text(angle = 20))
  
View(tidy_books)
#trump sentiments
trump_sentiments <- tidy_books %>%
  filter(screen_name == "realDonaldTrump") %>%
  inner_join(get_sentiments("nrc"))

ggplot(trump_sentiments, aes(x=sentiment)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "deeppink4") +
  labs(x = "Sentiment", y = "Percent of Total", 
       title = "Trump Sentiments during Election") +
  theme(text = element_text(family = "Courier"), axis.text.x = element_text(angle = 20))


# graph a list of top 10 most popular words with anger and disgust (separate) and their counts
#trump
anger_words_t <- tidy_books %>% 
  filter(screen_name == "realDonaldTrump") %>%
  inner_join(nrc_anger) %>%
  dplyr::count(word, sort = TRUE) %>%
  slice_head(n = 10)
View(anger_words_t)

#brexit
anger_words_b <- tidy_books %>% 
  filter(screen_name != "realDonaldTrump") %>%
  inner_join(nrc_anger) %>%
  dplyr::count(word, sort = TRUE) %>%
  slice_head(n = 10)
View(anger_words_b)

## plot of trump's top 10 anger words
ggplot(anger_words_t, aes(x = reorder(word,(-n)), y=n)) +
  geom_bar(stat = "summary",
           fun = "mean",
           fill = "indianred") +
  labs(x = "Word", y = "Count", title = "Top 10 most common words Trump uses when angry") +
  theme(text = element_text(family = "Courier"), axis.text.x = element_text(angle = 45))
ggsave("trump_top_10_anger.png", width = 7, height = 5, units = "in")
## plot of brexit's top 10 words
ggplot(anger_words_b, aes(x = reorder(word,(-n)), y=n)) +
  geom_bar(stat = "summary",
           fun = "mean",
           fill = "skyblue4") +
  labs(x = "Word", y = "Count", title = "Top 10 most common anger words for Parliament Supporters of Brexit") +
  theme(text = element_text(family = "Courier"), axis.text.x = element_text(angle = 45))

ggsave("brexit_top_10_anger.png", width = 9, height = 5, units = "in")
## plot of 

nrc_graph <- bind_rows(
  screen_name != "realDonaldTrump" %>%
    
  mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

## what words contributed to certain sentiments
nrc_word_counts <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()
View

nrc_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL) 

#### WordCloud ####
install.packages("wordcloud2")
library(wordcloud)
library(tidytext)
library(reshape2)
library(RColorBrewer)
library(tm)
install.packages("RXKCD")
library(RXKCD)
tidy_books %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors = stance))
  


#### seeing anti-globalization reasons using starspace_embeddings ####


## Load libraries and clean data

library(tidytext)


election_data_wrd <- election_data %>%
  filter(handle == "realDonaldTrump") %>%
  select(plain) 

election_data_wrd$unique_ID <- row.names(election_data_wrd)

library(widyr)


## Word Embedding probabilities
#create context window with length 8
tidy_skipgrams <- election_data_wrd %>%
  unnest_tokens(ngram, plain, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, unique_ID, ngramID) %>%
  unnest_tokens(word, ngram)

#calculate unigram probabilities (used to normalize skipgram probabilities later)
unigram_probs <- election_data_wrd %>%
  unnest_tokens(word, plain) %>%
  dplyr::count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

#calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

#normalize probabilities
normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob[1:10,]


normalized_prob %>% 
  filter(word1 == "trump") %>%
  arrange(-p_together)





## Get embeddings of the dictionary of words as well as the categories (hillary vs trump)
embed_words <- as.matrix(model, type = "words")
embed_cats <- as.matrix(model, type = "labels") #embeddings of trump vs hillary

## Find closest label / predicting embedding similarity
embedding_comb <- starspace_embedding(model, "make america great", type = "document") 
#find embedding of each word in "make" "america" "great" and find total embedding
embedding_similarity(embedding_comb, 
                     embed_cats, 
                     top_n = 3)
#look at the similarity between document embedding of labels and of text 

# right-wing antiglobalism, British National Party, National Democratic Party of Germany, National Front, Freedom Party of Austria, threat to national economies
# threat to national identity, immigration restriction, nationally controlled economy, left-wing antiglobalism
# capitalist logic underlying globalization result in asymmetrical power relations, alternative globalization
# democratic globalization
