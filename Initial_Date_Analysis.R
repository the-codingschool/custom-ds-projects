#### load required libraries ####
library(ggplot2)
library(dplyr)

#### Data Cleaning ####
#View(abortion_data)

#clean_data <- abortion_data %>%
  #rename(categorize_tweet = Categorize.the.tweet) %>%
  #select(text:categorize_tweet) 
  
#pro_life_data <- clean_data %>%
  #filter(categorize_tweet == "Pro-life")

#View(pro_life_data)

#pro_choice_data <- clean_data %>%
  #filter(categorize_tweet == "Pro-choice")

#View(pro_choice_data)

View(election_data)

vs_data <- election_data %>%
  filter(handle == "realDonaldTrump")
  select(-(longitude:place_bounding_box)) %>%
  select(-(is_retweet:lang), -(truncated:extended_entities)) %>%
  arrange(desc(retweet_count)) 
  
View(vs_data)



#### plotting ####
## plotting retweet_count by the favorite_count, colors is hillary vs trump
colors()
ggplot(data = vs_data, aes(x = favorite_count, y = retweet_count)) +
  geom_point(color = "steelblue3")

