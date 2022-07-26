library(ggplot2)
library(dplyr)


View(abortionTweets)

users <- search_users("pro life",
                      n = 500)

head(users, n = 2)
length(unique(users$location))

users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(20) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col(color = 'gold') + 
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Unique Locations of Users Who tweeted 'pro life' Recently")

View(users)
