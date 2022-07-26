#### load in required libraries ####
library(ggplot2)


#### load is csv file better ####

election_data <- read.csv("JingjingsProject/data/tweets.csv")
View(election_data)

#### Save R object as file to Github better####
saveRDS(election_data, "JingjingsProject/data/tweets.csv")

