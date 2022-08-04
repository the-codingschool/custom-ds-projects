#### load in required libraries ####
library(ggplot2)

#### load in csv file ####
abortion_data <- read.csv("JingjingsProject/data/Roe-v-Wade-Dataset.csv")
View(abortion_data)

head(abortion_data)
                 
#### Save R object as file to Github ####     
saveRDS(abortion_data, "JingjingsProject/data/Roe-v-Wade-Dataset.csv")


