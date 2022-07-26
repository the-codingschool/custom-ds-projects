#reticulate::py_install('transformers', pip = TRUE)
install.packages("tensorflow")
#library(keras)
library(tensorflow)
library(dplyr)
library(tfdatasets)
?keras
?tensorflow


#transformer = reticulate::import('transformers')

transformer$RobertaTokenizer$from_pretrained('roberta-base', do_lower_case=TRUE)
transformer$TFRobertaModel$from_pretrained('roberta-base')


