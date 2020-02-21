library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(tidyr)
library(tm)


setwd("~/Documents/Hult/Dual Degree/Module B/Text analytics/Class 1")
survey_answer <- read_document(file="testdocument.txt")


a <- 25 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))
my_df$id <- c(1:a)
  
for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- survey_answer[i*b+z-b]
  }#closing z loop
}#closing i loop

my_df$V1 <- gsub("You said", "", my_df$V1)
my_df$V2 <- gsub("You said", "", my_df$V2)
my_df$V3 <- gsub("You said", "", my_df$V3)
my_df$V4 <- gsub("You said", "", my_df$V4)
my_df$V5 <- gsub("You said", "", my_df$V5)
my_df$V6 <- gsub("You said", "", my_df$V6)

my_df <- melt(my_df[2:ncol(my_df)],'id')
my_df$goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,0,1)



# create corpus
opinions = Corpus(VectorSource(my_df$value))
inspect(opinions)


msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)


head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[c(1:123),]
msg.dfm.test<-msg.dfm[c(124,125),]

goal <- rep(c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1), times = 5)

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, goal[c(1:123)])
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred


