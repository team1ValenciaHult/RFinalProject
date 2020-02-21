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

my_df$goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,0,1)



# create corpus for question 2
opinions = Corpus(VectorSource(my_df$V2))
inspect(opinions)


msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)


head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[c(1:23),]
msg.dfm.test<-msg.dfm[c(24,25),]

goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1)

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, goal[c(1:23)])
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred_q2 <- predict(NB_classifier, msg.dfm.test, type = "probability")
pred_q2


# create corpus for question 3
opinions = Corpus(VectorSource(my_df$V3))
inspect(opinions)


msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)


head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[c(1:23),]
msg.dfm.test<-msg.dfm[c(24,25),]

goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1)

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, goal[c(1:23)])
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred_q3 <- predict(NB_classifier, msg.dfm.test, type = "probability")
pred_q3


# create corpus for question 4
opinions = Corpus(VectorSource(my_df$V4))
inspect(opinions)


msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)


head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[c(1:23),]
msg.dfm.test<-msg.dfm[c(24,25),]

goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1)

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, goal[c(1:23)])
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred_q4 <- predict(NB_classifier, msg.dfm.test, type = "probability")
pred_q4


# create corpus for question 5
opinions = Corpus(VectorSource(my_df$V5))
inspect(opinions)


msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)


head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[c(1:23),]
msg.dfm.test<-msg.dfm[c(24,25),]

goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1)

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, goal[c(1:23)])
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred_q5 <- predict(NB_classifier, msg.dfm.test, type = "probability")
pred_q5


# create corpus for question 6
opinions = Corpus(VectorSource(my_df$V6))
inspect(opinions)


msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)


head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[c(1:23),]
msg.dfm.test<-msg.dfm[c(24,25),]

goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1)

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, goal[c(1:23)])
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred_q6 <- predict(NB_classifier, msg.dfm.test, type = "probability")
pred_q6


pred_q2
pred_q3
pred_q4
pred_q5
pred_q6

result <- data_frame(Q2 = pred_q2[,c(2)], Q3 = pred_q3[,c(2)], Q4 = pred_q4[,c(2)], Q5 = pred_q5[,c(2)], Q6 = pred_q6[,c(2)])

