library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tidyverse)
library(topicmodels)
library(ggplot2)
library(reshape2)
library(wordcloud)

setwd("C:/Users/user/Desktop/Hult/Courses/Business analyst/ModB/Text analysis/final project")
nm <- list.files(path="C:/Users/user/Desktop/Hult/Courses/Business analyst/ModB/Text analysis/final project")
my_data1 <- read_document(file=nm[1]) #This comes out as a vector
my_data_together <- paste(my_data1, collapse = " ") # This will give us a concatenated vector, put 16 data into 1

my_txt_text <- do.call(rbind, lapply(nm[1], function(x) paste(read_document(file=x), collapse = " ")))
View(my_txt_text)

survey_answer <- read_document(file="testdocument.txt")




a <- 25 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- survey_answer[i*b+z-b]
  }#closing z loop
}#closing i loop
View(my_df) 

my_df <- my_df %>% 
  mutate(ID = seq(1:nrow(my_df)))

my_df %>% View()

my_df$V1 <- gsub("You said", "", my_df$V1)
my_df$V2 <- gsub("You said", "", my_df$V2)
my_df$V3 <- gsub("You said", "", my_df$V3)
my_df$V4 <- gsub("You said", "", my_df$V4)
my_df$V5 <- gsub("You said", "", my_df$V5)
my_df$V6 <- gsub("You said", "", my_df$V6)

my_df <- melt(my_df[2:ncol(my_df)],'ID')
my_df$goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,0,1)



my_df %>% 
  unnest_tokens(word,value) %>% 
  count(ID,variable,word) %>% 
  cast_dtm(ID, word, n) #%>% 
# LDA(., k=2, control=list(seed=123)) %>% 
# tidy(., matrix="gamma") 
write.csv(my_df,'longFormat.csv')

####################################################
#sentiment nrc ggplot bar chart and wordcloud
################ Single nrc ggplot ##################
single_struc <- my_df %>%
  filter(goal==1) %>%
  unnest_tokens(word, value) %>%
  inner_join(get_sentiments("nrc"))%>%
  anti_join(stop_words) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

single_struc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Single sentiment nrc", x=NULL)+
  coord_flip()

# Single nrc wordcloud
single_struc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(1,1),
                   fixed.asp=TRUE,
                   title.size=1)
#not single nrc ggplot 
not_single_struc <- my_df %>%
  filter(goal==0) %>%
  unnest_tokens(word, value) %>%
  inner_join(get_sentiments("nrc"))%>%
  anti_join(stop_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

not_single_struc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Not single sentiment nrc", x=NULL)+
  coord_flip()

# Not single nrc wordcloud
not_single_struc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(1,1),
                   fixed.asp=TRUE,
                   title.size=1)
