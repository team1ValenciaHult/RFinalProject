library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(scales)
library(tidyverse)
library(reshape2)
library(wordcloud)


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



################ NRC ##################
single_struc <- my_df %>%
  filter(goal==1) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# Pizza Pie
single_struc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(1,1),
                   fixed.asp=TRUE,
                   title.size=1)

not_single_struc <- my_df %>%
  filter(goal==0) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# Pizza Pie
not_single_struc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(1,1),
                   fixed.asp=TRUE,
                   title.size=1)

################ bing ##################
# #single_struc_bing <- my_df %>%
#   filter(goal==1) %>%
#   unnest_tokens(word, value) %>%
#   anti_join(stop_words) %>%
#   inner_join(get_sentiments('bing')) %>%
#   count(sentiment, sort = TRUE)

# not_single_struc_bing <- my_df %>%
#   filter(goal==0) %>%
#   unnest_tokens(word, value) %>%
#   anti_join(stop_words) %>%
#   inner_join(get_sentiments('bing')) %>%
#   count(sentiment, sort = TRUE)




