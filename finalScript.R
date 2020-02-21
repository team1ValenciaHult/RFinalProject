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


# Reading Data ------------------------------------------------------------


# Reading the txt document
survey_answer <- read_document(file="testdocument.txt")


# Creating the dataframe
a <- 25 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- survey_answer[i*b+z-b]
  }#closing z loop
}#closing i loop


# ID Creation
my_df <- my_df %>% 
  mutate(ID = seq(1:nrow(my_df)))


# Pre Cleaning
  # The data was generated with google's speech to text API
  # the script automatically includes "You Said"

my_df$V1 <- gsub("You said", "", my_df$V1)
my_df$V2 <- gsub("You said", "", my_df$V2)
my_df$V3 <- gsub("You said", "", my_df$V3)
my_df$V4 <- gsub("You said", "", my_df$V4)
my_df$V5 <- gsub("You said", "", my_df$V5)
my_df$V6 <- gsub("You said", "", my_df$V6)

# Recoding of success - the person is single(1)
  # failure - the persion is in a relationship - not single (0)

my_df[grep('yes',  my_df$V1), 'V1'] <- 1
my_df[grep('no',  my_df$V1), 'V1'] <- 0

my_df %>% melt(., 'ID') %>% View()
# Transforming DF to long format

melt(my_df,id.vars = c('ID','V1')) %>% View('melted')


my_df <- melt(my_df,id.vars = c('ID','V1'))

# Changing all column names just in case 
colnames(my_df) <- c('ID', 'goal', 'questions', 'value')


my_df$questions <- gsub('V','Q', my_df$questions)

# Josh and Abdul ----------------------------------------------------------

# Diego Notes: 
  # The business insight is not too clear in this example
  # We don't have that many words for not single people, the majority of them are in the middle
  

##### Single people wordcloud 

# Unnesting tokens and filtering by single answers

single_struc <- my_df %>%
  filter(goal==1) %>% # SINGLE!
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# Wordcloud Single
single_struc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = brewer.pal(10, "Dark2"),
                   max.words=1000,
                   scale=c(1,1),
                   title.size=1)

####### Not Single Wordcloud

not_single_struc <- my_df %>%
  filter(goal==0) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)


not_single_struc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = brewer.pal(10, "Dark2"),
                   max.words=1000,
                   scale=c(1,1),
                   title.size=1.2, 
                   random.order = FALSE)

# Chantal TFIDF  ----------------------------------------------------------------


my_df %>% 
unnest_tokens(word, value) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word,questions, sort = TRUE) %>% View('myWay')


#tf_idf single, not single by question from question 2 to 6
all_merged_single <- all_merged_single %>%
  bind_tf_idf(word, success, n) # we need to add location information - in this case book

all_merged_single # we get all the zeors because we are looking at stop words ... too common

all_merged_single %>%
  arrange(desc(tf_idf))
###########tf_idf graph
all_merged_single %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(success) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=success))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~success, ncol=2, scales="free")+
  coord_flip()


######################################
#tf_idf not single by question from question 2 to 6
all_merged_nsingle <- all_merged_nsingle %>%
  bind_tf_idf(word, success, n) # we need to add location information - in this case book

all_merged_nsingle # we get all the zeors because we are looking at stop words ... too common

all_merged_nsingle %>%
  arrange(desc(tf_idf))
###########tf_idf graph
all_merged_nsingle %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(success) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=success))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~success, ncol=2, scales="free")+
  coord_flip()
#############
# looking at the graphical apprach:
all_merged_survey %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()
