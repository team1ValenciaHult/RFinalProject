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
library(wordcloud)
library(igraph)
library(ggraph)
library(wordcloud2)


# Reading Data ------------------------------------------------------------


# Reading the txt document
survey_answer <- read_document(file="testdocument.txt")


# Creating the dataframe
a <- 25 #how many observations to you have
b <- 6 #how many variables do you have
my_df_raw <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df_raw[i,z]<- survey_answer[i*b+z-b]
  }#closing z loop
}#closing i loop


# ID Creation
my_df_raw <- my_df_raw %>% 
  mutate(ID = seq(1:nrow(my_df_raw)))


# Pre Cleaning
  # The data was generated with google's speech to text API
  # the script automatically includes "You Said"

my_df_raw$V1 <- gsub("You said", "", my_df_raw$V1)
my_df_raw$V2 <- gsub("You said", "", my_df_raw$V2)
my_df_raw$V3 <- gsub("You said", "", my_df_raw$V3)
my_df_raw$V4 <- gsub("You said", "", my_df_raw$V4)
my_df_raw$V5 <- gsub("You said", "", my_df_raw$V5)
my_df_raw$V6 <- gsub("You said", "", my_df_raw$V6)

# Recoding of success - the person is single(1)
  # failure - the persion is in a relationship - not single (0)

my_df_raw[grep('yes',  my_df_raw$V1), 'V1'] <- 1
my_df_raw[grep('no',  my_df_raw$V1), 'V1'] <- 0


# my_df %>% melt(., 'ID') %>% View()
# Transforming DF to long format

melt(my_df_raw,id.vars = c('ID','V1')) %>% 
  View('melted')


my_df <- melt(my_df_raw,id.vars = c('ID','V1'))

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

par(mar=c(0,0,0,0))

# Wordcloud Single
single_struc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = brewer.pal(8, "Dark2"),
                   max.words=10000,
                   scale=c(1,1),
                   title.size=0.7,
                   match.colors = TRUE)

####### Not Single Wordcloud

not_single_struc <- my_df %>%
  filter(goal==0) %>%
  unnest_tokens(word, value) %>%
  # anti_join(stop_words) %>%
  count(word, sort = TRUE)


not_single_struc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = brewer.pal(10, "Dark2"),
                   max.words=1000,
                   scale=c(0.8,0.8),
                   title.size=1, 
                   random.order = FALSE,
                   match.colors = TRUE)

# Sentiment GGplot Fix
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



# Chantal TFIDF  ----------------------------------------------------------------

all_merged<- my_df %>% 
unnest_tokens(word, value) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word,questions,goal, sort = TRUE)

# SINGLE

# Single TFIDF ------------------------------------------------------------

all_merged_single <- all_merged %>% 
  filter(goal == 1)

#tf_idf single, not single by question from question 2 to 6
all_merged_single <- all_merged_single %>%
  bind_tf_idf(word, questions, n) # we need to add location information - in this case book

# all_merged_single # we get all the zeors because we are looking at stop words ... too common
# 
# all_merged_single %>%
#   arrange(desc(tf_idf))

#### tf_idf graph
all_merged_single %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(questions) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=questions))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~questions, ncol=2, scales="free")+
  coord_flip()



# TFIDF Not Single --------------------------------------------------------

#tf_idf not single by question from question 2 to 6

all_merged_nsingle <- all_merged %>% 
  filter(goal == 0)

all_merged_nsingle <- all_merged_nsingle %>%
  bind_tf_idf(word, questions, n) # we need to add location information - in this case book

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


# looking at the graphical apprach:
all_merged_nsingle %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(questions) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=questions))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~questions, ncol=2, scales="free")+
  coord_flip()

# Creating bigrams - Justyna

my_bigrams1 <- my_df %>%
  unnest_tokens(bigram, value, token = "ngrams", n=2)

my_bigrams1 %>%
  count(bigram, sort = TRUE) 

bigrams_separated <- my_bigrams1 %>%
  separate(bigram, c("word1", "word2"), sep = " ") # split them into word1 and word2

# filter stop words in both words - should not be stop word (not in stop words)
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words" for BUSINESS OUTCOME 1 - single:
bigram_counts1 <- bigrams_filtered %>%
  filter(goal == "1") %>%
  count(word1, word2, sort = TRUE)

#want to see the new bigrams
bigram_counts1


#creating the new bigram, "no-stop-words" for BUSINESS OUTCOME 0 - not single:
bigram_counts0 <- bigrams_filtered %>%
  filter(goal == "0") %>%
  count(word1, word2, sort = TRUE)

#want to see the new bigrams
bigram_counts0

# bigram_counts1 - bigrams for single people
# bigram_counts0 - bigrams for not single people


# Justyna -----------------------------------------------------------------

# Bigrams Creation #####

my_bigrams1 <- my_df %>%
  unnest_tokens(bigram, value, token = "ngrams", n=2)

my_bigrams1 %>%
  count(bigram, sort = TRUE) 

bigrams_separated <- my_bigrams1 %>%
  separate(bigram, c("word1", "word2"), sep = " ") # split them into word1 and word2


# filter stop words in both words - should not be stop word (not in stop words)
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words" for BUSINESS OUTCOME 1 - single:
bigram_counts1 <- bigrams_filtered %>%
  filter(goal == "1") %>%
  count(word1, word2, sort = TRUE)

#want to see the new bigrams
bigram_counts1

#creating the new bigram, "no-stop-words" for BUSINESS OUTCOME 0 - not single:
bigram_counts0 <- bigrams_filtered %>%
  filter(goal == "0") %>%
  count(word1, word2, sort = TRUE)

#want to see the new bigrams
bigram_counts0

# bigram_counts1 - bigrams for single people
# bigram_counts0 - bigrams for not single people


####### VISUALISING A BIGRAM NETWORK #################
# WILL BE USED FOR SHINY APP
# bigram_counts1 - bigrams for single people



bigram_graph1 <- bigram_counts1 %>%
  filter(n>0.5) %>% 
  graph_from_data_frame()


ggraph(bigram_graph1, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  ggtitle("Bigram Network for Respondents who are SINGLE")


# bigram_counts0 - bigrams for not single people
library(plotly)
library(visNetwork)

visNetwork(bigram_graph2) %>% 
  visLayout()

bigram_graph2 <- bigram_counts0 %>%
  filter(n>0.5) %>% 
  graph_from_data_frame() 

ggraph(bigram_graph2, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  ggtitle("Bigram Network for Respondents who are NOT SINGLE")

################################################################################################

# unite bigrams back
bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

# Creating tf-idf for bigrams per goal 0-1
bigram_tf_idf <- bigram_united %>%
  count(goal, bigram) %>%
  bind_tf_idf(bigram, goal, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf 


# Are we going to include this? -------------------------------------------


# IS THIS NECESSARY ? 


# Visualizing tf-idf for bigrams
library(scales)
# looking at the graphical apprach:
bigram_tf_idf %>%
  mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>%
  group_by(goal) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill=goal))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~goal, ncol=2, scales="free")+
  coord_flip()

# the most significant for not single = play mobile, for single - watch movies


# Naive Bayes -------------------------------------------------------------

library(quanteda)
library(tm)


# Tokenize and remove stopwords
naive_df <- my_df %>% 
            unnest_tokens(word, value) %>% 
            anti_join(stop_words)


# Random the order before put it in the model
# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(naive_df))

# Randomly order data
naive_df <- naive_df[rows, ]

# create corpus
opinions = Corpus(VectorSource(naive_df$word))



msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 1, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)


# Set the amount of test set
test_rows <- 5

#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[c(1:c(nrow(msg.dfm)-test_rows)),]
msg.dfm.test<-msg.dfm[c(c(nrow(msg.dfm)-test_rows+1):nrow(msg.dfm)),]


naive_df$goal <- as.numeric(naive_df$goal)

naive_df$goal %>% str()

#building the Naive Bayes model:
# NB_classifier <- textmodel_nb(msg.dfm.train, goal[c(1:123)])
NB_classifier <- textmodel_nb(msg.dfm.train, naive_df[c(1:c(nrow(msg.dfm)-test_rows)),'goal'])
# NB_test <- predict(NB_classifier, msg.dfm.test , force = TRUE)
summary(NB_classifier)

# Evaluating Model 

actual_class <- tail(naive_df,5)['goal']
predicted_class <-predict(NB_classifier, msg.dfm.test)
tab_class <- table(actual_class, predicted_class)
tab_class

confusionMatrix(tab_class, mode = "everything")



# Put the probability of each words in a dataframe
word_prob <- NB_classifier[[3]]
word_prob <- data.frame(t(word_prob)) %>% 
              rename(
                not_single = X0,
                single = X1
              )
word_prob$words <- row.names(word_prob) 


word_prob_melt <- word_prob %>% melt() 
forWordCloud<- word_prob[,c('single','words')]

forWordCloud <- forWordCloud %>% 
   rename(
     word = words,
     freq = single
          )

forWordCloud2 <- forWordCloud[c(2,1)]

forWordCloud2$freq <- (forWordCloud$freq*10)^2


head(forWordCloud2)

wordcloud2(forWordCloud2, 
           shuffle = FALSE, 
           shape = 'cardioid')


# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)

opinions = Corpus(VectorSource('beer husband'))
userInputDfm <- dfm(corpus(opinions), tolower = TRUE)
predict(NB_classifier, userInputDfm, force = TRUE, type = 'prob')

pred<- predict(NB_classifier, userInputDfm, force = TRUE)

ifelse(pred == '0', 'Taken :(', 'SINGLE!!! :)')

pred %>% as.character() %>% writeLines()
# compare the predict and actual
result <- data_frame(pred)
result$actual <- naive_df[c(c(nrow(msg.dfm)-test_rows+1):nrow(msg.dfm)),'goal']

result
