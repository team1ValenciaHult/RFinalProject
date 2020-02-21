library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(reshape2)

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
# data exploration --------------------------------------------------------


# Extract question 1 in the a dataframe and delete "you said"
Q1_txt <- my_df$V1
Q1_txt <- substr(Q1_txt, start=10 , stop = 10000)
Q1_df <- data_frame(line=1:a, text=Q1_txt)
print(Q1_df)

# Extract question 2 in the a dataframe and delete "you said"
Q2_txt <- my_df$V2
Q2_txt <- substr(Q2_txt, start=10 , stop = 10000)
Q2_df <- data_frame(line=1:a, text=Q2_txt)
print(Q2_df)

# Extract question 3 in the a dataframe and delete "you said"
Q3_txt <- my_df$V3
Q3_txt <- substr(Q3_txt, start=10 , stop = 10000)
Q3_df <- data_frame(line=1:a, text=Q3_txt)
print(Q3_df)

# Extract question 4 in the a dataframe and delete "you said"
Q4_txt <- my_df$V4
Q4_txt <- substr(Q4_txt, start=10 , stop = 10000)
Q4_df <- data_frame(line=1:a, text=Q4_txt)
print(Q4_df)

# Extract question 5 in the a dataframe and delete "you said"
Q5_txt <- my_df$V5
Q5_txt <- substr(Q5_txt, start=10 , stop = 10000)
Q5_df <- data_frame(line=1:a, text=Q5_txt)
print(Q5_df)

# Extract question 6 in the a dataframe and delete "you said"
Q6_txt <- my_df$V6
Q6_txt <- substr(Q6_txt, start=10 , stop = 10000)
Q6_df <- data_frame(line=1:a, text=Q6_txt)
print(Q6_df)

View(Q6_df)



# Tokenize question 1, remove stop words, and count
Q1_struc <- Q1_df %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE)
print(Q1_struc)



# Tokenize question 2, remove stop words, and count
Q2_struc <- Q2_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
print(Q2_struc)

################################Q2 single, not single
Q2_struc_single <- my_df %>%
  filter(goal==1, variable=="V2")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q2_struc_single)

Q2_struc_nsingle <- my_df %>%
  filter(goal==0, variable=="V2")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q2_struc_nsingle)

# Tokenize question 3, remove stop words, and count
Q3_struc <- Q3_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
print(Q3_struc)

# Tokenize question 4, remove stop words, and count
Q4_struc <- Q4_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
print(Q4_struc)

# Tokenize question 5, remove stop words, and count
Q5_struc <- Q5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
print(Q5_struc)

# Tokenize question 6, remove stop words, and count
Q6_struc <- Q6_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)
print(Q6_struc)

########################################################################################################

all_merged_survey <- bind_rows(
  mutate(Q1_struc, question = 'Q1'),
  mutate(Q2_struc, question = 'Q2'),
  mutate(Q3_struc, question = 'Q3'),
  mutate(Q4_struc, question = 'Q4'),
  mutate(Q5_struc, question = 'Q5'),
  mutate(Q6_struc, question = 'Q6')
)


merged_survey_dtm <- all_merged_survey %>% 
  cast_dtm(question, word, n)

ap_lda <- LDA(merged_survey_dtm, k=2, control=list(seed=123))
ap_lda



#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"
library(tidytext)
library(ggplot2)
library(dplyr)

ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics


top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

beta_spread <- ap_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic2/topic1)) %>% 
  arrange(desc(log_rate))

beta_spread

# tf_idf
all_merged_survey <- all_merged_survey %>%
  bind_tf_idf(word, question, n) # we need to add location information - in this case book

all_merged_survey # we get all the zeors because we are looking at stop words ... too common

all_merged_survey %>%
  arrange(desc(tf_idf))
#what can we say about these words?

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

###############################################################################################################

# What is the feeling behind their words when they answered Q1
Q1_feeling <- Q1_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE)
print(Q1_feeling)


# What is the feeling behind their words when they answered Q2
Q2_feeling <- Q2_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE)
print(Q2_feeling)


# What is the feeling behind their words when they answered Q3
Q3_feeling <- Q3_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE)
print(Q3_feeling)


# What is the feeling behind their words when they answered Q4
Q4_feeling <- Q4_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE)
print(Q4_feeling)


###################################################
log(5/3)
log(5/1)
log(5/3)
3*0.51
