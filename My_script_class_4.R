library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)



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
  mutate(Q6_struc, question = 'Q6'),
)

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
