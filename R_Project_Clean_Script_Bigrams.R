library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(reshape2)

# Loading survey data
survey_answer <- read_document(file="testdocument.txt")

#Creating a dataframe
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

# Removing "You said"
my_df$V1 <- gsub("You said", "", my_df$V1)
my_df$V2 <- gsub("You said", "", my_df$V2)
my_df$V3 <- gsub("You said", "", my_df$V3)
my_df$V4 <- gsub("You said", "", my_df$V4)
my_df$V5 <- gsub("You said", "", my_df$V5)
my_df$V6 <- gsub("You said", "", my_df$V6)

# Melt answers for all questions in one column + adding 0-1 variable for business outcome
my_df <- melt(my_df[2:ncol(my_df)],'ID')
my_df$goal <- c(1,0,1,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,0,1)

###############################################################################################


# Tokenizing while keeping location ids
my_df1 <- my_df %>%
  group_by(variable) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words)

View(my_df)
View(my_df1)

###############################################################################################

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

####### VISUALISING A BIGRAM NETWORK #################
# WILL BE USED FOR SHINY APP
# bigram_counts1 - bigrams for single people

library(igraph)
library(ggraph)

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

########################################################################

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

############################################################################


