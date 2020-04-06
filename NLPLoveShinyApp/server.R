
library(textreadr)
library(dplyr)
library(stringr)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(scales)
library(wordcloud)
library(igraph)
library(ggraph)
library(quanteda)
library(tm)
library(textdata)
library(readr)



# PreProcessing -----------------------------------------------------------
par(mar=c(0,0,0,0))


# Reading Sentiments ------------------------------------------------------


# Reading the txt document
survey_answer <- read_document(file="testdocument.txt")
nrcSentiments <-read_csv('nrcSentiments.csv')

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

# Recoding of questions - the person is single(1)
# failure - the persion is in a relationship - not single (0)

my_df_raw[grep('yes',  my_df_raw$V1), 'V1'] <- 1
my_df_raw[grep('no',  my_df_raw$V1), 'V1'] <- 0


# my_df %>% melt(., 'ID') %>% View()
# Transforming DF to long format

# melt(my_df_raw,id.vars = c('ID','V1')) %>% View('melted')


my_df <- melt(my_df_raw,id.vars = c('ID','V1'))

# Changing all column names just in case 
colnames(my_df) <- c('ID', 'goal', 'questions', 'value')

# Before V was changed to Q
# my_df$questions <- gsub('V','Q', my_df$questions)


my_df$questions <- gsub('V2','How often do yo go out',my_df$questions)
my_df$questions <- gsub('V3','What do yo do on your free time',my_df$questions)
my_df$questions <- gsub('V4','What is the relationship with your parents',my_df$questions)
my_df$questions <- gsub('V5','What is your Ideal Vacation',my_df$questions)
my_df$questions <- gsub('V6','What is your typical evening',my_df$questions)

my_df$questions




# Josh and Abdul Wordclouds -----------------------------------------------
# GGPLOT Single

single_struc <- my_df %>%
    filter(goal==1) %>%
    unnest_tokens(word, value) %>%
    inner_join(nrcSentiments)%>%
    anti_join(stop_words) %>%
    count(word, sentiment, sort = TRUE)%>%
    ungroup()

wordcloud_single <- single_struc %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word=reorder(word, n)) %>%
    ggplot(aes(word, n, fill=sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y")+
    labs(y="Single sentiment", x=NULL)+
    labs(x=NULL, y="")+
    coord_flip()



# Not Single GGPLOT 

#not single nrc ggplot 
not_single_struc <- my_df %>%
    filter(goal==0) %>%
    unnest_tokens(word, value) %>%
    inner_join(nrcSentiments)%>%
    anti_join(stop_words) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

wordcloud_nsingle <- not_single_struc %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word=reorder(word, n)) %>%
    ggplot(aes(word, n, fill=sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y")+
    labs(y="Not single sentiments", x=NULL)+
    labs(x=NULL, y="")+
    coord_flip()


l_wordcloud = list(single = list(single_struc, wordcloud_single ),
         notSingle = list(not_single_struc, wordcloud_nsingle))

choice_data_wordcloud <- names(l_wordcloud)


# Chantal TFIDF  ----------------------------------------------------------------

# Dataframe for section 

all_merged<- my_df %>% 
    unnest_tokens(word, value) %>%
    anti_join(stop_words) %>% #here's where we remove tokens
    count(word,questions,goal, sort = TRUE)

# Single _____________________________________

# Filter single people
all_merged_single <- all_merged %>% 
    filter(goal == 1)

# TFIDF Creation
all_merged_single <- all_merged_single %>%
    bind_tf_idf(word, questions, n) 


# GGPLOT of Single people
tfidf_single_plot <- all_merged_single %>%
    arrange(desc(tf_idf)) %>%
    mutate(word=factor(word, levels=rev(unique(word)))) %>%
    group_by(questions) %>%
    top_n(5) %>%
    ungroup %>%
    ggplot(aes(word, tf_idf, fill=questions))+
    geom_col(show.legend=FALSE)+
    facet_wrap(~questions, ncol=2, scales="free")+
    labs(x=NULL, y="tf-idf")+
    coord_flip()

# Not Single ________________________________

# Filter not single people 
all_merged_nsingle <- all_merged %>% 
    filter(goal == 0)

# Create TFIDF
all_merged_nsingle <- all_merged_nsingle %>%
    bind_tf_idf(word, questions, n) 


# GGPLOT not single people

tfidf_nsingle_plot<- all_merged_nsingle %>%
    arrange(desc(tf_idf)) %>%
    mutate(word=factor(word, levels=rev(unique(word)))) %>%
    group_by(questions) %>%
    top_n(5) %>%
    ungroup %>%
    ggplot(aes(word, tf_idf, fill=questions))+
    geom_col(show.legend=FALSE)+
    labs(x=NULL, y="tf-idf")+
    facet_wrap(~questions, ncol=2, scales="free")+
    labs(x=NULL, y="tf-idf")+
    coord_flip()



l_tfidf = list(single = list(all_merged_single, tfidf_single_plot),
         notSingle = list(all_merged_nsingle, tfidf_nsingle_plot))



# Justyna Bigrams ---------------------------------------------------------

# Bigram Creation
my_bigrams1 <- my_df %>%
    unnest_tokens(bigram, value, token = "ngrams", n=2)

# Bigram Cleaning
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



#creating the new bigram, "no-stop-words" for BUSINESS OUTCOME 0 - not single:
bigram_counts0 <- bigrams_filtered %>%
    filter(goal == "0") %>%
    count(word1, word2, sort = TRUE)

# Plots 

# Single 

bigram_graph1 <- bigram_counts1 %>%
    filter(n>0.5) %>% 
    graph_from_data_frame()

singleBigram<- ggraph(bigram_graph1, layout = "fr") +
    geom_edge_link()+
    geom_node_point()+
    geom_node_text(aes(label=name), vjust =1, hjust=1)+
    ggtitle("Token Network for Respondents who are SINGLE")

# Not Single 
bigram_graph2 <- bigram_counts0 %>%
    filter(n>0.5) %>% 
    graph_from_data_frame() 

notSingleBigram <- ggraph(bigram_graph2, layout = "fr") +
    geom_edge_link()+
    geom_node_point()+
    geom_node_text(aes(label=name), vjust =1, hjust=1)+
    ggtitle("Token Network for Respondents who are NOT SINGLE")


l_bigram = list(single = list(bigram_counts1, singleBigram),
               notSingle = list(bigram_counts0, notSingleBigram))




# Nick Naive Bayes -------------------------------------------------------------

# Tokenize and remove stopwords
naive_df <- my_df %>% 
    unnest_tokens(word, value) %>% 
    anti_join(stop_words)


# Random the order before put it in the model
# Set seed
set.seed(42)

# create corpus
opinions = Corpus(VectorSource(naive_df$word))


msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 1, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)


NB_classifier <- textmodel_nb(msg.dfm, naive_df[,'goal'])



# Server ------------------------------------------------------------------



server <- function(input, output) {

        
    output$wordCloud <- renderPlot({

        l_wordcloud[[input$selectPlotWordCloud]][2]
        
        
    })
    
    output$TFIDF <- renderPlot({
        
        l_tfidf[[input$selectPlotTFIDF]][2]
        
        
        
    })
    
    
    output$bigram <- renderPlot({
        
        l_bigram[[input$selectPlotBigram]][2]
        
        
    })
    
    
    output$bayes <- renderPrint({
        
        opinions = Corpus(VectorSource(input$bayestext))
        userInputDfm <- dfm(corpus(opinions), tolower = TRUE)
        predBayes <- predict(NB_classifier, userInputDfm, force = TRUE)
        predBayes <- predBayes %>% as.character()
        ifelse(predBayes == '0', 'Taken :(', 'SINGLE!!! :)') %>% writeLines()
        
    })
    
 }
