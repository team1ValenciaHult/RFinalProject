library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tidyverse)
library(topicmodels)
library(ggplot2)
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
  cast_dtm(ID, word, n) 


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

#####################################Q3 single, not singe
Q3_struc_single <- my_df %>%
  filter(goal==1, variable=="V3")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q3_struc_single)

Q3_struc_nsingle <- my_df %>%
  filter(goal==0, variable=="V3")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q3_struc_nsingle)


#####################################Q4 single, not singe
Q4_struc_single <- my_df %>%
  filter(goal==1, variable=="V4")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q4_struc_single)

Q4_struc_nsingle <- my_df %>%
  filter(goal==0, variable=="V4")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q4_struc_nsingle)

#####################################Q5 single, not singe
Q5_struc_single <- my_df %>%
  filter(goal==1, variable=="V5")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q5_struc_single)

Q5_struc_nsingle <- my_df %>%
  filter(goal==0, variable=="V5")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q5_struc_nsingle)

#####################################Q6 single, not singe
Q6_struc_single <- my_df %>%
  filter(goal==1, variable=="V6")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q6_struc_single)

Q6_struc_nsingle <- my_df %>%
  filter(goal==0, variable=="V6")%>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)
print(Q6_struc_nsingle)
###########location variable##########################################


all_merged_single <- bind_rows(
  mutate(Q2_struc_single, success = 'How often do yo go out?'),
  mutate(Q3_struc_single, success = 'Free time'),
  mutate(Q4_struc_single, success = 'Relationship with your parents'),
  mutate(Q5_struc_single, success = 'Ideal vacation'),
  mutate(Q6_struc_single, success = 'Typical saturday evening')
 
)

all_merged_nsingle <- bind_rows(
  mutate(Q2_struc_nsingle, success = 'How often do yo go out?'),
  mutate(Q3_struc_nsingle, success = 'Free time'),
  mutate(Q4_struc_nsingle, success = 'Relationship with your parents'),
  mutate(Q5_struc_nsingle, success = 'Ideal vacation'),
  mutate(Q6_struc_nsingle, success = 'Typical saturday evening')
)


######################################
#tf_idf single by question from question 2 to 6
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
  labs(x=NULL, y="")+
  facet_wrap(~success, ncol=2, scales="free")+
  coord_flip()


######################################
#tf_idf not single by question from question 2 to 6
all_merged_nsingle <- all_merged_nsingle %>%
  bind_tf_idf(word, success, n) 

all_merged_nsingle 
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
  labs(x=NULL, y="")+
  facet_wrap(~success, ncol=2, scales="free")+
  coord_flip()
