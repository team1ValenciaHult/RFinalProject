## About the app

This Shiny App focuses on solving the following problem: 

- What words can be used in order to determine if someone is single or taken?

Due to the complexity of this problem, the data was collected by asking five questions that are common in a social environment (e.g., bars, clubs, social gatherings, etc.).

These questions are:

1. How often do you go out?
2. What do you do in your free time?
3. How is your relationship with your parents? (Note: This is not a common topic in social gatherings)
4. What is your Ideal Vacation?
5. What is your typical evening?


The app contains the following tabs: 

- Introduction: Instructions about how to use the app. 
- Sentiment: Sentiment Analysis with the NRC lexicon.   
- Characteristic Words: Top TF-IDF vectorized tokens per question per single/not single answer. 
- Network Analysis: Network analysis of most frequent tokens per single/ not single answer. 
- Naive Bayes Classification: Interactive predictions based on Naive Bayes. If the word(s) is/are included in the training corpus, the algorithm will calculate the probability of success (single) or failure (not single).
- Conclusion: Overall recommendations based on the text analysis. 

## How does the Data Science Wingman/Wingwoman app works?

This app is designed for wingmen/wingwomen data scientists. <br>
    
The wingman/wingwoman will have to go to the 'Naive Bayes Classification' tab and input what the candidate for love is answering. The wingman/wingwoman can also explore the other tabs for insights from the analysis. 
<br>

The candidate for love is required to answer at least 1 of the following questions:
                         
1. How often do you go out?
2. What do you do in your free time?
3. How is your relationship with your parents? (Note: This is not a common topic in social gatherings)
4. What is your Ideal Vacation?
5. What is your typical evening?


