library(shinydashboard)
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




ui <- dashboardPage(
    dashboardHeader(title = "Data Science Wingman App"),
    dashboardSidebar(
        sidebarMenu(menuItem('Introduction',tabName = 'introduction', icon = icon('question')),
                    menuItem("Sentiment", tabName = "sentiment", icon = icon("dashboard")),
                     menuItem("Characteristic Words", tabName = "TFIDF", icon = icon("chart-line")),
                     menuItem("Network Analysis", tabName = "networkAnalysis", icon = icon("th")),
                     menuItem('Naive Bayes Classification', tabName = 'naiveBayesClassification', icon = icon('heart')),
                    menuItem('Conclusion', tabName = 'conclusion', icon = icon('handshake')))),
    dashboardBody(
        tags$head(tags$style(HTML(".small-box {width: 300px}
                                 div.box-header {text-align: center;}"))),
        tabItems(
            
            # First tab content
            tabItem(tabName = "introduction",
                    # Content
                    mainPanel(box(
                        title = "How does the Data Science Wingman/Wingwoman app work?", width = 10, background = "light-blue",
                        "This app is designed for data scientist wingmen/wingwomen. 
                        The candidate for love is required to answer multiple of the following questions below. 
                        The wingman/wingwoman will have to write to go to the 'Naive Bayes Classification' tab and 
                        input what the candidate for love is answering. The wingman/wingwoman can also explore the 
                        other tabs for insights of the analysis. ")),
                    verticalLayout(box('1. How often do yo go out?',width = 5, background = "green"),
                             box('2. What do yo do on your free time?',width = 5, background = "green"),
                             box('3. How is your relationship with your parents?',width = 5, background = "green"),
                             box('4. What is your Ideal Vacation?',width = 5, background = "green"),
                             box('5. What is your typical evening?',width = 5, background = "green"))
                    
                    
                    
                    ),
            
            # Second tab content
            tabItem(tabName = "sentiment",
                    h2("Sentiment Analysis"),
                    # Content
                    sidebarPanel(selectInput("selectPlotWordCloud", 
                                          "Choose desired relationship status", 
                                          choices = c("single",
                                                      "notSingle"))), 
                    mainPanel( plotOutput("wordCloud", width = '800px', height = '500px'))),
                    
                    
            # Third Tab Content
            
            tabItem(tabName = "TFIDF",
                    h2("TFIDF Analysis"),
                    # Content 
                    sidebarPanel(selectInput("selectPlotTFIDF", 
                                          "Choose desired relationship status", 
                                          choices = c("single",
                                                      "notSingle"))), 
                     mainPanel(plotOutput("TFIDF", width = '800px', height = '800px'))),
            
            # Fourth Tab Content
            
            tabItem(tabName = "networkAnalysis",
                    h2("Network Analysis"),
                    mainPanel(selectInput("selectPlotBigram", 
                                          "Choose desired relationship status", 
                                          choices = c("single",
                                                      "notSingle")), 
                              plotOutput("bigram", width = '800px', height = '800px'))),
            
            # Fifth Tab Content
            
            tabItem(tabName = "naiveBayesClassification",
                    h2("Naive Bayes Classification"),
                    mainPanel(  textInput("bayestext", 
                                          label = h3("Enter in the textbox the love candidate responses"), 
                                          value = "Enter text..."), 
                    valueBox(
                        uiOutput("bayes"), "New Orders", icon = icon("heart"),
                        href = "https://www.facebook.com/abdul.bishar"
                    ))),
            
            tabItem(tabName = "conclusion",
                    h2("Conclusion"),
                    div(img(src="conclusion.jpeg")))
                    # mainPanel(imageOutput("conclusion")))
            
            )
            
            
            )
        
        )



