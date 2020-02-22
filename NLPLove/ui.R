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
        sidebarMenu(menuItem("Sentiment", tabName = "sentiment", icon = icon("dashboard")),
                     menuItem("Characteristic Words", tabName = "TFIDF", icon = icon("chart-line")),
                     menuItem("Network Analysis", tabName = "networkAnalysis", icon = icon("th")),
                     menuItem('Naive Bayes Classification', tabName = 'naiveBayesClassification', icon = icon('heart')),
                    menuItem('Conclusion', tabName = 'conclusion', icon = icon('handshake')))),
    dashboardBody(
        tags$head(tags$style(HTML(".small-box {width: 300px}"))),
        tabItems(
            # First tab content
            tabItem(tabName = "sentiment",
                    h2("Sentiment Analysis"),
                    # Content
                    mainPanel(selectInput("selectPlotWordCloud", 
                                          "Choose desired relationship status", 
                                          choices = c("single",
                                                      "notSingle")), 
                              plotOutput("wordCloud"))),
                    
                    
            # Second Tab Content
            
            tabItem(tabName = "TFIDF",
                    h2("TFIDF Analysis"),
                    # Content 
                    mainPanel(selectInput("selectPlotTFIDF", 
                                          "Choose desired relationship status", 
                                          choices = c("single",
                                                      "notSingle")), 
                              plotOutput("TFIDF"))),
            
            # Third Tab Content
            
            tabItem(tabName = "networkAnalysis",
                    h2("Network Analysis"),
                    mainPanel(selectInput("selectPlotBigram", 
                                          "Choose desired relationship status", 
                                          choices = c("single",
                                                      "notSingle")), 
                              plotOutput("bigram"))),
            
            # Fourth Tab Content
            
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


