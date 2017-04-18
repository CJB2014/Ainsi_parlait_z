
library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)
library(SnowballC)
library(tm)
library(stringr)
library(ggplot2)
library(gutenbergr)

afinn <- get_sentiments('afinn')
bing <- get_sentiments('bing')
nrc <- get_sentiments('nrc')

#---------------------------------------------------------------------------------
# Sentiment analysis  
#---------------------------------------------------------------------------------
dracula <- read_lines(url_book[1])
dracula <- dracula[-c(1:189)]
dracula <- as.data.frame(dracula, stringAsFactors = F)
dracula$dracula <- as.character(dracula$dracula)

#bing
tidy_dracula <- dracula%>%
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(dracula,'CHAPTER')))%>%
  unnest_tokens(word, dracula)

draculasent <- tidy_dracula %>%
  inner_join(bing)%>%
  count(chapter, index = linenumber %/% 80, sentiment)%>%
  spread(sentiment, n , fill = 0 )%>%
  mutate(sentiment = positive - negative)
  
ggplot(draculasent, aes(index, sentiment, fill = as.factor(chapter)))+
  geom_col(show.legend = F)+
  facet_wrap(~chapter, ncol = 5, scales = 'free_x')

library(wordcloud)

tidy_dracula%>%
  anti_join(stop_words)%>%
  count(word)%>%
  with(wordcloud(word,n,max.words = 100))



#---------------------------------------------------------------------------------
# Topic Modelling 
#---------------------------------------------------------------------------------
library(topicmodels)
library(tm)





















