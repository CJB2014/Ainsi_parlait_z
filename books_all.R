
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)
library(SnowballC)
library(tm)
library(stringr)
library(ggplot2)
library(gutenbergr)


#---------------------------------------------------------------------------------
# Bring data in 
#---------------------------------------------------------------------------------
# books id : Dracula, heart of darkness, ulysses, beyond good and evil, scarlett letter

url_book <- c('http://www.gutenberg.org/cache/epub/345/pg345.txt',
              'https://www.gutenberg.org/files/4300/4300-0.txt',
              'https://www.gutenberg.org/files/219/219-0.txt',
              'http://www.gutenberg.org/cache/epub/4363/pg4363.txt',
              'http://www.gutenberg.org/cache/epub/33/pg33.txt')

titles_book <- c('Dracula','Heart_Of_Darkness','Ulysses','Beyond_Good_And_Evil','Scarlett_Letter')
books <- lapply(url_book, read_lines)
names(books) <- titles_book

allbooks <- lapply(books, as.data.frame,stringsAsFactors = F)
names(allbooks) <- titles_book

dima <- t(as.data.frame(lapply(allbooks, dim)))
allbooks <- do.call(rbind, allbooks)
allbooks$books <- rep(titles_book, times =dima[,1])



#---------------------------------------------------------------------------------
# Word stat
#---------------------------------------------------------------------------------

text_df <- text_df %>%
  mutate(linenumber = row_number(), 
         partie = cumsum(str_detect(ainsi, 'PARTIE')))%>%
  ungroup()


tidy_df <- text_df%>%
  unnest_tokens(word,ainsi)

stop_words_fr <- data.frame(word = stopwords('french'), stringsAsFactors = F)

tidy_df <- tidy_df %>%
  anti_join(stop_words_fr)%>%
  count(word, sort = T)

ggplot(data = head(tidy_df, n = 20), aes(word,n))+geom_bar(stat = 'identity')+coord_flip()


#---------------------------------------------------------------------------------
# Sentiment analysis  
#---------------------------------------------------------------------------------




#---------------------------------------------------------------------------------
# Topic Modelling 
#---------------------------------------------------------------------------------

