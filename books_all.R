
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




#---------------------------------------------------------------------------------
# Word stat -- Dracula 
#---------------------------------------------------------------------------------
dracula <- read_lines(url_book[1])
dracula <- dracula[-c(1:189)]
dracula <- as.data.frame(dracula, stringAsFactors = F)
dracula$dracula <- as.character(dracula$dracula)

stat_word <- function(df){
  names(df) <- 'text'

  df1 <- df %>%
    mutate(linenumber = row_number(), 
           partie = cumsum(str_detect(text, 'CHAPTER')))%>%
    ungroup()
  
  
  tidy_df <- df%>%
    unnest_tokens(word,text)
  
  data("stop_words")
  
  count_df <- tidy_df %>%
    anti_join(stop_words)%>%
    count(word, sort = T)
  
  plot_df <- ggplot(data = head(count_df, n = 20), aes(word,n))+geom_bar(stat = 'identity')+coord_flip()
  return(list(df1,tidy_df,count_df,plot_df))
}

drac <- stat_word(dracula)

#---------------------------------------------------------------------------------
# Word stat -- other book
#---------------------------------------------------------------------------------
##heart of darkness
HOD <- read_lines(url_book[3])
HOD <- HOD[-c(1:30)]
HOD <- as.data.frame(HOD)
HOD$HOD <- as.character(HOD$HOD)

HOD_stat <-stat_word(HOD)

##Ulysses
UL <- read_lines(url_book[2])
UL <- UL[-c(1:35)]
UL <- as.data.frame(UL, stringsAsFactors = F)

UL_stat <- stat_word(UL)

##scarlet letter 
SL <- read_lines(url_book[5])
SL <- SL[-c(1:169)]
SL <- as.data.frame(SL, stringsAsFactors = F)

SL_stat <- stat_word(SL)

##beyond good an evil
BGE <- read_lines(url_book[4])
BGE <- BGE[-c(1:77)]
BGE <- as.data.frame(BGE, stringsAsFactors = F)

BGE_stat <- stat_word(BGE)

#---------------------------------------------------------------------------------
# Sentiment analysis  
#---------------------------------------------------------------------------------




#---------------------------------------------------------------------------------
# Topic Modelling 
#---------------------------------------------------------------------------------

