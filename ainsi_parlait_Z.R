
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)



ainsi <- read_lines('http://www.gutenberg.org/cache/epub/5258/pg5258.txt')


ainsi <- ainsi[-c(1:36)]
text_df <- data.frame(ainsi, stringsAsFactors = F)

tidy_df <- text_df %>%
  mutate(linenumber = row_number(), 
         partie = cumsum(str_detect(ainsi, 'PARTIE')))%>%
  ungroup()

