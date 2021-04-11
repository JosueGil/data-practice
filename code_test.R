library(tidyverse)

url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
tab <- html_table(html_nodes(read_html(url),"table")[[1]],fill = TRUE) %>% 
  select('State or territory','Census population') %>%
  rename(state = 'State or territory', population = 'Census population') %>%
  filter(!state %in% c('2010')) %>% 
  mutate(population = as.numeric(str_replace_all(population,",|\\[\\d{1,2}\\]","")))


library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
t <- gutenberg_download(1342)
stop <- stop_words
words <- t %>% unnest_tokens(word,text) %>% filter(!word %in% stop$word&
                                                   !word %in% s) %>%
  group_by(word) %>% summarize(n())
i <- words %>% filter 
s <-words$word[which(str_detect(words$word,"\\d+"))]
afinn <- get_sentiments("afinn")
words <- t %>% unnest_tokens(word,text) %>%  filter(!word %in% stop$word&
                                                      !word %in% s) %>% 
  filter(word %in% afinn$word) %>% left_join(afinn) %>% select(word,value) %>% 
  filter(value == 4)
