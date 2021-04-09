library(tidyverse)

url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
h <- read_html(url)
nodes <- html_nodes(h,"table")
tab <- html_table(nodes[[1]],fill = TRUE) %>% 
  select('State or territory','Census population') %>%
  rename(state = 'State or territory', population = 'Census population') %>%
  filter(!state %in% c('2010')) %>% 
  mutate(population = as.numeric(str_replace_all(population,",|\\[\\d{1,2}\\]","")))


                