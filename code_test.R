library(tidyverse)

url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
tab <- html_table(html_nodes(read_html(url),"table")[[1]],fill = TRUE) %>% 
  select('State or territory','Census population') %>%
  rename(state = 'State or territory', population = 'Census population') %>%
  filter(!state %in% c('2010')) %>% 
  mutate(population = as.numeric(str_replace_all(population,",|\\[\\d{1,2}\\]","")))


test_df <- data.frame(
  x = 1,
  y = 2,
  double_x = 2,
  double_y = 4)

test_df %>%
  rename_with(toupper)
m<- as.character(1)
test_df %>%
  rename_with(~ sub("y", paste0("s",n),.x))
