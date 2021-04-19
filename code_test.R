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

library(lubridate)
read <- function(n){
  dates <<- read_csv(paste0("Vaccination-files/Vaccinations",
                       as.character(n),".csv"),n_max = 1) %>% 
    str_extract("[A-Z][a-z]*\\s\\d{1,2}\\s2021") 
  read_csv(paste0("Vaccination-files/Vaccinations",
                  as.character(n),".csv"),skip = 2)
}

read(1)
d <- mdy(dates)
month(d)

library(Lahman)
library(ggplot2)
library(tidyverse)
t <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(rpg = R/G,bbpg = BB/G, hrpg = HR/G)
lm(rpg~bbpg + hrpg,data = t)
lm(rpg~hrpg,data = t)

library(Lahman)
library(tidyverse)
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
year_fit <- function(n){
  fit <- Teams %>% 
    filter(yearID %in% n)  %>%  
    lm(R ~ BB + HR, data = .) %>% tidy( conf.int = TRUE)
  x <<- append(x,fit$estimate[2])
}
x <- c()
invisible(sapply(1961:2018,year_fit))
t <- tibble(year = 1961:2018, bb = x) 
t %>% ggplot(aes(year,bb)) + 
  geom_point() +
  geom_smooth(method=lm , color="blue", fill="#69b3a2", se=TRUE)
l <- tidy(lm(t$bb~t$year))



library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))
 
galton %>% group_by(pair) %>% 
  do(tidy(lm(childHeight~parentHeight, data = .),conf.int = TRUE))
library(textreadr)
#<table id="vaccinations-table" class="expanded-data-table vaccinations-fontsize">
url <-  "https://covid.cdc.gov/covid-data-tracker/#vaccinations"
tab <- download.file(url,"Download Data")
tab


library(tidyverse)
colnames(tab1)[1]


