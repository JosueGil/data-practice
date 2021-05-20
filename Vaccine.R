#Data Analysis of Vaccine data provided by the CDC for each week after March 25, 2021
#Author: Josue Gil

library(tidyverse)
library(rvest)
library(scales)
library(maps)
library(lubridate)
library(gridExtra)

#defining a function that reads csv files with a specific number and establishes a date.
read <- function(n){
  dates <<- read_csv(paste0("Vaccination-files/Vaccinations",
                             as.character(n),".csv"),n_max = 1) %>% 
    str_extract("[A-Z][a-z]*\\s\\d{1,2}\\s2021") %>% mdy()
  read_csv(paste0("Vaccination-files/Vaccinations",
                  as.character(n),".csv"),skip = 2)
}
#The stateselect() function takes a data frame of vaccine data and 
#selects wanted columns, renames columns and removes rows that
#are not wanted, as well as adding columns for pvi, and population
stateselect <- function(n){#selecting the wanted variables of the vaccine data
  #added a dataset of the US population to join with the vaccine dataset
  url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
  tab <- html_table(html_nodes(read_html(url),"table")[[1]],fill = TRUE) %>% 
    select('State or territory','Census population[6][a]') %>%
    rename(state = 'State or territory', population = 'Census population[6][a]') %>%
    filter(!state %in% c('2010')) %>% 
    mutate(population = as.numeric(str_replace_all(population,",|\\[\\d{1,2}\\]","")))
  #Faster to rename the columns in the CSV file but used dplyr for practice.
  Vaccines <- n %>% select('State/Territory/Federal Entity', 'Total Doses Delivered',
                           'Total Doses Administered by State where Administered',
                           'Doses Administered per 100k by State where Administered',
                           'People with at least One Dose by State of Residence',
                           'Percent of Total Pop with at least One Dose by State of Residence',
                           'People Fully Vaccinated by State of Residence',
                           'Percent of Total Pop Fully Vaccinated by State of Residence')
  #Renaming the variables to shorter titles 
  Vaccines<- rename(Vaccines,state = 'State/Territory/Federal Entity',
                    total_d = 'Total Doses Delivered',
                    total_a = 'Total Doses Administered by State where Administered', 
                    total_a_per100k = 'Doses Administered per 100k by State where Administered',
                    one_dose = 'People with at least One Dose by State of Residence', 
                    one_dose_percent ='Percent of Total Pop with at least One Dose by State of Residence',
                    fully_vaccinated = 'People Fully Vaccinated by State of Residence', 
                    fully_vaccinated_percent ='Percent of Total Pop Fully Vaccinated by State of Residence')
  #adding population to the vaccine dataset, changing class of variables and filtering lower population territories.
  Vaccines[43,1] <- "New York"
  Vaccines <- Vaccines %>% left_join(tab) %>% 
    mutate(fully_vaccinated_percent = as.numeric(fully_vaccinated_percent),
           one_dose_percent = as.numeric(one_dose_percent),
           total_a_per100k = as.numeric(total_a_per100k)) %>%
    filter(!is.na(one_dose_percent), population > 200000)
  #adding party leanings for each state
  tab1 <- read_csv("RDstate.csv") %>% select(State,pvi) 
  tab1$pvi[which(is.na(tab1$pvi))] <- 0
  colnames(tab1)[1] <- "state"
  Vaccines<- left_join(Vaccines,tab1)
}
#Function that changes units to have K(thousands) or M (millions)
addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'))))  # in millions
  return(labels)
}
#We get some warnings during stateselect() that can be ignore so we remove them
#also asks for the especific file that wants to be viewed
Vaccines <- suppressWarnings(stateselect(read(readline(prompt="Enter file: "))))
#Function that shows the bar graph of Administered Vaccines colored by the % of pop with at least one dose
barmap <- function(){
  Vaccines %>% mutate(state = reorder(state,total_a)) %>% 
  ggplot(aes(state,total_a, fill = one_dose_percent)) + geom_col() + 
  scale_y_continuous(name = "Total Administered Vaccines",labels = addUnits)+ 
  theme(axis.text.y = element_text(size = 6.5),legend.title = element_text(size = 8,face = "bold"),
        plot.caption = element_text(size = 5.5))+ coord_flip()+ 
  labs(title = paste0("Total Doses administered in the U.S. by State/Territory (",dates,")"),
       caption = "Data Source: The Center for Disease Control", 
       x = "States/Territory", fill = "% at least one dose") +
  geom_text(aes(label = addUnits(total_a)),size = 2.5, hjust = -0.05)
}
#Function that shows a histogram of the percentages of people with one dose or 
#the percentage of people fully vaccinated
histogram <- function(n,m){
  ifelse(missing(m),m <-  1.5,m <- m)
  Vaccines %>% ggplot(aes(.data[[n]], y =..density..))  +
  geom_histogram(binwidth = m, fill = "#2f6696", color = "black")+ 
  geom_density(color = "red") + labs(y = "proportion States/Territory", x = "Percentage",
                                     title = paste0("Percentage of ",
                                                    str_replace_all(str_remove(n,"percent"),"_"," "),
                                                    "in the U.S. (",
                                                    dates,")"),
                                     caption = "Data Source: The Center for Disease Control and Prevention")+
  theme(plot.caption = element_text(size = 5.5))
}
#Function that shows a map of the US with percentage (fully vaccinated or one dose)
usmap <- function(n){
  usmap <- map_data("state")%>% select(region,long,lat,group)
  statesonly <- Vaccines %>% filter(!state %in% c("Puerto Rico","Indian Health Svc")) %>% 
    rename(region = state) %>% mutate(region = tolower(region))
  vaccinemap <- inner_join(usmap, statesonly,by = "region")
  rm("usmap","statesonly")
  vaccinemap %>% ggplot(aes(x=long,y=lat,fill= .data[[n]])) + 
    geom_polygon(aes(group = group), color = "black")+coord_fixed(1.3) + theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()) +  labs(title = paste0("Percentage of population ",
                                                           str_replace_all(str_remove(n,"percent"),"_"," "),
                                                           "in the U.S. (",
                                                         dates,")"),
                                          caption = "Data Source: The Center for Disease Control and Prevention",
                                          fill = "% Vaccinated")+ 
    theme(legend.title = element_text(size = 8,face = "bold"), 
        plot.caption = element_text(size = 5.5),
        plot.title = element_text(face = "bold"))
}
#Function showing scatterplot between party alignment 
#and Vaccine percentage (fully vaccinated or one dose) with a regression line
party_cor <- function(n){
  Vaccines %>% filter(! is.na(pvi)) %>% 
    ggplot(aes(pvi,.data[[n]])) +
    geom_point()+ geom_smooth(method = lm, formula = y ~ x)+
    labs(title = paste0(str_replace_all(str_remove(n,"percent"),"_"," "),
                        "vs. pvi (",dates,")"),
         y =str_replace_all(str_remove(n,"percent"),"_"," "))
}
stop()



##########################################################
# The rate per week of Vaccinations since March 25, 2021
#This function finds the rate of people with at least one dose of a specific file
Vaccines <- suppressWarnings(stateselect(read(4)))
N <- length(list.files("Vaccination-files"))
#Obtain a data frame with rates and vaccinations (at least one dose).
Vaccines <- Vaccines %>% select(state)
for(n in 1:N){
  m <- suppressWarnings(stateselect(read(n))) %>%
    mutate(rate = one_dose/population) %>%
    select('state','one_dose','rate')
  if (n == 1){Vaccines <- left_join(Vaccines,m, by = "state")}
  else{Vaccines <- bind_rows(Vaccines,m)}
}
rm(m)
# Created a visualization for change in proportion of Vaccinated people for each state.
rate <- Vaccines %>% group_by(state) %>%
  mutate(rate_change = 100*(max(rate) - min(rate)), mrate = max(rate)) %>% 
  ungroup() %>% mutate(state = reorder(state,mrate)) 
rategraph <- rate %>%
  ggplot(aes(rate,state, col = rate_change)) + 
  geom_line(show.legend = FALSE) + scale_color_gradient(low="yellow",
                                                        high="dark red") +
  geom_point() +geom_vline(xintercept = 0.85)
#rategraph

#Vaccine rates per week
Vaccines <- suppressWarnings(stateselect(read(1))) %>% select(state)
for (n in 1:3){
  w1 <- suppressWarnings(stateselect(read(n))) %>%  
    mutate(rate1 = one_dose/population) %>%
    select('state','rate1')
  w2 <- suppressWarnings(stateselect(read(n+1))) %>%  
    mutate(rate2 = one_dose/population) %>%
    select('state','rate2') %>% left_join(w1, by = "state") %>% 
    mutate(ratepw = rate2-rate1)
  Vaccines <- Vaccines %>% mutate(rate = w2$ratepw)
  colnames(Vaccines)[n+1]= paste(colnames(Vaccines)[n+1], 
                                 str_glue("week{n}"),sep = "_")
}
rm(w1,w2)
rate_per_state <- function(s){
  colnames(Vaccines)[-1] <- as.numeric(str_extract(colnames(Vaccines)[-1], "\\d{1,2}"))
  Vaccines %>% filter(state == s) %>% pivot_longer(cols = colnames(Vaccines)[-1],
                                                   names_to = "week") %>% 
    ggplot(aes(week,value)) + geom_point() + ylim(0,0.2)
}
#Vaccination Rate per week in the US
rate_us <- function(){
  tibble(rates_per_week = colMeans(Vaccines[-1]), week = 1:3)
}
rate_us() %>% ggplot(aes(week,rates_per_week)) +
  geom_point()+ ylim(0,0.05)
