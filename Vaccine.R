#Data Analysist of Vaccine data provided by the CDC on March 25, 2021
#The CSV file contains the number of Doses delivered and administered and all relevant data.
#It is organized by state/territory/Entity
#Author: Josue Gil

library(tidyverse)
library(rvest)
library(scales)
library(ggthemes)
library(maps)

#defining a function that reads csv files with a specific number.
read <- function(n){
  dates <<- read_csv(paste0("Vaccination-files/Vaccinations",
                             as.character(n),".csv"),n_max = 1) %>% 
    str_extract("[A-Z][a-z]*\\s\\d{1,2}\\s2021") %>% mdy()
  read_csv(paste0("Vaccination-files/Vaccinations",
                  as.character(n),".csv"),skip = 2)
}
#The stateselect() function takes a data frame of vaccine data and 
#selects wanted varaibles, renames variables and removes rows that
#are not wanted
stateselect <- function(n){#selecting the wanted variables of the vaccine data
  #added a dataset of the US population to join with the vaccine dataset
  url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
  tab <- html_table(html_nodes(read_html(url),"table")[[1]],fill = TRUE) %>% 
    select('State or territory','Census population') %>%
    rename(state = 'State or territory', population = 'Census population') %>%
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
                    fully_vac = 'People Fully Vaccinated by State of Residence', 
                    fully_vac_percent ='Percent of Total Pop Fully Vaccinated by State of Residence')
  #adding population to the vaccine dataset, changing class of variables and filtering lower population territories.
  Vaccines <- Vaccines %>% left_join(tab) %>% 
    mutate(fully_vac_percent = as.numeric(fully_vac_percent),
           one_dose_percent = as.numeric(one_dose_percent)) %>%
    filter(!is.na(one_dose_percent), population > 200000)
}
#Function that changes units to have K(thousands) or M (millions)
addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'))))  # in millions
  return(labels)
}
#We get some warnings during stateselect() that can be ignore so we remove them
Vaccines <- suppressWarnings(stateselect(read(readline(prompt="Enter file: "))))
#Bar graph of Administered Vaccines colored by the % of pop with at least one dose
barmap <- Vaccines %>% mutate(state = reorder(state, total_a)) %>% 
  ggplot(aes(state,total_a, fill = one_dose_percent)) + geom_col() + 
  scale_y_continuous(name = "Total Administered Vaccines",labels = addUnits)+ 
  theme(axis.text.y = element_text(size = 6.5),legend.title = element_text(size = 8,face = "bold"),
        plot.caption = element_text(size = 5.5))+ coord_flip()+ 
  labs(title = paste0("Total Doses administered in the U.S by State/Territory (",dates,")"),
       caption = "Data Source: The Center for Disease Control", 
       x = "States/Territory", fill = "% at least one dose") +
  geom_text(aes(label = addUnits(total_a)),size = 2.5, hjust = -0.05)

#Histogram of the percentages of people with one dose
histo <- Vaccines %>% ggplot(aes(one_dose_percent, y =..density..))  +
  geom_histogram(binwidth = 1.5, fill = "#224C98", color = "black")+ 
  geom_density(color = "red") + labs(y = "proportion States/Territory", x = "Percentage",
                                     title = paste0("Percentage of People with One Dose in the US (",
                                                    dates,")"),
                                     caption = "Data Source: The Center for Disease Control and Prevention")+
  theme(plot.caption = element_text(size = 5.5))

#Map of the US with percentage
usmap <- map_data("state")%>% select(region,long,lat,group)
statesonly <- Vaccines %>% filter(!state %in% c("Puerto Rico","Indian Health Svc")) %>% 
  rename(region = state) %>% mutate(region = tolower(region))
vaccinemap <- inner_join(usmap, statesonly,by = "region")
rm("usmap","statesonly")
mapv <- vaccinemap %>% ggplot(aes(x=long,y=lat,fill= one_dose_percent)) + 
  geom_polygon(aes(group = group), color = "black")+coord_fixed(1.3) + theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()) +  labs(title = paste0("Percentage of Population Vaccinated with at Least One Dose (",
                                                         dates,")"),
                                          caption = "Data Source: The Center for Disease Control and Prevention",
                                          fill = "% Vaccinated")+ 
  theme(legend.title = element_text(size = 8,face = "bold"), 
        plot.caption = element_text(size = 5.5),
        plot.title = element_text(face = "bold"))

#Added a qqplot of the percentage of one dose
qqplot <- Vaccines %>% ggplot(aes(sample = one_dose_percent))+ geom_qq()+ 
  geom_qq_line()
 

##########################################################
# The rate per week of Vaccinations since March 25, 2021
#This function finds the rate of people with at least one dose of a specific file
Vaccines <- suppressWarnings(stateselect(read(1)))
rate_finder <- function(n){
  m <- suppressWarnings(stateselect(read(n))) %>%
    mutate(rate = one_dose/population) %>%
    select('state','one_dose','rate')
  #colnames(m)[2:3]= paste(colnames(m)[2:3], str_glue("week{n}"),sep = "_")
  m
}
#Obtain a data frame with rates and vaccinations (at least one dose).
Vaccines <- Vaccines %>% select(state)
for(n in 1:3){
  m <- rate_finder(n)
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
rategraph
#Vaccine rates per week
Vaccines <- suppressWarnings(stateselect(read(1))) %>% select(state)
for (n in 1:2){
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


