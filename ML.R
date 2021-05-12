library(tidyverse)
library(rvest)
library(scales)
library(maps)
library(lubridate)
library(gridExtra)


read <- function(n){
  dates <<- read_csv(paste0("Vaccination-files/Vaccinations",
                            as.character(n),".csv"),n_max = 1) %>% 
    str_extract("[A-Z][a-z]*\\s\\d{1,2}\\s2021") %>% mdy()
  read_csv(paste0("Vaccination-files/Vaccinations",
                  as.character(n),".csv"),skip = 2)
}

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

#######################################
library(caTools)
Vaccines <- suppressWarnings(stateselect(read(6))) %>% 
  filter(!is.na(pvi))
split <- sample.split(Vaccines$pvi, SplitRatio = 0.8)
training_set <- subset(Vaccines, split == TRUE)
test_set <- subset(Vaccines, split == FALSE)

regressor <- lm(one_dose_percent ~ pvi,
                data = training_set)
summary(regressor)

pred <- predict(regressor,newdata = test_set)
ggplot() +
  geom_point(aes(test_set$pvi,test_set$one_dose_percent),
             color = "red") +
  geom_line(aes(training_set$pvi,y = predict(regressor,newdata = training_set)),
            color = "blue")
ggplot() +
  geom_point(aes(training_set$pvi,training_set$one_dose_percent),
             color = "red") +
  geom_line(aes(training_set$pvi,y = predict(regressor,newdata = training_set)),
            color = "blue")

#########################################
#Testing Sensitivity and Specificity in class attendance by sex
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(c("Female", "Male"))
mean(y == y_hat)
cm <- table(y_hat,y)
specificity(data = y_hat,reference = y)
###########################################
# using Iris data set to practice 
library(tidyverse)
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
# splitting the data into a training and test set
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
#examining the best predictor
L <- train$Petal.Length # after switching from length, width, sepal and petal we find the best predictor
cutoff <- seq(min(L), max(L),0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(L > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
#over trainning leads to less accurate results in the test data
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor")
mean(y_hat == train$Species)
#using test instead of train 
L <- test$Petal.Width
cutoff <- seq(min(L), max(L),0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(L > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)
#plot shows a combination of petal length and width might be more useful
plot(iris,pch=21,bg=iris$Species)
# Using a combination of petal length and width to predict species
y_hat <- ifelse(test$Petal.Width > 1.5 & test$Petal.Length > 4.7, "virginica", "versicolor")
mean(y_hat == train$Species)

###################################
#Conditional probabilities in a disease experiment
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

count <- disease-test
sum(count == 1)/1e6
x <- sum(disease[which(test == 1)])/length(which(test ==1))
x/0.02
#############################################
#Conditional probabilities in heights dataset
library(tidyverse)
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>% 
  qplot(height, p, data =.)

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x,y,data = .)
