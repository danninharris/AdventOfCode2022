#Load packages
library(readxl)
library(tidyverse)

#### Data wrangling ####

#Read in data
df<-read_excel("Advent_day1.xlsx", col_names=F) 

#replace na's with zeros
df$...1<-replace_na(df$...1, 0)

#rename column to something more intiative 
df<-df%>%
  rename(calories = ...1)

#### Part 1 ####

#create a cumulative sum of calories for each elf
df1<-df %>%
  group_by(elf = cumsum(replace(calories, is.na(calories), 0) == 0)) %>%
  mutate(cumsum = cumsum(calories)) 

#extract the maximum number of calories carried by an elf
max(test$cumsum)

#Answer = 72240

#### Part 2 ####

##create a column containing the total calories for each elf and extract distinct values
df2<-df1 %>%
  group_by(elf)%>%
  mutate(total_calories = max(cumsum)) %>%
  select(-c(calories, cumsum))%>%
  arrange(desc(total_calories))%>%
  distinct()

#subset the top 3 columns
top3<-subset(df2[1:3,])

#sum the total calories column 
sum(top3$total_calories)

#Answer = 210957
