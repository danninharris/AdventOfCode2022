library(readxl)
library(tidyverse)

#### Data wrangling ####

#Read in data
df<-read_excel("Advent_day2.xlsx", col_names=F) 

#rename column to something more intiative 
df<-df%>%
  rename(opponent = ...1,
         you = ...2)

#### Part 1 ####

df1<- df %>%
  mutate(outcome = case_when((opponent == 'A') & (you == 'X') ~ 'draw',
                             (opponent == 'A') & (you == 'Y') ~ 'win',
                             (opponent == 'A') & (you == 'Z') ~ 'lose',
                             (opponent == 'B') & (you == 'X') ~ 'lose',
                             (opponent == 'B') & (you == 'Y') ~ 'draw',
                             (opponent == 'B') & (you == 'Z') ~ 'win',
                             (opponent == 'C') & (you == 'X') ~ 'win',
                             (opponent == 'C') & (you == 'Y') ~ 'lose',
                             (opponent == 'C') & (you == 'Z') ~ 'draw'))

df1<- df1 %>%
  mutate(outcome_points = case_when(outcome == 'win' ~ 6,
                             outcome == 'lose' ~ 0,
                             outcome == 'draw' ~ 3))
                             
df1<-df1%>%
  mutate(hand_played_points = case_when(you == 'X' ~ 1,
                                    you == 'Y' ~ 2,
                                     you == 'Z' ~ 3))

df1$score <- df1$outcome_points + df1$hand_played_points

sum(df1$score)

#Answer = 9241

#### Part 2 ####

df2<- df %>%
  mutate(need_to_play = case_when((opponent == 'A') & (you == 'X') ~ 'Z',
                             (opponent == 'A') & (you == 'Y') ~ 'X',
                             (opponent == 'A') & (you == 'Z') ~ 'Y',
                             (opponent == 'B') & (you == 'X') ~ 'X',
                             (opponent == 'B') & (you == 'Y') ~ 'Y',
                             (opponent == 'B') & (you == 'Z') ~ 'Z',
                             (opponent == 'C') & (you == 'X') ~ 'Y',
                             (opponent == 'C') & (you == 'Y') ~ 'Z',
                             (opponent == 'C') & (you == 'Z') ~ 'X'))

df2<- df2 %>%
  mutate(outcome_points = case_when(you == 'X' ~ 0,
                                    you == 'Y' ~ 3,
                                    you == 'Z' ~ 6))

df2<-df2%>%
  mutate(hand_played_points = case_when(need_to_play == 'X' ~ 1,
                                        need_to_play == 'Y' ~ 2,
                                        need_to_play == 'Z' ~ 3))

sum(df2$outcome_points + df2$hand_played_points)

#Answer = 14610

