#install.packages('tidyverse')
library(tidyverse)
#install.packages('dplyr')
library(tidyr)
library(dplyr)

SavantData <- read.csv('SavantHittingData19.csv', stringsAsFactors = F)
SavantData$count <- paste(SavantData$balls, SavantData$strikes, sep = "-")
PlayerList <- read.csv('PlayerList.csv', stringsAsFactors = F)

TwoStrikes <- filter(SavantData, strikes == '2')
Sample2 <- TwoStrikes %>% 
  sample_n(10)

ZeroZero <- filter(SavantData, count == "0-0")
ZeroOne <- filter(SavantData, count == "0-1")
ZeroTwo <- filter(SavantData, count == "0-2")
OneTwo <- filter(SavantData, count == "1-2")
TwoTwo <- filter(SavantData, count == "2-2")
OneZero <- filter(SavantData, count == "1-0")
TwoZero <- filter(SavantData, count == "2-0")
ThreeZero <- filter(SavantData, count == "3-0")
OneOne <- filter(SavantData, count == "1-1")
TwoOne <- filter(SavantData, count == "2-1")
ThreeOne <- filter(SavantData, count == "3-1")
ThreeTwo <- filter(SavantData, count == "3-2")

SS <- SavantData %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked"))) %>%
  filter (pitches > min(119)) %>%
  mutate(SwStk = ((swingingstrike/pitches)*100))

TwoStrikesSS <- TwoStrikes %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(119)) %>%
  mutate(SwStk = ((swingingstrike/pitches)*100))

ZeroZeroSS <- ZeroZero %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(219)) %>%
  mutate(SwStk00 = ((swingingstrike/pitches)*100))

ZeroOneSS <- ZeroOne %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(49)) %>%
  mutate(SwStk01 = ((swingingstrike/pitches)*100))

ZeroTwoSS <- ZeroTwo %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(49)) %>%
  mutate(SwStk02 = ((swingingstrike/pitches)*100))

OneTwoSS <- OneTwo %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(29)) %>%
  mutate(SwStk12 = ((swingingstrike/pitches)*100))

TwoTwoSS <- TwoTwo %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(39)) %>%
  mutate(SwStk22 = ((swingingstrike/pitches)*100))

OneZeroSS <- OneZero %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(39)) %>%
  mutate(SwStk10 = ((swingingstrike/pitches)*100))

TwoZeroSS <- TwoZero %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(39)) %>%
  mutate(SwStk20 = ((swingingstrike/pitches)*100))

ThreeZeroSS <- ThreeZero %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(19)) %>%
  mutate(SwStk30 = ((swingingstrike/pitches)*100))

OneOneSS <- OneOne %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(49)) %>%
  mutate(SwStk11 = ((swingingstrike/pitches)*100))

TwoOneSS <- TwoOne %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(49)) %>%
  mutate(SwStk21 = ((swingingstrike/pitches)*100))

ThreeOneSS <- ThreeOne %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  filter (pitches > min(19)) %>%
  mutate(SwStk31 = ((swingingstrike/pitches)*100))

ThreeTwoSS <- ThreeTwo %>%
  group_by(player_name) %>%
  summarise(
    pitches = n(),
    swingingstrike = sum((description == "swinging_strike")+(description == "swinging_strike_blocked")
    )) %>%
  mutate(SwStk32 = ((swingingstrike/pitches)*100))

Joined <- merge(x=PlayerList, y=ZeroZeroSS, by = 'player_name', all.x = TRUE)
Joined1 <- merge(x=Joined, y=ZeroOneSS, by = 'player_name', all.x = TRUE)
Joined2 <- merge(x=Joined1, y=ZeroTwoSS, by = 'player_name', all.x = TRUE)
Join2 <- Joined2[c('player_name','SwStk00','SwStk01', 'SwStk02')]
Joined3 <- merge(x=Join2, y=OneZeroSS, by = 'player_name', all.x = TRUE)
Joined4 <- merge(x=Joined3, y=OneOneSS, by = 'player_name', all.x = TRUE)
Joined5 <- merge(x=Joined4, y=OneTwoSS, by = 'player_name', all.x = TRUE)
Join3 <- Joined5[c('player_name','SwStk00','SwStk01', 'SwStk02', 'SwStk10', 'SwStk11','SwStk12')]
Joined6 <- merge(x=Join3, y=TwoOneSS, by = 'player_name', all.x = TRUE)
Joined7 <- merge(x=Joined6, y=TwoTwoSS, by = 'player_name', all.x = TRUE)
Joined8 <- merge(x=Joined7, y=TwoZeroSS, by = 'player_name', all.x = TRUE)
Join4 <- Joined8[c('player_name','SwStk00','SwStk01', 'SwStk02', 'SwStk10', 'SwStk11','SwStk12', 'SwStk21', 'SwStk22', 'SwStk20')]
Joined9 <- merge(x=Join4, y=ThreeOneSS, by = 'player_name', all.x = TRUE)
Joined10 <- merge(x=Joined9, y=ThreeZeroSS, by = 'player_name', all.x = TRUE)
Joined11 <- merge(x=Joined10, y=ThreeTwoSS, by = 'player_name', all.x = TRUE)
Join5 <- Joined11[c('player_name','SwStk00','SwStk01', 'SwStk02', 'SwStk10', 'SwStk11','SwStk12', 'SwStk21', 'SwStk22', 'SwStk20', 'SwStk31','SwStk30','SwStk32')]
FinalJoin <- merge(x=Join5, y=SS, by = 'player_name', all.x = TRUE)
Join6 <- FinalJoin[c('player_name','SwStk','SwStk00','SwStk01', 'SwStk02', 'SwStk10', 'SwStk11','SwStk12', 'SwStk21', 'SwStk22', 'SwStk20', 'SwStk31','SwStk30','SwStk32')]

Cleaned <- Join6[rowSums(is.na(Join5[ ,2:13]))== 0,]
FinalSwStk <- Cleaned[complete.cases(Cleaned[,2:13]),]

write.csv(FinalSwStk,"SwingingStrikeCount.csv",row.names = F)
