library(dplyr)
library(tidyverse)
library(survival)
library(survminer)
options(scipen = 999)

master = read_csv("Master.csv")

batters = read_csv("Batting.csv")

head(master)

master = 
  master %>%
  select(playerID, nameFirst, nameLast, nameGiven, weight, height, bats, throws, debut, finalGame)

master = 
  master %>%
  mutate(finalGame = substr(finalGame, 0, 4)) %>%
  mutate(debut = substr(debut, 0, 4))

head(master)

batters =
  batters %>% group_by(playerID) %>%
  summarise(cG = sum(G), cAB = sum(AB), cR = sum(R), cH = sum(H), cRBI = sum(RBI), cSB = sum(SB), cSO = sum(SO), LastYear = max(yearID))

head(batters)

data = master %>% left_join(batters)

head(data)

data = data %>%
  filter(!is.na(weight), !is.na(height), !is.na(bats), !is.na(throws), !is.na(debut), !is.na(finalGame), !is.na(cG), !is.na(cAB), !is.na(cAB),
         !is.na(cR), !is.na(cH), !is.na(cRBI), !is.na(cSB), !is.na(cSO), !is.na(LastYear))

data = data %>%
  mutate(isIn = ifelse(LastYear > 2015, 1, 0))

playersIn = data %>%
  filter(isIn == 1)

playersOut = data %>%
  filter(isIn == 0)

