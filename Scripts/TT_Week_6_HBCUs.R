library(janitor)
library(tidyverse)
library(padr)
library(hablar)
library(ggbump)
library(ggthemes)
tuesdata <- tidytuesdayR::tt_load(2021, week = 6)
bachstudents <- tuesdata$bach_students %>% clean_names() %>% 
  select(total, white1, black1, hispanic, asian_pacific_islander_asian,
         asian_pacific_islander_pacific_islander, 
         american_indian_alaska_native, two_or_more_race) %>% 
  filter(total >= 2005) %>% pivot_longer(-total) %>% 
  group_by(name) %>% 
  mutate(rank = as.integer(rank(value, ties.method = "random")))

ggplot(bachstudents, aes(total, rank, color = name, group = name))+
  geom_bump(size = 2, smooth = 7) + geom_point(size=5) + theme_fivethirtyeight()

