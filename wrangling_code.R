#load the relevant libraries
library(tidyverse)
library(rstanarm)

#read in the data files
happy <- read_csv("hapiscore_whr.csv")
women <- read_csv("wn_bothhouses_c.csv")

#make data "tidy" with pivot_longer
happy <- happy %>% 
  pivot_longer(!country, names_to = "year", values_to = "happiness")

women <- women %>% 
  pivot_longer(!country, names_to = "year", values_to = "womenpercent")

#join tibbles
full <- right_join(happy, women)

#take out empty values 
adjust <- na.omit(full)

#make appropriate variables factors 
adjust$country <- as.factor(adjust$country)
adjust$year <- as.factor(adjust$year)
