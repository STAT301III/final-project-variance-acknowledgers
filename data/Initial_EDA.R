#load in packages needed for EDA
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(skimr)
library(janitor)
library(corrplot)

#read in raw data
uni <- read.csv('data/unprocessed/Unicorn_Companies.csv')


#Do a quick skim of the data
skim_without_charts(uni)

#Let's first turn the numerical columns that are currently characters to numerical:
uni_clean <- uni %>%
  mutate(Valuation...B. = as.double(substring(Valuation...B., 2)), 
         Date.Joined = as.Date(Date.Joined, "%m/%d/%Y"), )

#With total raised, usually follows $100.4M type of structure but sometimes is None


skim_without_charts(uni_clean)



#For the column with the investors, I want to first make it an array of names, then append them all into one big list
#and analyze the most commonly occurring companies, any companies that often appear with other companies, maybe some sort of 
#analysis of a company with its market cap, etc. 




