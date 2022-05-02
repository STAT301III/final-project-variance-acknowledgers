#load in packages needed for EDA
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(skimr)
library(janitor)
library(corrplot)
library(stringr)
library(lubridate)

#read in raw data
uni <- read.csv('data/unprocessed/Unicorn_Companies.csv')


#Do a quick skim of the data
skim_without_charts(uni_clean)


#Here, we can see that there is no missingness in the data at all, but upon inspecting the data
#that is because any missing values contain a character "None" in lieu of an NA


#Let's first turn the numerical columns that are currently characters to numerical:
uni_clean <- uni %>%
  mutate(valuation = as.numeric(extract_numeric(Valuation...B.)) * 1000000000,
         date_joined= extract_numeric(str_sub(Date.Joined, start= -4)),
         investor_count = extract_numeric(Investors.Count),
         deal_terms = extract_numeric(Deal.Terms),
         portfolio_exits = extract_numeric(Portfolio.Exits),
         total_raised = extract_numeric(Total.Raised) * 1000000000,
         ) %>% 
  select(-c(Date.Joined, Investors.Count, Valuation...B., Deal.Terms, Portfolio.Exits, Total.Raised)) %>%
  clean_names()

# taking investors out, then pivoting wider//onehotting
uni_clean <-
  uni_clean %>% 
  mutate(select_inverstors = str_replace(select_inverstors, ",", "|"),
         select_inverstors = str_replace(select_inverstors, ",", "|"),
         select_inverstors = str_replace(select_inverstors, ",", "|"),
         select_inverstors = str_replace(select_inverstors, ",", "|"),
         select_inverstors = str_remove_all(select_inverstors, ' '),
         select_inverstors = str_to_lower(select_inverstors)) %>% 
  separate_rows(select_inverstors, sep = "\\|")

# checking counts of investors
one_hot_investors <- uni_clean %>% 
  group_by(select_inverstors) %>% 
  summarise(counts = n()) %>% 
  arrange(desc(counts)) %>% 
  filter(counts > 5)

uni_clean <- 
  uni_clean %>% filter(select_inverstors %in% one_hot_investors$select_inverstors)

# now pivot wider
uni_clean <- uni_clean[-c(1063, 367), ]
uni_clean <- uni_clean %>% 
  mutate(value = 1) %>% 
  spread(select_inverstors, value, fill = 0)


#With total raised, usually follows $100.4M type of structure but sometimes is None

# function to fix data


skim_without_charts(uni_clean)



#For the column with the investors, I want to first make it an array of names, then append them all into one big list
#and analyze the most commonly occurring companies, any companies that often appear with other companies, maybe some sort of 
#analysis of a company with its market cap, etc. 




