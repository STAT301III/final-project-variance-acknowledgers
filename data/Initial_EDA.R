#load in packages needed for EDA
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(skimr)
library(janitor)
library(corrplot)
library(stringr)

#read in raw data
uni <- read.csv('data/unprocessed/Unicorn_Companies.csv')


#Do a quick skim of the data
skim_without_charts(uni)


#Here, we can see that there is no missingness in the data at all, but upon inspecting the data
#that is because any missing values contain a character "None" in lieu of an NA


#Let's first turn the numerical columns that are currently characters to numerical:
uni_clean <- uni %>%
  mutate(Valuation = case_when(Valuation == "None" ~ NA,
    TRUE ~ as.double(substring(Valuation...B., 2))), 
         Date.Joined = case_when(Date.Joined == "None" ~ NA,
           TRUE ~ as.Date(Date.Joined, "%m/%d/%Y")), 
         Investors = case_when(Select.Inverstors == "None" ~ NA,
           TRUE ~ Select.Inverstors),
         Total.Raised = case_when(Total.Raised == "None" ~ NA,
           substr(Total.Raised, nchar(Total.Raised), nchar(Total.Raised)) == "M" ~ as.double(substr(Total.Raised, 2, nchar(Total.Raised)-1))*1000000,
           substr(Total.Raised, nchar(Total.Raised), nchar(Total.Raised)) == "B" ~ as.double(substr(Total.Raised, 2, nchar(Total.Raised)-1))*1000000000
           ),
         Investors.Count = case_when(Investors.Count == "None" ~ NA,
           TRUE ~ as.integer(Investors.Count)),
         Deal.Terms = case_when(Deal.Terms == "None" ~ NA,
                                TRUE ~ as.integer(Deal.Terms)),
         Portfolio.Exits = case_when(Portfolio.Exits == "None" ~ NA,
                                TRUE ~ as.integer(Portfolio.Exits))
  )

#With total raised, usually follows $100.4M type of structure but sometimes is None


skim_without_charts(uni_clean)



#For the column with the investors, I want to first make it an array of names, then append them all into one big list
#and analyze the most commonly occurring companies, any companies that often appear with other companies, maybe some sort of 
#analysis of a company with its market cap, etc. 




