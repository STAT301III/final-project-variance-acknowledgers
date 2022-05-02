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


# UNIVARIATE ANALYSIS
# Response variables

ggplot (uni_clean) +
  geom_histogram(aes(x = valuation), bins = 50)

# Looking at the distribution of the valuation, we can see that the data is right skewed and centered around 1 billion. That is where the majority of observation can be seen and it makes sense that there is no left skew or even any values to the left of 1 billion because we are looking at unicorn companies. It means this data set will only have values above 1 billion. 

ggplot (uni_clean) +
  geom_histogram(aes(x = industry), stat = "count") +
  coord_flip()

# Looking at another potential response variable in industry, we see that internet software and services has the most observations. The second biggest we see is Fintech with other big industries including E-commerce, data management/analytics, cybersecurity, health, and AI. There is enough diversity in industries to conduct a thorough analysis. The majority of these are one off industries.

ggplot (uni_clean) +
  geom_histogram(aes(x = financial_stage), stat = "count") +
  coord_flip()


# We now look at our final potential response variable: the financial stage. It appears that this one will be much harder to predict as the overwhelming majority of observations don't have a financial stage listed. One reason we can think of for this is that it can be hard to discern, especially for private companies, what stage that they may be in. There may be multiple fundraising rounds and each company matures differently. 

# PREDICTORS
ggplot (uni_clean) +
  geom_histogram(aes(x = investor_count), bins = 50)
# Looking at the distribution of investor count of these unicorn companies, there is a rightward skew and the data is centered around 12 investors. There are outliers that have investors of over 75. We hypothesize the number of investors to be important as it gives a look into how much attention the company has drawn and how mature it may be when it comes to the valuation. 

ggplot (uni_clean) +
  geom_histogram(aes(x = country), stat = "count") +
  coord_flip()

# We look at the countries of each of these companies to understand if there could be a relationship between countries and a response variable like valuation. We can see that the US has an overwhelming majority of these unicorn companies with China a distant second. It gives an insight as to which contries it may be more likely that a unicorn company appears. 

ggplot (uni_clean) +
  geom_histogram(aes(x = founded_year), stat = "count") +
  coord_flip()

# First off, there is a leftwarad skew in this data but it is distributed normally overall. It is centered at 2015 which gives an idea into how long it takes a usual unicorn company to mature and reach unicorn status. 

# Bivariate Analysis
# Response Variable and Predictor Variable Relationships
ggplot (uni_clean, aes(x = investor_count, y = valuation)) +
  geom_point() +
  geom_smooth(method ='lm')

# Looking at the graph between investor count and valuation, there appears to be a slightly positive correlation between the two variables. There definitely are outliers but most of the data is clustered in the third quadrant, where investors are below 50 and valuation just above 1 billion. 

ggplot (uni_clean, aes(x = total_raised, y = valuation)) +
  geom_point() +
  geom_smooth(method ='lm')

# Looking at this graph, we see that there really is no relationship between total raised and valuation. It appears to be a rather unimpactful predictor as the data is almost normally distributed.

ggplot(uni_clean, aes(x = reorder(founded_year, -valuation), y = valuation)) +
  geom_bar(aes(y = valuation), stat = "identity") +
  coord_flip()

# Looking at this graph shows us that many current unicron companies with higher valuations were founded in the 2010s.

ggplot(uni_clean, aes(x = reorder(industry, -valuation), y = valuation)) +
  geom_bar(aes(y = valuation), stat = "identity") +
  coord_flip()

# We see some interesting insights when looking at this graph, Fintech is the industry that has the highest valued companies. In second, it is internet software and services. This is an interesting insight that gives a different angle than the univariate analysis on industries did. The univariate analysis told us that internet software and services had the most amount of companies. We see that most number of companies does not equate to highest valuations.

ggplot(uni_clean, aes(x = reorder(country, -valuation), y = valuation)) +
  geom_bar(aes(y = valuation), stat = "identity") +
  coord_flip()

# This analysis is in line with the country univariate analysis. The US has hte highest valued companies, then China, then India. 

# Relationships among predictor variables
ggplot (uni_clean, aes(x = investor_count, y = total_raised)) +
  geom_point() +
  geom_smooth(method ='lm')

# There is really no relationship between the number of investors in a company and the total amount raised. We see a very slight positive correlation and this can probably be attributed to the fact that a single investment entity can make a massive investment compared to others.

ggplot (uni_clean, aes(x = deal_terms, y = total_raised)) +
  geom_point() +
  geom_smooth(method ='lm')

# We see that there is a lack of a relationship between the number of deal terms and the total raised.

ggplot(uni_clean, aes(x = industry, fill = country)) + 
  geom_bar(position = "stack")

# When using this stacked bar graph to analyze categorical variables of industry and country, we see that in both fintech and internet services the US takes up a big fraction of the companies. Going forward when doing analysis, we will have to elimiante some of the smaller companies and possibly create a threshold for the countries we analyze as it is hard to see relationships with so many countries.



