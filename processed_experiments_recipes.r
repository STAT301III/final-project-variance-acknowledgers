## creating expriments and finish processing data

# load packages
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(skimr)
library(janitor)
library(corrplot)
library(stringr)
library(lubridate)
library(tidymodels)
tidymodels_prefer()

# token libs
library(tokenizers) # you wont need this one loaded in the future as it is wrapped by other functions
library(tidytext)
library(textrecipes)  #step_* functions for NLP
library(SnowballC)  # wordStem()

#read in raw data
uni <- read.csv('data/unprocessed/Unicorn_Companies.csv')

# setting up real data from here
uni_clean <- uni %>%
  mutate(valuation = as.numeric(extract_numeric(Valuation...B.)) * 1000000000,
         date_joined= extract_numeric(str_sub(Date.Joined, start= -4)),
         investor_count = extract_numeric(Investors.Count),
         deal_terms = extract_numeric(Deal.Terms),
         portfolio_exits = extract_numeric(Portfolio.Exits),
         total_raised = extract_numeric(Total.Raised) * 1000000000,
         Select.Inverstors = str_split(Select.Inverstors, ",")
  ) %>%
  unnest(cols = c(Select.Inverstors)) %>% 
  select(-c(Date.Joined, Investors.Count, Valuation...B., Deal.Terms, Portfolio.Exits, Total.Raised)) %>%
  clean_names()

# Experiments :

# 1.) Predicting valuation - two models, one without nlp, one with
## obvious reason for conducting experiment - what variables effect valuation of a company
## do particular investors only join at certain stages of startups, if so what stage?
## experiment type: regression, potential models: random forest, mars, boosted_tree
## metric: RMSE

uni_clean %>% 
  filter(valuation < 5000000000) %>% 
  ggplot() +
  geom_histogram(aes(x = valuation))

# 2.) Predicting financial stage; could be interesting to see if we can get 
## meaningful predictions given the imbalancedness of the dataset
## we will have to stratify data here
## experiment type: classification, potential models: logit, rf, boosted tree, NN
## metric: precision, recall, f1_meas (accuracy not useful here)

# i know there will be multiples for companies with multiple investors
uni_clean %>%
  group_by(financial_stage) %>%
  summarise(
    number_at_stage = n()
  ) %>% 
  ggplot() +
  geom_col(aes(x = financial_stage, y = number_at_stage))

# 3.) Predicting investor count; NLP will be interesting here, see if certain big_name 
## investors have the ability to bring in other investors. Two types of potential models:
## a.) using a model that has all vars and NLP
## b.) using a model that only has one-hotted investors
## experiment type: regression, potential models: NN or boosted tree
## see which is more predictive using RMSE

uni_clean %>%
  ggplot() +
  geom_histogram(aes(x = investor_count))

# Setting up data for models -----
skimr::skim(uni_clean)

# filling in NAs with 0's (all numerical vars) 
uni_clean <- uni_clean %>% 
  mutate_all(~replace(., is.na(.), 0))


## experiment 1 -----

## preparing data
set.seed(3013)
exp_1_split <- initial_split(uni_clean, strata = valuation)
exp_1_train <- training(exp_1_split)
exp_1_test <- testing(exp_1_split)

exp_1_folds <- vfold_cv(exp_1_train, strata = valuation)

# recipe with no natural language processing
recipe_no_nlp <- recipe(valuation ~ ., data = exp_1_train) %>% 
  step_rm(select_inverstors, company, portfolio_exits) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())
  
# recipe with NLP
# changing to one hot dataframe
uni_clean %>% 
  mutate(invested = 1) %>% 
  pivot_wider(names_from = select_inverstors,
              values_from = invested)

recipe_no_nlp <- recipe(valuation ~ ., data = exp_1_train) %>% 
  step_rm(company, portfolio_exits) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())


  