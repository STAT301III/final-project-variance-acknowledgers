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
library(xgboost)
tidymodels_prefer()

# load data and recipe
load(file = "data/processed/exp_1_train.rda")
load(file = "data/processed/exp_1_test.rda")
load(file = "data/processed/exp_1_folds.rda")

#Final iterations of recipes, three experiments, each with 
#An iteration containing NLP and one Not
load("recipes/recipe_nlp_exp_1.rds")
load("recipes/recipe_nlp_exp_2.rds")
load("recipes/recipe_nlp_exp_3.rds")
load("recipes/recipe_no_nlp_exp_1.rds")
load("recipes/recipe_no_nlp_exp_2.rds")
load("recipes/recipe_no_nlp_exp_3.rds")

              #CHANGE#
prep <- prep(recipe_no_nlp_2, exp_1_train)
bake <- bake(prep, exp_1_train)

sum(is.na(bake))


# boosted tree model
boost_model <- 
  boost_tree(mode = "classification", 
             mtry = tune(),
             min_n = tune()) %>%
  set_engine("xgboost")

# set up workflow
boost_wf <-
  workflow() %>% 
  add_model(boost_model) %>%
              #CHANGE#
  add_recipe(recipe_no_nlp_2)

# create grid
boost_params <- hardhat::extract_parameter_set_dials(boost_model) %>% 
  update(mtry = mtry(c(1, 340))) 

boost_grid <- grid_regular(boost_params, levels = 3)

control <- control_resamples(save_pred = TRUE)

# tuning boost

boost_tuned <- boost_wf %>% 
  tune_grid(exp_1_folds, grid = boost_grid)

boost_wf_tuned <- boost_wf %>%
  finalize_workflow(select_best(boost_tuned))

#CHANGE#
boost_results_no_nlp_2 <- fit(boost_wf_tuned, exp_1_train)

#Save each object
      #CHANGE#                      #CHANGE#
save(boost_results_no_nlp_2, file = 'fit models/boost_tuned_no_nlp_2.rda')

#1 No X
#1 NLP X
#2 No X
#2 NLP X
#3 No X
#3 NLP X


