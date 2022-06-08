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

prep <- prep(recipe_no_nlp_2, exp_1_train)
bake <- bake(prep, exp_1_train)

sum(is.na(bake))


# random forest model
rf_model <- 
  rand_forest(mode = "classification",
              mtry = tune()) %>%
  set_engine("ranger")

# set up workflow
rf_wf <-
  workflow() %>% 
  add_model(rf_model) %>%
  add_recipe(recipe_no_nlp_2)

# create grid
rf_params <- hardhat::extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(c(1, 340))) 

rf_grid <- grid_regular(rf_params, levels = 3)

control <- control_resamples(save_pred = TRUE)

# tuning rf


rf_tuned_no_nlp_2 <- rf_wf %>% 
  tune_grid(exp_1_folds, grid = rf_grid)

#Save each object
save(rf_tuned_no_nlp_2, file = 'fit models/rf_tuned_no_nlp_2.rda')



