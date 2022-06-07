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

load(file = "recipes/recipe_no_nlp.rds")

prep <- prep(recipe_no_nlp, exp_1_train)
bake <- bake(prep, exp_1_train)

# 367

# random forest model
rf_model <- 
  rand_forest(mode = "regression", 
              mtry = tune(), 
              min_n = tune(),
              trees = tune()) %>%
  set_engine("ranger")

# set up workflow
rf_wf <-
  workflow() %>% 
  add_model(rf_model) %>%
  add_recipe(recipe_no_nlp)

# create grid
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(c(1, 366))) 

rf_grid <- grid_regular(rf_params, levels = 3)

# tuning rf
rf_tuned <- rf_wf %>% 
  tune_grid(exp_1_folds, grid = rf_grid
  )


