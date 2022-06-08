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

#Final iterations of recipes, 2 experiments, each with 
#An iteration containing NLP and one Not

#Only two for this because the second experiment is classification
#and will be completed in the logistic regression section
load("recipes/recipe_nlp_exp_1.rds")
load("recipes/recipe_nlp_exp_3.rds")
load("recipes/recipe_no_nlp_exp_1.rds")
load("recipes/recipe_no_nlp_exp_3.rds")

              #CHANGE#
prep <- prep(recipe_nlp_1, exp_1_train)
bake <- bake(prep, exp_1_train)

sum(is.na(bake))


# linear regression model initialisation
lin_reg_model <- linear_reg(mode = "regression", 
                            penalty = tune(),
                            mixture = tune()) %>%
  set_engine("lm")

# set up workflow
lin_reg_wf <-
  workflow() %>% 
  add_model(lin_reg_model) %>%
              #CHANGE#
  add_recipe(recipe_nlp_1)

# create grid
lin_reg_params <- hardhat::extract_parameter_set_dials(lin_reg_model)

lin_reg_grid <- grid_regular(rf_params, levels = 3)

control <- control_resamples(save_pred = TRUE)

# tuning lin reg

lin_reg_tuned <- lin_reg_wf %>% 
  tune_grid(exp_1_folds, grid = lin_reg_grid)

lin_reg_wf_tuned <- lin_reg_wf %>%
  finalize_workflow(select_best(lin_reg_tuned))

  #CHANGE#
lin_reg_results_nlp_1 <- fit(lin_reg_wf_tuned, exp_1_train)

#Save each object
          #CHANGE#                                  #CHANGE#
save(lin_reg_results_nlp_1, file = 'fit models/lin_reg_tuned_nlp_1.rda')


#1 no X
#1 NLP
#3 No X
#3 NLP X
