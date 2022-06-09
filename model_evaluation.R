library(yardstick)

#Load in all the datasets and all the fitted models

#Data
load(file = "data/processed/exp_1_train.rda")
load(file = "data/processed/exp_1_test.rda")
load(file = "data/processed/exp_1_folds.rda")

#Models
load(file = "fit models/boost_tuned_nlp_1.rda")
load(file = "fit models/boost_tuned_nlp_2.rda")
load(file = "fit models/boost_tuned_nlp_3.rda")
load(file = "fit models/boost_tuned_no_nlp_1.rda")
load(file = "fit models/boost_tuned_no_nlp_2.rda")
load(file = "fit models/boost_tuned_no_nlp_3.rda")
load(file = "fit models/rf_tuned_nlp_1.rda")
load(file = "fit models/rf_tuned_nlp_2.rda")
load(file = "fit models/rf_tuned_nlp_3.rda")
load(file = "fit models/rf_tuned_no_nlp_1.rda")
load(file = "fit models/rf_tuned_no_nlp_2.rda")
load(file = "fit models/rf_tuned_no_nlp_3.rda")
load(file = "fit models/lin_reg_tuned_nlp_1.rda")
load(file = "fit models/lin_reg_tuned_nlp_3.rda")
load(file = "fit models/lin_reg_tuned_no_nlp_1.rda")
load(file = "fit models/lin_reg_tuned_no_nlp_3.rda")

#Let's go experiment by experiment:
#Experiment 1: 

###Boost w/ NLP
yardstick::rmse(bind_cols(exp_1_test$valuation, predict(boost_results_nlp_1, new_data = exp_1_test)) %>% 
          rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$valuation, predict(boost_results_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: $3,472,410,017
# MAPE: 83.7%

###Boost w/o NLP
yardstick::rmse(bind_cols(exp_1_test$valuation, predict(boost_results_no_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$valuation, predict(boost_results_no_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: $3,614,239,982
# MAPE: 97.2%

###RF w/ NLP
yardstick::rmse(bind_cols(exp_1_test$valuation, predict(rf_results_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$valuation, predict(rf_results_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: $3,209,664,368
# MAPE: 57.8%

###RF w/o NLP
yardstick::rmse(bind_cols(exp_1_test$valuation, predict(rf_results_no_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$valuation, predict(rf_results_no_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: $3,747,265,148
# MAPE: 76.2%

###Linear Regression w/ NLP
yardstick::rmse(bind_cols(exp_1_test$valuation, predict(lin_reg_results_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$valuation, predict(lin_reg_results_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: $5,390,591,068
# MAPE: 193%

###Linear Regression w/o NLP
yardstick::rmse(bind_cols(exp_1_test$valuation, predict(lin_reg_results_no_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$valuation, predict(lin_reg_results_no_nlp_1, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: $4,413,169,368
# MAPE: 160%

#############################################################################################################
bind_cols(exp_1_test$financial_stage, predict(boost_results_nlp_2, new_data = exp_1_test))
#Experiment 2:
###Boost w/ NLP
yardstick::accuracy(bind_cols(factor(exp_1_test$financial_stage, levels = c('Acq', 'Acquired', 'Asset', 'Corporate', 'Divestiture', 'IPO', 'Management', 'None', 'Reverse')), predict(boost_results_nlp_2, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred_class"), truth, predicted)

yardstick::f_meas(bind_cols(factor(exp_1_test$financial_stage, levels = c('Acq', 'Acquired', 'Asset', 'Corporate', 'Divestiture', 'IPO', 'Management', 'None', 'Reverse')), predict(boost_results_nlp_2, new_data = exp_1_test)) %>% 
                      rename("truth" = "...1") %>% rename("predicted" = ".pred_class"), truth, predicted)
# Accuracy: 95.2%
# f_meas: 97.5%

###Boost w/o NLP
yardstick::accuracy(bind_cols(factor(exp_1_test$financial_stage, levels = c('Acq', 'Acquired', 'Asset', 'Corporate', 'Divestiture', 'IPO', 'Management', 'None', 'Reverse')),
                              predict(boost_results_no_nlp_2, new_data = exp_1_test)) %>% 
                      rename("truth" = "...1") %>% rename("predicted" = ".pred_class"), truth, predicted)

yardstick::f_meas(bind_cols(factor(exp_1_test$financial_stage, levels = c('Acq', 'Acquired', 'Asset', 'Corporate', 'Divestiture', 'IPO', 'Management', 'None', 'Reverse')),
                            predict(boost_results_no_nlp_2, new_data = exp_1_test)) %>% 
                    rename("truth" = "...1") %>% rename("predicted" = ".pred_class"), truth, predicted)
# Accuracy: 97.1%
# f_meas: 48.6%

###RF w/ NLP
yardstick::accuracy(bind_cols(factor(exp_1_test$financial_stage, levels = c('Acq', 'Acquired', 'Asset', 'Corporate', 'Divestiture', 'IPO', 'Management', 'None', 'Reverse')),
                              predict(rf_results_nlp_2, new_data = exp_1_test)) %>% 
                      rename("truth" = "...1") %>% rename("predicted" = ".pred_class"), truth, predicted)

yardstick::f_meas(bind_cols(factor(exp_1_test$financial_stage, levels = c('Acq', 'Acquired', 'Asset', 'Corporate', 'Divestiture', 'IPO', 'Management', 'None', 'Reverse')),
                            predict(rf_results_nlp_2, new_data = exp_1_test)) %>% 
                    rename("truth" = "...1") %>% rename("predicted" = ".pred_class"), truth, predicted)
# Accuracy: 95.2%
# f_meas: 97.5%

###RF w/o NLP
yardstick::accuracy(bind_cols(factor(exp_1_test$financial_stage, levels = c('Acq', 'Acquired', 'Asset', 'Corporate', 'Divestiture', 'IPO', 'Management', 'None', 'Reverse')),
                              predict(rf_results_no_nlp_2, new_data = exp_1_test)) %>% 
                      rename("truth" = "...1") %>% rename("predicted" = ".pred_class"), truth, predicted)

yardstick::f_meas(bind_cols(factor(exp_1_test$financial_stage, levels = c('Acq', 'Acquired', 'Asset', 'Corporate', 'Divestiture', 'IPO', 'Management', 'None', 'Reverse')),
                            predict(rf_results_no_nlp_2, new_data = exp_1_test)) %>% 
                    rename("truth" = "...1") %>% rename("predicted" = ".pred_class"), truth, predicted)
# Accuracy: 97.1%
# f_meas: 55.6%


#############################################################################################################


#Experiment 3: 

###Boost w/ NLP
yardstick::rmse(bind_cols(exp_1_test$investor_count, predict(boost_results_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$investor_count, predict(boost_results_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: 8.53
# MAPE: 67.4%

###Boost w/o NLP
yardstick::rmse(bind_cols(exp_1_test$investor_count, predict(boost_results_no_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$investor_count, predict(boost_results_no_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: 8.69
# MAPE: 68.2%

###RF w/ NLP
yardstick::rmse(bind_cols(exp_1_test$investor_count, predict(rf_results_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$investor_count, predict(rf_results_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: 8.92
# MAPE: 66.6%

###RF w/o NLP
yardstick::rmse(bind_cols(exp_1_test$investor_count, predict(rf_results_no_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$investor_count, predict(rf_results_no_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: 8.84
# MAPE: 65.3%

###Linear Regression w/ NLP
yardstick::rmse(bind_cols(exp_1_test$investor_count, predict(lin_reg_results_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$investor_count, predict(lin_reg_results_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: 9.52
# MAPE: 81.2%

###Linear Regression w/o NLP
yardstick::rmse(bind_cols(exp_1_test$investor_count, predict(lin_reg_results_no_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)

yardstick::mape(bind_cols(exp_1_test$investor_count, predict(lin_reg_results_no_nlp_3, new_data = exp_1_test)) %>% 
                  rename("truth" = "...1") %>% rename("predicted" = ".pred"), truth, predicted)
# RMSE: 9.48
# MAPE: 77.6%


###########################################################################################


#Now let's put that data into tables and export them for usage in the final report. 

exp_1_table <- tibble("Model Type" = c("Random Forest", "Random Forest", "xgboost", "xgboost", "Linear Regression", "Linear Regression"),
                      "Uses NLP?" = c("Yes", "No", "Yes", "No", "Yes", "No"),
                      "Root Mean Squared Error" = c("$3,209,664,368", "$3,747,265,148", "$3,472,410,017",
                                                    "$3,614,239,982", "$5,390,591,068", "$4,413,169,368"),
                      "Mean Absolute Percent Error" = c("57.8%", "76.2%", "83.7%", "97.2%", "193%", "160%"))

exp_2_table <- tibble("Model Type" = c("Random Forest", "Random Forest", "xgboost", "xgboost"),
                      "Uses NLP?" = c("Yes", "No", "Yes", "No"),
                      "Accuracy (misleading)" = c("95.2%", "97.1%", "95.2%", "97.1%"),
                      "F measure" = c("97.5%", "55.6%", "97.5%", "48.6%"))

exp_3_table <- tibble("Model Type" = c("Random Forest", "Random Forest", "xgboost", "xgboost", "Linear Regression", "Linear Regression"),
                                      "Uses NLP?" = c("Yes", "No", "Yes", "No", "Yes", "No"),
                                      "Root Mean Squared Error" = c(8.92, 8.84, 8.53, 8.69, 9.52, 9.48),
                                      "Mean Absolute Percent Error" = c("66.6%", "65.3%", "67.4%", "68.2%", "81.2%", "77.6%"))


save(exp_1_table, exp_2_table, exp_3_table, file = 'results/metric_tables.rda')

