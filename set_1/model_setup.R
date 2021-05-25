# regression competition ----

# load libraries ----
library(tidyverse)
library(tidymodels)
library(skimr)
library(corrplot)

# Resolve common conflicts
tidymodels_prefer()

# set seed
set.seed(918204)

# load data ----
# training data
money_train <- read_csv("data/train.csv") %>% 
  mutate_at(c("addr_state", "application_type", "emp_length", "grade",
              "home_ownership", "initial_list_status", "purpose", 
              "sub_grade", "term", "verification_status"), as.factor)


# testing data
money_test <- read_csv("data/test.csv")

# short EDA ----
skim_without_charts(money_train)

# explore target variable
money_train %>% 
  ggplot(aes(money_made_inv)) +
  geom_histogram(bins = 50) +
  theme_minimal()

# check for missingness - no missing data
naniar::any_miss(money_train)
naniar::any_miss(money_test)

# corrplot
money_train %>% 
  select(-c(addr_state, earliest_cr_line, emp_title, last_credit_pull_d)) %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.integer) %>%
  cor() %>% 
  corrplot(method = "circle")

# log transform outcome variable?

# resampling via cross-validation ----
money_fold <- vfold_cv(money_train, v = 5, repeats = 3, strata = money_made_inv)

# recipes -----
money_recipe <- recipe(money_made_inv ~ application_type + bc_util + delinq_2yrs + grade +
                         initial_list_status + int_rate + loan_amnt + out_prncp_inv + sub_grade + term,
                      data = money_train) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.1) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_predictors()) %>% 
  step_nzv(all_predictors()) 

# bake the recipes to verify
money_recipe %>% 
  prep(money_train) %>% 
  bake(new_data = NULL)

# save necessary objects for tuning ----
save(money_fold, money_recipe,
     file = "set_1/model_info/money_setup.rda")

# tuning models & separate R scripts ----
# elastic net -- en_tuning.R
# nearest neighbors -- knn_tuning.R
# random forest -- rf_tuning.R
# boosted tree -- bt_tuning.R
# support vector machine (polynomial) -- svmp_tuning.R
# support vector machine (radial basis function) -- svm_rbf_tuning.R
# single Layer Neural Network (multilayer perceptron --- mlp) -- mlp_tuning.R
# multivariate adaptive regression splines (MARS) -- mars_tuning.R

# load tuned files ----

# nearest neighbors
load(file = "set_1/model_info/knn_tune.rda")

# random forest
load(file = "set_1/model_info/rf_tune.rda")

# boosted tree
load(file = "set_1/model_info/bt_tune.rda")

# svm polynomial
load(file = "set_1/model_info/svmp_tune.rda")

# svm radial basis function
load(file = "set_1/model_info/svm_rbf_tune.rda")

# single layer neural network - mlp
load(file = "set_1/model_info/mlp_tune.rda")

# mars
load(file = "set_1/model_info/mars_tune.rda")

# autoplots ----

knn_tune %>% 
  autoplot(metric = "rmse")

rf_tune %>% 
  autoplot(metric = "rmse")

bt_tune %>% 
  autoplot()

svmp_tune %>% 
  autoplot()

svm_rbf_tune %>% 
  autoplot(metric = "rmse")

mlp_tune %>% 
  autoplot(metric = "rmse")

mars_tune %>% 
  autoplot(metric = "rmse")

# select best model
tune_results <- tibble(
  model_type = c("Nearest Neighbors", 
                 "Random Forest",
                 "Boosted Tree",
                 "SVM Polynomial",
                 "SVM RBF",
                 "Neural Network",
                 "MARS"),
  tune_info = list(knn_tune, 
                   rf_tune,
                   bt_tune, 
                   svmp_tune,
                   svm_rbf_tune, 
                   mlp_tune,
                   mars_tune),
  assessment_info = map(tune_info, collect_metrics),
  best_model = map(tune_info, ~ select_best(.x, metric = "rmse"))
)

# select best models
tune_results %>% 
  select(model_type, best_model) %>% 
  unnest(best_model)


# create a table with runtimes
tune_runtime <- tibble(
  run_time = list(
    # en_runtime, 
    knn_runtime, 
    rf_runtime,
    bt_runtime,
    svmp_runtime,
    svm_rbf_runtime, 
    mlp_runtime, 
    mars_runtime),
) %>% 
  unnest_wider(run_time) %>% 
  rename(run_time = "...1") %>% 
  separate(run_time, into = c("model_type", "run_time"), sep = ": ") %>% 
  separate(run_time, into = c("run_time", "sec", "elapsed"), sep = " ") %>% 
  select(-c("sec", "elapsed"))

# combine best model results and run_time table together
tune_results %>% 
  select(model_type, assessment_info) %>% 
  unnest(assessment_info) %>% 
  filter(.metric == "rmse") %>% 
  left_join(tune_runtime, by = c("model_type")) %>% 
  group_by(model_type) %>% 
  arrange(mean) %>% 
  distinct(model_type, .keep_all = T) %>% 
  select(model_type, run_time, .metric, mean, std_err) %>% 
  mutate(mean = round(mean, 4),
         std_err = round(std_err, 4)) %>% 
  rename("model type" = model_type, "run time (sec)" = run_time, metric = .metric, "std error" = std_err) %>% 
  DT::datatable()

# finalize rf_workflow
rf_workflow_tuned <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

# fit to training data
rf_results <- fit(rf_workflow_tuned, money_train)

# variable importance plot on random forest
rf_results %>% 
  pull_workflow_fit() %>% 
  vip::vip() 

# predict on testing data
final_rf_results <- rf_results %>%
  predict(new_data = money_test) %>%
  bind_cols(money_test %>%
              select(id)) %>%
  mutate(Predicted = .pred,
         Id = id) %>%
  select(Id, Predicted)

# check final results
final_rf_results

# # write out file for kaggle submission
# write_csv(final_rf_results, "set_1/kaggle_submission/rf_output.csv")
