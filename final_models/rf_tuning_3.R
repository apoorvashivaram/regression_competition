# Random Forest Model 3 tuning ----

# load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)

# set seed ----
set.seed(234)

# load required objects ----
load("model_info/money_setup_v3.rda")

# define model ----
rf_model <- rand_forest(
  mode = "regression", 
  mtry = tune(), 
  min_n = tune()
) %>% 
  # variable importance plot
  set_engine("ranger", importance = "impurity")

# # check tuning parameters
# parameters(rf_model)

# set-up tuning grid ----
rf_params <- parameters(rf_model) %>% 
  # don't want to use all the parameters (# of predictors)
  update(mtry = mtry(range = c(2, 9)))

# define grid ----
rf_grid <- grid_regular(rf_params, levels = 5)

# random forest workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(money_recipe)

# tuning/fitting ----
tic("Random Forest-Model 3")

# tuning code
rf_tune <- rf_workflow %>% 
  tune_grid(
    resamples = money_fold, 
    grid = rf_grid
  )

# calculate runtime info
toc(log = TRUE)

# save runtime info
rf_runtime <- tic.log(format = TRUE)

# write out results and workflow ---
save(rf_tune, rf_workflow, rf_runtime, file = "model_info/rf_tune_v3.rda")