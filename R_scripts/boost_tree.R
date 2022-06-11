library(tidymodels)

load('data/processed/train_data.Rdata')

train_data[['ChurnYes']] = factor(train_data[['ChurnYes']])

mobile_split <- initial_split(train_data, prop = 0.9, strata = ChurnYes)
mobile_training <- mobile_split %>% training()
mobile_test <- mobile_split %>% testing()


set.seed(1)
mobile_folds <- vfold_cv(mobile_training, v = 3)



mobile_recipe <- recipe(ChurnYes ~ ., data = train_data) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

mobile_recipe %>% 
  prep(training = train_data) %>% 
  bake(new_data = NULL)

tree_model <-
  boost_tree(trees = tune(),min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

tree_wf <- workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(mobile_recipe)

grid <- grid_regular(trees(), min_n(), levels = 5)

set.seed(1)

tree_tuning <- tree_wf %>% 
  tune_grid(resamples = mobile_folds,
            grid = grid)

best <- tree_tuning %>% 
  select_best(metric = 'roc_auc')

final_tree_wf <- tree_wf %>% 
  finalize_workflow(best)

tree_fit <- final_tree_wf %>% 
  last_fit(split = mobile_split)

tree_results <-  tree_fit %>% 
  collect_predictions()

roc_curve(tree_results, 
          truth = ChurnYes, 
          estimate = .pred_0) %>% 
  autoplot()


load('data/processed/test_data.Rdata')
test_data[['ChurnYes']] = factor(test_data[['ChurnYes']])
best_tree_model <- boost_tree(trees = best$trees,min_n = best$min_n) %>%
  set_mode("classification") %>%
  set_engine("xgboost") %>%
  fit(ChurnYes ~ ., data=test_data)
best_tree_model %>% predict(test_data) %>% 
  bind_cols(test_data) %>% 
  metrics(truth = ChurnYes, estimate = .pred_class)
