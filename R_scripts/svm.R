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

svm_model <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svm_wf <- workflow() %>% 
  add_model(svm_model) %>% 
  add_recipe(mobile_recipe)

grid <- grid_regular(cost(),
                     rbf_sigma(),
                     levels = 5)

set.seed(1)

svm_tuning <- svm_wf %>% 
  tune_grid(resamples = mobile_folds,
            grid = grid)

best <- svm_tuning %>% 
  select_best(metric = 'roc_auc')

final_svm_wf <- svm_wf %>% 
  finalize_workflow(best)

svm_fit <- final_svm_wf %>% 
  last_fit(split = mobile_split)

svm_results <-  svm_fit %>% 
  collect_predictions()

roc_curve(svm_results, 
          truth = ChurnYes, 
          estimate = .pred_0) %>% 
  autoplot()


load('data/processed/test_data.Rdata')
test_data[['ChurnYes']] = factor(test_data[['ChurnYes']])
best_svm_model <- svm_rbf(cost = best$cost, rbf_sigma = best$rbf_sigma) %>%
  set_mode("classification") %>%
  set_engine("kernlab") %>%
  fit(ChurnYes ~ ., data=test_data)
best_svm_model %>% predict(test_data) %>% 
  bind_cols(test_data) %>% 
  metrics(truth = ChurnYes, estimate = .pred_class)
