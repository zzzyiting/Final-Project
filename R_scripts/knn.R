library(tidymodels)

load('data/processed/train_data.Rdata')

train_data[['ChurnYes']] = factor(train_data[['ChurnYes']])

mobile_split <- initial_split(train_data, prop = 0.9, strata = ChurnYes)
mobile_training <- mobile_split %>% training()
mobile_test <- mobile_split %>% testing()


set.seed(1)
mobile_folds <- vfold_cv(mobile_training, v = 10)



mobile_recipe <- recipe(ChurnYes ~ ., data = train_data) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

mobile_recipe %>% 
  prep(training = train_data) %>% 
  bake(new_data = NULL)

knn_model <- nearest_neighbor(neighbors = tune()) %>% 
  set_engine('kknn') %>% 
  set_mode('classification')

knn_wf <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(mobile_recipe)

k_grid <- tibble(neighbors = c(180, 190, 200, 210, 220, 230))

set.seed(1)

knn_tuning <- knn_wf %>% 
  tune_grid(resamples = mobile_folds,
            grid = k_grid)

best_k <- knn_tuning %>% 
  select_best(metric = 'roc_auc')

final_knn_wf <- knn_wf %>% 
  finalize_workflow(best_k)

knn_fit <- final_knn_wf %>% 
  last_fit(split = mobile_split)

knn_results <-  knn_fit %>% 
  collect_predictions()

roc_curve(knn_results, 
          truth = ChurnYes, 
          estimate = .pred_0) %>% 
  autoplot()


load('data/processed/test_data.Rdata')
test_data[['ChurnYes']] = factor(test_data[['ChurnYes']])
best_knn_model <- nearest_neighbor(neighbors = best_k$neighbors) %>% 
  set_engine('kknn') %>% 
  set_mode('classification') %>%
  fit(ChurnYes ~ ., data=test_data)
best_knn_model %>% predict(test_data) %>% 
  bind_cols(test_data) %>% 
  metrics(truth = ChurnYes, estimate = .pred_class)
