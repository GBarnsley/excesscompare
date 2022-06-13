#function to get the CV folds, very non-optimal, rewrite if time
get_folds <- function(ids, n = 10){
  #split evenly by type and iso3c
  group_assignments <- stringr::str_remove_all(ids, "[0-9]")
  groups <- unique(group_assignments)
  #get the number of replicates
  replicates <- unique(stringr::str_remove_all(ids, "[A-Za-z_]"))
  fold_indexes <- round(seq(0, length(replicates), length.out = n + 1)) %>%
    diff()
  fold_indexes <- map(seq_along(fold_indexes),~rep(.x, fold_indexes[.x])) %>%
    unlist()
  fold_assignments <- map(groups, function(group){
    #randonly arrange replicates
    new_reps <- sample(replicates, length(replicates))
    map(seq_len(n), ~paste0(group,new_reps[fold_indexes==.x]))
  })
  #reformat into complete groups
  fold_assignments <- transpose(fold_assignments) %>%
    map(~unlist(.x))
  #now convert into numeric assignments
  map_int(ids, function(id){ detect_index(fold_assignments, ~id %in% .x) })
}


#function to generate layers
write_model <- function(input_parameters, input_deaths,
                        layers, activation){
  output <- input_parameters
  for(n in layers){
    output <- output %>%
      layer_dense(n, activation)
  }
  #handle deaths
  output_deaths <- input_deaths %>%
    #ISSUE
    layer_flatten() %>%
    layer_dense(1)
  #final estimation layer #ISSUE
  keras_model(list(input_parameters, input_deaths),
              layer_concatenate(list(output, output_deaths)) %>%
                layer_dense(1, "sigmoid"))
}

#function to fit a keras model
fit_keras_model <- function(model, x, y){
  #copy model
  output_model <- model %>%
    clone_model() %>%
    #compile
    compile(
      loss = "mean_squared_error",
      optimizer = "adam",
      metrics = "mse"
    )
  valid_model <- model %>%
    clone_model() %>%
    #compile
    compile(
      loss = "mean_squared_error",
      optimizer = "adam",
      metrics = "mse"
    )
  #train valid for 200 epochs until stopping condition met
  history <- valid_model %>%
    fit(
      x = x,
      y = y,
      epochs = 200,
      callbacks = list(
        callback_early_stopping(
          "mse",
          patience = 5,
          mode = "min"
        )
      ),
      validation.split = 0.3,
      view_metrics = FALSE,
      verbose = 0,
      shuffle = FALSE
    )
  #find the optimal number of epochs, based upon the out of fold error
  epoch <- as_tibble(history) %>%
    filter(!is.na(value) & metric == "mse") %>%
    filter(value == min(value)) %>%
    pull(epoch)
  #fit the output model and then return it
  silent <- output_model %>%
    fit(
      x = x,
      y = y,
      epochs = epoch,
      validation.split = 0,
      view_metrics = FALSE,
      verbose = 0,
      shuffle = FALSE
    )
  output_model
}

#function to train on a set of folds and produce predictions
cv_for_super_learner <- function(fold, folds, deaths_array, parameter_matrix, target_deaths_averted, candidates){
  #define test/train
  train_ids <- names(folds)[folds != fold]
  train_deaths_array <- deaths_array[train_ids,,]
  train_parameter_matrix <- parameter_matrix[train_ids,]
  train_deaths_averted <- target_deaths_averted[train_ids]
  test_ids <- names(folds)[folds == fold]
  test_deaths_array <- deaths_array[test_ids,,]
  test_parameter_matrix <- parameter_matrix[test_ids,]
  test_deaths_averted <- target_deaths_averted[test_ids]
  #now train each model
  trained_models <- map(candidates, function(candidate){
    fit_keras_model(candidate, list(train_parameter_matrix, train_deaths_array), train_deaths_averted)
  })
  names(trained_models) <- paste0("candidate_", seq_along(trained_models))
  #generate predictions on the test data
  prediction <- map_dfc(trained_models, ~predict(.x, list(test_parameter_matrix, test_deaths_array))[,1]) %>%
    #add the true values
    mutate(
      y = test_deaths_averted
    )
  prediction
}
