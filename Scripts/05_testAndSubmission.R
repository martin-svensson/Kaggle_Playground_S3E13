# ====================================================================================================== #
# Description
#
#   Test and submission. 
#   NOTE: Because there is so little data, the validation strategy has changed from using train/test split
#         and CV, to using only CV (04_modelComparison)
#
# Change log:
#   Ver   Date        Comment
#   1.0   13/04/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)

library(tidymodels)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/01_output.RData")
load("./Output/03_output.RData")
load("./Output/04a_tracking.RData")
load("./Output/04_output.RData")
load("./Output/04c_output.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_train <- 
  output_01$df_train %>% 
  rows_append(
    y = output_01$df_train_org
  )

df_train_split <- training(output_01$data_split)

df_train_split %<>% 
  rows_append(
    y = output_01$df_train_org
  )

df_test_split <- testing(output_01$data_split)

best_wflow_id <- "recipe_int_lightgbm"
stack_switch <- TRUE 
# TRUE: Use stacked model from 04c_stacking.R
# FALSE: Use the best workflow
contains_gam_switch <- FALSE

# ---- Submission --------------------------------------------------------------

if (stack_switch) {
  
  # -- Fit the members on the entire training set
  
  if (!contains_gam_switch) {
  
    # If the stack does not contain a GAM model, we can just use 
    model_stack_fit <-
      fit_members(output_04b$model_stack_ens)
    
    model_stack_preds <- 
      model_stack_fit %>% 
      predict(
        output_01$df_test,
        type = "prob"
      )
    
    df_subm <- 
      model_stack_preds %>% 
      bind_cols(
        y = output_01$df_test %>% select(id)
      ) %>% 
      pivot_longer(
        cols = starts_with(".pred"),
        names_to = "prognosis"
      ) %>% 
      mutate(
        prognosis = gsub("\\.pred_", "", x = prognosis)
      ) %>% 
      arrange(
        id, 
        desc(value)
      ) %>% # top 3
      filter(
        row_number() %in% 1:3,
        .by = id
      ) %>% 
      select(!value) %>% 
      mutate(
        name = paste0(".class_", row_number(id)),
        .by = id
      ) %>% 
      pivot_wider(
        id_cols = id,
        values_from = prognosis
      ) %>% 
      mutate(
        prognosis = str_c(.class_1, .class_2, .class_3, sep = " ")
      ) %>% 
      select(id, prognosis)
    
  } else {
    
    # Otherwise, we have to do it manually:
    model_stack_subm_fit <-
      blend_coefs$terms %>% 
      setdiff("(Intercept)") %>% 
      purrr::set_names() %>% 
      map(
        ~ output_04$cv_results %>%
          extract_workflow(.x) %>%
          fit(df_train)
      )
    
    model_stack_subm_pred <- 
      blend_coefs$terms %>% 
      setdiff("(Intercept)") %>% 
      purrr::set_names() %>% 
      map(
        ~ model_stack_subm_fit[[.x]] %>% 
          predict(
            new_data = output_01$df_test,
            type = "prob"
          ) %>% 
          pull(.pred_1)
      )
    
    model_stack_blend <- 
      model_stack_subm_pred %>% 
      as_tibble() %>% 
      mutate(
        "(Intercept)" = 1L
      ) %>% 
      select(
        blend_coefs$terms
      ) %>% 
      mutate( 
        .pred_1 = # see: output_04b$model_stack_ens$equations
          sweep(., 2, blend_coefs$estimate, `*`) %>% 
          rowSums() %>% 
          stats::binomial()$linkinv()
      ) %>% 
      rowwise() %>% 
      mutate(
        .pred_1_simple = mean(c_across(starts_with("recipe"))) # simple average, just for testing
      )
    
    df_subm <- 
      output_01$df_test %>% 
      cbind(model_stack_blend) %>% # swap for gam or ensemble
      select(
        id,
        "target" = .pred_1_simple
      ) 
    
  }
  
} else {
  
  wflow_final_spec <- 
    output_04$cv_results %>% 
    extract_workflow(
      best_wflow_id
    ) 
    # finalize_workflow(
    #   cv_results_best
    # )
  
  wflow_final_fit <- 
    wflow_final_spec %>% 
    fit(df_train)
  
  subm_pred <- 
    wflow_final_fit %>% 
    predict(
      new_data = output_01$df_test,
      type = "prob"
    ) 
  
  df_subm <- 
    subm_pred %>% 
    bind_cols(
      y = output_01$df_test %>% select(id)
    ) %>% 
    pivot_longer(
      cols = starts_with(".pred"),
      names_to = "prognosis"
    ) %>% 
    mutate(
      prognosis = gsub("\\.pred_", "", x = prognosis)
    ) %>% 
    arrange(
      id, 
      desc(value)
    ) %>% # top 3
    filter(
      row_number() %in% 1:3,
      .by = id
    ) %>% 
    select(!value) %>% 
    mutate(
      name = paste0(".class_", row_number(id)),
      .by = id
    ) %>% 
    pivot_wider(
      id_cols = id,
      values_from = prognosis
    ) %>% 
    mutate(
      prognosis = str_c(.class_1, .class_2, .class_3, sep = " ")
    ) %>% 
    select(id, prognosis)
  
}


# ==== EXPORT ------------------------------------------------------------------------------------------ 

df_subm %>% 
  fwrite(file = "./Output/05_submission.csv") 

# -- save tracking data
save(
  df_expTracking,
  file = "./Output/04a_tracking.RData"
)

# ==== UNUSED ------------------------------------------------------------------

# ---- Performance estimate on test set ----------------------------------------

if (stack_switch) {
  
  # -- Fit the members on the entire training set
  
  # If the stack does not contain a GAM model, we can just use 
  model_stack_fit <-
    fit_members(output_04b$model_stack_ens)
  
  # Otherwise we have to do it manually. 
  # Note that cv_results must be a fit_resamples object, not a tuning object (ie. parameters must be tuned before hand in 04_modelComparison)
  # This is because finalizing_workflows does not work well with GAM
  blend_coefs <- 
    stacks:::.get_glmn_coefs(
      output_04b$model_stack_ens[["coefs"]][["fit"]], 
      output_04b$model_stack_ens[["coefs"]][["spec"]][["args"]][["penalty"]]
    ) %>% 
    filter(
      estimate != 0
    ) %>% 
    mutate(
      terms = 
        terms %>% 
        gsub("\\.pred_1_", "", x = .) %>% 
        gsub("_\\d_\\d", "", x = .)
    )
  
  last_fit_preds <-
    blend_coefs$terms %>% 
    setdiff("(Intercept)") %>% 
    purrr::set_names() %>% 
    map(
      ~ output_04$cv_results %>%
        extract_workflow(.x) %>%
        last_fit(
          split = output_01$data_split,
          metrics = output_04$my_metric_set
        ) %>% 
        pull(.predictions) %>% 
        .[[1]] %>% 
        pull(.pred_1)
    )
  
  last_fit_blend_preds <- 
    last_fit_preds %>% 
    as_tibble() %>% 
    mutate(
      "(Intercept)" = 1L
    ) %>% 
    select(
      blend_coefs$terms
    ) %>% 
    mutate( 
      .pred_1 = # see: output_04b$model_stack_ens$equations
        sweep(., 2, blend_coefs$estimate, `*`) %>% 
        rowSums() %>% 
        stats::binomial()$linkinv()
    ) %>% 
    rowwise() %>% 
    mutate(
      .pred_1_simple = mean(c_across(starts_with("recipe"))) # simple average, just for testing
    )
  
} else {
  
  cv_results_best <- 
    output_04$cv_results %>% 
    extract_workflow_set_result(best_wflow_id) %>% 
    select_best(metric = "roc_auc")
  
  performance_est <-
    output_04$cv_results %>%
    extract_workflow(
      best_wflow_id
    ) %>%
    # finalize_workflow( # if cv_results is a tune object, we need to finalize workflows with best hp
    #   output_04$cv_results_best
    # ) %>%
    last_fit(
      split = output_01$data_split,
      metrics = output_04$my_metric_set
    )  
  
}


# --  Performance
best_wflow_estimate <- 
  if (stack_switch) {
    
    roc_auc_vec(
      estimate = 1 - last_fit_blend_preds$.pred_1,
      truth = df_test_split$target
    )
    
  } else {
    
    performance_est %>% 
      collect_metrics()
    
  }


# Predictions
df_test_preds <- 
  if (stack_switch) {
    
    last_fit_preds
    
  } else {
    
    performance_est %>% 
      collect_predictions()
    
  }
# ---- Track experiment --------------------------------------------------------

# Track result for best_wflow_id (the stacked model does not fit in with below structure, so we wont track it for now)

exp_results <- 
  tibble(
    "time" = lubridate::now(),
    "wflow_name" = best_wflow_id,
    "val.or.test" = "Test results"
  )

exp_results$recipe <- 
  best_wflow_id %>% 
  purrr::set_names() %>% 
  map( # map is unnecessary since length(best_wflow_id) == 1, but it returns a list, which is what we want
    ~ extract_preprocessor(
      x = output_04$cv_results,
      id = .x
    ) %>% 
      butcher::butcher() # reduce memory demand
  )

exp_results$model.spec <- 
  best_wflow_id %>% 
  purrr::set_names() %>% 
  map(
    ~ extract_spec_parsnip(
      x = output_04$cv_results,
      id = .x
    )
  )

exp_results$tuned.par <- 
  best_wflow_id %>% 
  purrr::set_names() %>% 
  map(
    ~ output_04$cv_results %>% 
      extract_workflow_set_result(.x) %>% 
      select_best(metric = "rmsle")
  )

exp_results$metrics <- 
  best_wflow_estimate %>% 
  mutate(
    wflow_id = best_wflow_id
  ) %>% 
  select(
    wflow_id,
    .metric,
    .estimate,
    .estimator
  ) %>% list() %>% 
  set_names(best_wflow_id)

# -- Append to tracking data
df_expTracking %<>% 
  dplyr::rows_append(
    y = exp_results
  )

